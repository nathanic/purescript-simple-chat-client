module Main where

import Prelude

import Control.Alt ((<|>))
import Control.Bind ((=<<))
import Control.Monad (when)
import Control.Monad.Rec.Class (forever)
import Control.Monad.Aff (Aff(), launchAff, runAff)
import Control.Monad.Aff.AVar (AVar(), AVAR(), makeVar, putVar, takeVar)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE(), log)
import Control.Monad.Eff.Exception (EXCEPTION(), throwException)
import Control.Monad.Eff.Console.Unsafe (logAny)
import Control.Monad.Eff.Var (($=))

import Data.Array (snoc)
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..), isJust, isNothing)
import Data.String (joinWith)

import Halogen
import Halogen.HTML.Indexed as H
import Halogen.HTML.Events.Indexed as E
import Halogen.HTML.Properties.Indexed as P
import Halogen.Util (appendToBody, onLoad)

import Network.HTTP.Affjax (AJAX(), post)

import WebSocket

-- | hard-coded because this is just a dang demo
chatServerUrl :: URL
chatServerUrl = URL "ws://localhost:9160"

-- | The state of the component.
type State = { messages :: Array ChatMessage
             , buffer :: String
             , user :: User
             , socket :: Maybe Connection
             -- , connect :: forall eff. URL -> Eff (AppEffects eff) Unit
             , connect :: URL -> Eff (AppEffects ()) Unit
             }

initialState :: State
initialState = { messages: []
               , buffer: ""
               , user: "AnonymousCoward"
               , socket: Nothing
               , connect: const $ log "ERROR: skeevy placeholder for connect was called somehow!"
               }

type ChatMessage = { content :: String
                   -- a real app would probably store other stuff here
                   }
type User = String

-- | The effects used in the app.
type AppEffects eff =
        HalogenEffects (
              -- ajax :: AJAX ,
              console :: CONSOLE,
              ws :: WEBSOCKET
            | eff
            )

-- | The component query algebra.
data Query a
  = ReceivedMessage String a
  | SendMessage String a
  | SetBuffer String a
  | SetUserName String a
  | ConnectButton a
  | Connect Connection a
  | Disconnect a

-- | Didn't seem worth an extra bower import
unlines :: Array String -> String
unlines = joinWith "\n"

-- | The definition for the app's main UI component.
-- ui :: forall eff. Component State Query (Aff (AppEffects eff))
ui :: Component State Query (Aff (AppEffects ()))
ui = component render eval
  where
    render :: State -> ComponentHTML Query
    render st =
        H.div_
            [ H.h1_ [ H.text "Silly Chat Thing" ]
            , H.p_
                [ H.text "Username"
                , H.input
                    [ P.inputType P.InputText
                    , P.value st.user
                    , E.onValueChange (E.input SetUserName)
                    ]
                , H.button
                    [ E.onClick (E.input_ ConnectButton) ]
                    [ H.text if isJust st.socket
                        then "Disconnect"
                        else "Connect"
                    ]
                ]
            , H.p_
                [ H.textarea
                    [ P.value $ unlines $ map _.content st.messages
                    , P.readonly true
                    , P.class_ $ H.className "chatbox"
                    ]
                ]
            , H.p_
                [ H.input
                    [ P.inputType P.InputText
                    , P.class_ $ H.className "sendbuffer"
                    , P.placeholder "Type a message to send"
                    , P.value st.buffer
                    , E.onValueChange (E.input SetBuffer)
                    ]
                , H.button
                    [ P.disabled (isNothing st.socket)
                    , E.onClick (E.input_ (SendMessage st.buffer))
                    ]
                    [ H.text "Send" ]
                ]
            ]

    eval :: Natural Query (ComponentDSL State Query (Aff (AppEffects ())))
    eval (ConnectButton next) = do
        connect <- gets _.connect
        liftEff' $ connect chatServerUrl
        pure next
    eval (Connect conn next) = do
        modify _ { socket = Just conn }
        liftAff' $ log' "got a connection!"
        st <- get
        send' ("Hi! I am " <> st.user) st.socket
        pure next
    eval (Disconnect next) = do
        modify _ { socket = Nothing }
        liftAff' $ log' "lost the connection."
        pure next
    eval (ReceivedMessage content next) = do
        modify \st -> st { messages = st.messages `snoc` {content: content}}
        pure next
    eval (SendMessage content next) = do
        modify _ { buffer = "" }
        gets _.socket >>= send' content
        pure next
    eval (SetBuffer content next) = do
        modify _ { buffer = content }
        pure next
    eval (SetUserName user next) = do
        modify _ { user = user }
        pure next

-- | convencience fn to send a string through the websocket Connection.
-- | this takes a Maybe because, well, that's how it is in State and it's more
-- | convenient to keep the pattern match in one place. 
-- | same goes for the liftEff, and even the argument order (for >>=).
-- send :: String -> Maybe Connection -> Aff (ws :: WEBSOCKET | ()) Unit
send :: String -> Maybe Connection -> Aff (AppEffects ()) Unit
send _ Nothing                    = pure unit
send s (Just (Connection socket)) = liftEff $ socket.send $ Message s

-- | This is an even *more* convenient send helper that is hoisted into the lofty
-- | hights of the Halogen Free Monad!
-- send' :: forall eff. String -> Maybe Connection -> Free (HalogenF State Query (Aff (ws :: WEBSOCKET | eff)) Unit)
send' :: String -> Maybe Connection -> ComponentDSL State Query (Aff (AppEffects ())) Unit
send' s c = liftAff' $ send s c

-- | saves us some typing and lift-spam
log' :: forall eff. String -> Aff (console :: CONSOLE | eff) Unit
log' = liftEff <<< log

-- | Construct a WebSocket Connection and fill it out with the event handlers
-- | we need for this particular app. The event handlers publish Query values
-- | to the AVar argument, so they can be fed to the component's driver function
-- | elsewhere.
-- makeSocket :: forall eff. AVar (Query Unit) -> URL -> Eff (AppEffects eff) Unit
makeSocket :: AVar (Query Unit) -> URL -> Eff (AppEffects ()) Unit
makeSocket chan url = do
    conn@(Connection socket) <- newWebSocket url []

    socket.onopen $= \event -> do
        logAny event
        log "onopen: Connection opened"
        launchAff $ putVar chan $ action $ Connect conn

    socket.onmessage $= \event -> do
        logAny event
        let received = runMessage (runMessageEvent event)
        log $ "onmessage: Received '" ++ received ++ "'"
        launchAff $ putVar chan $ action $ ReceivedMessage received

    socket.onclose $= \event -> do
        logAny event
        log "onclose: Connection closed"
        launchAff $ putVar chan $ action $ Disconnect

    pure unit


-- | Spawn our various async doodaddery and go!
main :: Eff (AppEffects ()) Unit
main = do
    runAff throwException (const (pure unit)) $ do
        chan <- makeVar
        app <- runUI ui (initialState { connect = makeSocket chan })
        onLoad $ appendToBody app.node
        -- forever feed the beast
        forever (takeVar chan >>= app.driver)
