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
import Control.Monad.Eff.Exception (throwException)
import Control.Monad.Eff.Console.Unsafe (logAny)
import Control.Monad.Eff.Var (($=))

import Data.Array (replicate,snoc)
import Data.Function (Fn1(),runFn1)
import Data.Maybe (Maybe(..), isJust, isNothing)
import Data.String (joinWith)

import DOM (DOM())

import Halogen
import Halogen.HTML.Indexed as H
import Halogen.HTML.Events.Indexed as E
import Halogen.HTML.Properties.Indexed as P
import Halogen.Util (appendToBody, onLoad)

import WebSocket

-- | The state of the component.
type State = { messages :: Array ChatMessage
             , buffer :: String
             , user :: User
             , socket :: Maybe Connection
             , chatServerUrl :: String
             , queryChan :: AVar (Query Unit)
             }

type ChatMessage = { content :: String
                   -- a real app would probably store other stuff here
                   }
type User = String

-- | The effects used in the app.
type AppEffects eff = HalogenEffects ( console :: CONSOLE
                                     , ws :: WEBSOCKET
                                     | eff
                                     )

-- | The component query algebra.
data Query a
  = ReceivedMessage String a
  | SendMessage String a
  | SetBuffer String a
  | SetUrl String a
  | SetUserName String a
  | ConnectButton a
  | Connect Connection a
  | Disconnect a

-- for some reason this first def for AppDriver won't typecheck
-- type AppDriver = Driver Query (AppEffects ())
type AppDriver = Query Unit -> Aff (AppEffects ()) Unit

-- | Didn't seem worth an extra bower import
unlines :: Array String -> String
unlines = joinWith "\n"

-- | The definition for the app's main UI component.
-- can't do an open row here or it won't typecheck :-/
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
                , H.input
                    [ P.inputType P.InputText
                    , P.value st.chatServerUrl
                    , E.onValueChange (E.input SetUrl)
                    ]
                , H.button
                    [ E.onClick (E.input_ ConnectButton) ]
                    [ H.text if isJust st.socket
                        then "Disconnect"
                        else "Connect"
                    ]
                ]
            , H.p_
                [ H.pre
                    [ P.class_ $ H.className "chatbox"
                    , P.id_ "chatbox" ]
                    [ H.text $ unlines $ map _.content st.messages ]
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
                , H.button
                    [ P.disabled (isNothing st.socket)
                    , E.onClick (E.input_ (SendMessage (unlines $ replicate 10 st.buffer)))
                    ]
                    [ H.text "Spam!" ]
                ]
            ]

    -- eval :: Natural Query (ComponentDSL State Query (Aff (AppEffects eff)))
    eval :: Natural Query (ComponentDSL State Query (Aff (AppEffects ())))
    eval (ConnectButton next) = do
        driver <- makeAuxDriver <$> get
        url <- URL <$> gets _.chatServerUrl
        liftAff' $ makeSocket driver url
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
        liftEff' $ scrollBottom "chatbox"
        pure next
    eval (SendMessage content next) = do
        modify _ { buffer = "" }
        gets _.socket >>= send' content
        pure next
    eval (SetBuffer content next) = do
        modify _ { buffer = content }
        pure next
    eval (SetUrl content next) = do
        modify _ { chatServerUrl = content }
        pure next
    eval (SetUserName user next) = do
        modify _ { user = user }
        pure next

-- XXX is DOM an appropriate effect here? we do query the DOM and poke an element prop...
foreign import scrollBottomImpl :: forall e. Fn1 String (Eff (dom :: DOM | e) Unit)
scrollBottom :: forall e. String -> Eff (dom :: DOM | e) Unit
scrollBottom = runFn1 scrollBottomImpl

-- | convenience fn to send a string through the websocket Connection.
-- | this takes a Maybe because, well, that's how it is in State and it's more
-- | convenient to keep the pattern match in one place.
-- | same goes for the liftEff, and even the argument order (for >>=).
send :: forall eff. String -> Maybe Connection -> Aff (ws :: WEBSOCKET | eff) Unit
send _ Nothing                    = pure unit
send s (Just (Connection socket)) = liftEff $ socket.send $ Message s

-- | This is an even *more* convenient send helper that is hoisted into the lofty
-- | heights of the Halogen Free Monad!
send' :: forall eff. String -> Maybe Connection -> ComponentDSL State Query (Aff (ws :: WEBSOCKET | eff)) Unit
send' s c = liftAff' $ send s c

-- | saves us some typing and lift-spam
log' :: forall eff. String -> Aff (console :: CONSOLE | eff) Unit
log' = liftEff <<< log

-- | We need the Driver function accessible from within the component eval,
-- | so it can make websockets that can send queries to the component though said driver.
-- | My hacky solution to this is to carry an AVar around in the State that we can publish
-- | Query values into.  Elsewhere an async loop is consuming this var and feeding the real driver.
makeAuxDriver :: forall r. {queryChan :: AVar (Query Unit) | r} -> AppDriver
makeAuxDriver {queryChan=chan} = putVar chan

-- | Construct a WebSocket Connection and fill it out with the event handlers
-- | we need for this particular app. The supplied Driver fn will be used to
-- | publish Query values to the associated Halogen Component
makeSocket :: forall eff. AppDriver -> URL -> Aff (avar :: AVAR, ws :: WEBSOCKET | eff) Unit
makeSocket driver url = do
    liftEff do
        conn@(Connection socket) <- newWebSocket url []

        socket.onopen $= \event -> do
            logAny event
            log "onopen: Connection opened"
            quietLaunchAff $ driver $ action $ Connect conn

        socket.onmessage $= \event -> do
            logAny event
            let received = runMessage (runMessageEvent event)
            log $ "onmessage: Received '" ++ received ++ "'"
            quietLaunchAff $ driver $ action $ ReceivedMessage received

        socket.onclose $= \event -> do
            logAny event
            log "onclose: Connection closed"
            quietLaunchAff $ driver $ action $ Disconnect

    pure unit


quietLaunchAff :: forall eff a. Aff eff a -> Eff eff Unit
quietLaunchAff = runAff (const (pure unit)) (const (pure unit))

-- | Spawn our various async doodaddery and go!
main :: Eff (AppEffects ()) Unit
main = do
    runAff throwException (const (pure unit)) $ do
        chan <- makeVar
        app <- runUI ui { messages: []
                        , buffer: ""
                        , user: "AnonymousCoward"
                        , chatServerUrl: "ws://localhost:9160"
                        , socket: Nothing
                        , queryChan: chan
                        }
        onLoad $ appendToBody app.node
        -- The other part of the "aux driver" hack: shuffle queries from the AVar to the Driver
        forever (takeVar chan >>= app.driver)

