module Main where

import Prelude

import Control.Alt ((<|>))
import Control.Bind ((=<<))
import Control.Monad (when)
import Control.Monad.Rec.Class (forever)
import Control.Monad.Aff (Aff(),launchAff,runAff,forkAff)
import Control.Monad.Aff.AVar (AVar(),AVAR(),makeVar,putVar,takeVar)
-- import Control.Monad.Aff.Console (log)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE(),log)
import Control.Monad.Eff.Exception (EXCEPTION(),throwException)
import Control.Monad.Eff.Console.Unsafe (logAny)
import Control.Monad.Eff.Var (($=))
import Control.Monad.Eff.Var as Var
import Control.Monad.Free (foldFree, liftF, Free())

import Data.Array (snoc)
import Data.Either (Either(..))
import Data.Foldable (foldMap)
import Data.Foreign.Class (readProp)
import Data.Functor (($>))
import Data.Maybe (Maybe(..), isJust)
import Data.String (joinWith)
import Data.Tuple (Tuple(..))

import Halogen
import Halogen.Util (appendToBody, onLoad)
import Halogen.HTML.Indexed as H
import Halogen.HTML.Events.Indexed as E
import Halogen.HTML.Properties.Indexed as P

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
               -- XXX skeevy placeholder
               , connect: const (pure unit)
               }

type ChatMessage = { from :: User
                   , content :: String
                   -- , timeStamp :: hmm, so how do we do time in purescript?
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
  = ReceivedMessage User String a
  | SendMessage String a
  | SetBuffer String a
  | SetUserName String a
  | ConnectButton a
  | Connect Connection a
  | Disconnect a


unlines :: Array String -> String
unlines = joinWith "\n"

-- | The definition for the app's main UI component.
-- ui :: forall eff. Component State Query (Aff (AppEffects eff))
ui :: Component State Query (Aff (AppEffects ()))
ui = component render eval
  where
    render :: State -> ComponentHTML Query
    render st =
        H.div_ $
            [ H.h1_
                [ H.text "Silly Chat Thing" ]
            , H.p_
                [ H.text "Username"
                , H.input
                    [ P.inputType P.InputText
                    , P.value st.user
                    , E.onValueChange (E.input SetUserName)
                    ]
                , H.button
                    [ E.onClick (E.input_ ConnectButton) ]
                    [ H.text if isJust st.socket then "Disconnect" else "Connect" ]
                ]
            , H.p_
                [ H.textarea
                    [ P.value $ unlines $ map (\m -> m.from <> ": " <> m.content) st.messages ]
                ]
            , H.p_
                [ H.input
                    [ P.inputType P.InputText
                    , P.placeholder "Type a message to send"
                    , P.value st.buffer
                    , E.onValueChange (E.input SetBuffer)
                    -- , E.onValueInput (E.input SetTopic)
                    ]
                , H.button
                    [ P.disabled (not $ isJust st.socket)
                    , E.onClick (E.input_ (SendMessage st.buffer))
                    ]
                    [ H.text "Send" ]
                ]
            ]

    eval :: Natural Query (ComponentDSL State Query (Aff (AppEffects ())))
    eval (ConnectButton next) = do
        -- crap, how do i get hold of the driver in here?
        -- could send it in with a query, but that feels skeevy
        -- maybe we need to use AVars so we're less tightly coupled?
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
    eval (ReceivedMessage from content next) = do
        modify \st -> st { messages = st.messages `snoc` {from: from, content: content}}
        pure next
    eval (SendMessage content next) = do
        modify \st -> st { -- messages = st.messages `snoc` {from: st.user, content: content} , 
                            buffer = ""
                         }
        gets _.socket >>= send' content
        pure next
    eval (SetBuffer content next) = do
        modify _ { buffer = content }
        pure next
    eval (SetUserName user next) = do
        modify _ { user = user }
        pure next

-- this takes a Maybe because, well, that's how it is in State and it's more
-- convenient to keep the pattern match in one place
-- send :: String -> Maybe Connection -> Aff (ws :: WEBSOCKET | ()) Unit
send :: String -> Maybe Connection -> Aff (AppEffects ()) Unit
send _ Nothing                    = pure unit
send s (Just (Connection socket)) = liftEff $ socket.send $ Message s

-- send' :: forall eff. String -> Maybe Connection -> Free (HalogenF State Query (Aff (ws :: WEBSOCKET | eff)) Unit)
send' :: String -> Maybe Connection -> Free (HalogenF State Query (Aff (AppEffects ()))) Unit
send' s c = liftAff' $ send s c

log' :: forall eff. String -> Aff (console :: CONSOLE | eff) Unit
log' = liftEff <<< log

-- makeSocket :: forall eff. AVar (Query Unit) -> URL -> Eff (AppEffects eff) Unit
makeSocket :: AVar (Query Unit) -> URL -> Eff (AppEffects ()) Unit
makeSocket chan url = do
    conn@(Connection socket) <- newWebSocket url []

    socket.onopen $= \event -> do
        logAny event
        log "onopen: Connection opened"
        -- launchAff $ driver $ action $ Connect conn
        launchAff $ putVar chan $ action $ Connect conn

    socket.onmessage $= \event -> do
        logAny event
        let received = runMessage (runMessageEvent event)
        log $ "onmessage: Received '" ++ received ++ "'"
        -- launchAff $ driver $ action $ ReceivedMessage "Remote" received
        launchAff $ putVar chan $ action $ ReceivedMessage "Remote" received

    socket.onclose $= \event -> do
        logAny event
        log "onclose: Connection closed"
        launchAff $ putVar chan $ action $ Disconnect

    pure unit


-- | Run the app.
main :: Eff (AppEffects ()) Unit
main = do
    runAff throwException (const (pure unit)) $ do
        log' "starting the app"
        chan <- makeVar
        app <- runUI ui (initialState { connect = makeSocket chan })
        log' "setting the node to get appended onLoad"
        onLoad $ appendToBody app.node

        -- li'l async event loop to forward from this socket AVar into the UI component
        -- without either having to directly know about each other
        forkAff $ forever do
            log' "taking from the chan to feed the driver..."
            (takeVar chan >>= app.driver)

        log' "all done in main"

