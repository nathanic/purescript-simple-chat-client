module Main where

-- check out https://github.com/parsonsmatt/purs-architecture-tutorial/

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Aff (Aff(), runAff)
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE())
import Control.Monad.Eff.Exception (throwException)
import Control.Monad.Free (foldFree, liftF, Free())

import Data.Array (snoc)
import Data.Either (Either(..))
import Data.Foldable (foldMap)
import Data.Foreign.Class (readProp)
import Data.Functor (($>))
import Data.Maybe (Maybe(..))
import Data.String (joinWith)
import Data.Tuple (Tuple(..))

import Halogen
import Halogen.Util (appendToBody, onLoad)
import Halogen.HTML.Indexed as H
import Halogen.HTML.Events.Indexed as E
import Halogen.HTML.Properties.Indexed as P

import Network.HTTP.Affjax (AJAX(), post)


-- | The state of the component.
type State = { messages :: Array Message
             , buffer :: String
             , connected :: Boolean
             , user :: User
             }

initialState :: State
initialState = { messages: []
               , buffer: ""
               , connected: false
               , user: "Joe Bob"
               }

type Message = { from :: User
               , content :: String
               -- , timeStamp :: hmm, so how do we do time in purescript?
               }
type User = String

-- | The component query algebra.
data Query a
  = ReceivedMessage User String a
  | SendMessage String a
  | SetBuffer String a
  | Connect a
  | Disconnect a

-- | Output algebra, which the component sends to the effectful stuff at the bottom
data OutputF a = Log String a
               | SendToServer String a
type Output = Free OutputF

output :: String -> Output Unit
output msg = liftF (Log msg unit)

send :: String -> Output Unit
send msg = liftF (SendToServer msg unit)

-- | The effects used in the app.
type AppEffects eff = 
        HalogenEffects (
              -- ajax :: AJAX , 
              console :: CONSOLE 
            | eff
            )

unlines :: Array String -> String
unlines = joinWith "\n"

-- | The definition for the app's main UI component.
ui :: Component State Query Output
ui = component render eval
  where
    render :: State -> ComponentHTML Query
    render st =
        H.div_ $
            [ H.h1_
                [ H.text "Silly Chat Thing" ]
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
                    [ P.disabled (not st.connected)
                    , E.onClick (E.input_ (SendMessage st.buffer))
                    ]
                    [ H.text "Send" ]
                ]
            ]

    eval :: Natural Query (ComponentDSL State Query Output)
    eval (Connect next) = do
        modify _ { connected = true }
        liftH $ output "got a connection!"
        pure next
    eval (Disconnect next) = do
        modify _ { connected = false }
        liftH $ output "lost the connection."
        pure next
    eval (ReceivedMessage from content next) = do
        modify \st -> st { messages = st.messages `snoc` {from: from, content: content}}
        pure next
    eval (SendMessage content next) = do
        modify \st -> st { messages = st.messages `snoc` {from: st.user, content: content}
                         , buffer = ""
                         }
        liftH $ send content
        pure next
    eval (SetBuffer content next) = do
        modify _ { buffer = content }
        pure next

ui' :: forall eff. Component State Query (Aff (AppEffects eff))
ui' = interpret (foldFree evalOutput) ui
  where
  evalOutput :: Natural OutputF (Aff (AppEffects eff))
  evalOutput (Log msg next) = do
    log msg
    pure next
  evalOutput (SendToServer msg next) = do
    log $ "sending to server: " <> msg
    pure next

-- | Run the app.
main :: Eff (AppEffects ()) Unit
main = runAff throwException (const (pure unit)) $ do
  log "starting the app"
  app <- runUI ui' initialState
  log "setting the node to get appended onLoad"
  onLoad $ appendToBody app.node

  -- connect websocket
  -- use the driver to send in events
  -- but how will the app send events out?
  -- what about that "interpret" example?
  -- https://github.com/aphorisme/haskell-socketson
  log "sending some fake driver messages"
  app.driver $ Connect unit
  app.driver $ ReceivedMessage "Nobody" "What's up, doc?" unit
  log "all done in main"

