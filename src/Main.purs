module Main where

import Prelude

import Control.Monad.Aff (runAff)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Exception (throwException)

import Halogen
import Halogen.Util (appendToBody, onLoad)
import qualified Halogen.HTML.Indexed as H
import qualified Halogen.HTML.Events.Indexed as E

data Query a = ToggleState a
             | Inc a
             | Dec a

type State = { on :: Boolean
             , count :: Int
             }

initialState :: State
initialState = { on: false, count: 0 }

ui :: forall g. (Functor g) => Component State Query g
ui = component render eval
  where

  render :: State -> ComponentHTML Query
  render state =
    H.div_
      [ H.h1_
          [ H.text "Hello world!" ]
      , H.p_
          [ H.text "Why not toggle this button:" ]
      , H.button
          [ E.onClick (E.input_ ToggleState) ]
          [ H.text
              if not state.on
              then "Don't push me"
              else "I said don't push me!"
          ]
      , H.p_
          [ H.button
                [ E.onClick (E.input_ Inc) ]
                [ H.text
                    if state.count < 1
                    then "I've never been pressed!"
                    else "I've been pressed " <> show state.count <> " times."
                ]
          , H.button
                [ E.onClick (E.input_ Dec) ]
                [ H.text "Memory Hole! ¯\\_(ツ)_/¯" ]
          ]
      ]

  eval :: Natural Query (ComponentDSL State Query g)
  eval (ToggleState next) = do
    modify (\state -> state { on = not state.on })
    pure next
  eval (Inc next) = do
    modify (\state -> state { count = state.count + 1 })
    pure next
  eval (Dec next) = do
    modify (\state -> state { count = state.count - 1})
    pure next

main :: Eff (HalogenEffects ()) Unit
main = runAff throwException (const (pure unit)) $ do
  app <- runUI ui initialState
  onLoad $ appendToBody app.node
