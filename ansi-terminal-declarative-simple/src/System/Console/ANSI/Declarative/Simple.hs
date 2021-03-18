{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module System.Console.ANSI.Declarative.Simple where

import Control.Concurrent (Chan, readChan)
import qualified System.Console.ANSI as Ansi
import qualified System.Console.ANSI.Declarative.View as View
import qualified System.IO as IO

data App state event = App
  { update :: state -> event -> Transition state,
    view :: state -> View.View,
    inputs :: Chan event,
    initialState :: state
  }

data Transition state
  = Transition (IO state)
  | Exit

runApp :: App state event -> IO state
runApp app = do
  -- TODO: need to reset this afterwards
  IO.hSetBuffering IO.stdin IO.NoBuffering
  IO.hSetEcho IO.stdin False
  Ansi.hideCursor
  run (initialState app)
  where
    run !state = do
      View.render (view app state)
      event <- readChan (inputs app)
      case update app state event of
        Exit -> pure state
        Transition trans -> do
          state' <- trans
          run state'
