{-# LANGUAGE BangPatterns #-}

module System.Console.ANSI.Declarative.Simple
  ( App (..),
    Transition (..),
    runApp,
  )
where

import Control.Concurrent (Chan, forkIO, killThread, newChan, readChan, writeChan)
import Control.Exception (bracket)
import Control.Monad (forever)
import Data.Foldable (toList, traverse_)
import Data.Traversable (for)
import qualified System.Console.ANSI as Ansi
import qualified System.Console.ANSI.Declarative.Input as Input
import qualified System.Console.ANSI.Declarative.View as View
import qualified System.IO as IO

data App state event = App
  { update :: state -> event -> Transition state,
    view :: state -> View.View,
    useKeyboardInput :: Maybe (Input.KeyboardInput -> Maybe event),
    -- | Each action will be called in its own thread repeatedly and should
    -- block until an event becomes available.
    getAdditionalEvents :: [IO event],
    initialState :: state
  }

data Transition state
  = Transition (IO state)
  | Exit

runApp :: App state event -> IO state
runApp app = do
  withTerminalState $
    -- Only now, after setting up stdin properly, start reading input.
    withEventThreads app $ \eventChan ->
      run eventChan (initialState app)
  where
    run eventChan !state = do
      View.render (view app state)
      event <- readChan eventChan
      case update app state event of
        Exit -> pure state
        Transition trans -> do
          state' <- trans
          run eventChan state'

withTerminalState :: IO a -> IO a
withTerminalState = bracket setup cleanup . const
  where
    setup = do
      echo <- IO.hGetEcho IO.stdin
      buffIn <- IO.hGetBuffering IO.stdin
      buffOut <- IO.hGetBuffering IO.stdout
      IO.hSetEcho IO.stdin False
      IO.hSetBuffering IO.stdin IO.NoBuffering
      IO.hSetBuffering IO.stdout IO.LineBuffering
      pure (echo, buffIn, buffOut)

    cleanup (echo, buffIn, buffOut) = do
      Ansi.setSGR [Ansi.Reset]
      Ansi.clearScreen
      Ansi.showCursor
      Ansi.setCursorPosition 0 0
      IO.hSetEcho IO.stdin echo
      IO.hSetBuffering IO.stdin buffIn
      IO.hSetBuffering IO.stdout buffOut

-- IDEA: If we used the async package here, we coul check whether any of the
-- spawned threads terminated and re-raise the exception in the main thread.
withEventThreads :: App state event -> (Chan event -> IO a) -> IO a
withEventThreads app action = bracket setup cleanup (action . fst)
  where
    setup = do
      eventChan <- newChan
      inputThread <- fmap toList $
        for (useKeyboardInput app) $ \embed -> do
          forkIO . forever $ do
            input <- Input.readInput
            case embed input of
              Just x -> writeChan eventChan x
              Nothing -> pure ()
      otherThreads <- for (getAdditionalEvents app) $ \getEvent ->
        forkIO . forever $ getEvent >>= writeChan eventChan
      pure (eventChan, inputThread <> otherThreads)

    cleanup (_eventChan, threads) = do
      traverse_ killThread threads
