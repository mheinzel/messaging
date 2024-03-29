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
import Data.Foldable (traverse_)
import Data.Traversable (for)
import qualified System.Console.ANSI as Ansi
import qualified System.Console.ANSI.Declarative.Widget as Widget
import qualified System.Console.Terminal.Size as Term
import qualified System.IO as IO

data App widget state event = App
  { update :: state -> event -> Transition state,
    view :: Widget.Size -> state -> widget,
    -- | Each action will be called in its own thread repeatedly and should
    -- block until an event becomes available.
    -- See 'System.Console.ANSI.Declarative.Simple.Input'.
    events :: [IO event],
    initialState :: state
  }

data Transition state
  = Transition (IO state)
  | Exit

runApp :: Widget.IsWidget widget => App widget state event -> IO state
runApp app = do
  withTerminalState $
    -- Only now, after setting up stdin properly, start reading events/input.
    withEventThreads app $ \eventChan ->
      run eventChan (initialState app)
  where
    run eventChan !state = do
      size <- maximalSize
      Widget.renderToTerminal size (view app size state)
      -- Ideally, we would read multiple events at once here before re-rendering,
      -- but this requires switching to a Chan that allows non-blocking reads.
      -- Otherwise, we get blocked after some previous events and cannot show
      -- them until another one arrives.
      event <- readChan eventChan
      case update app state event of
        Exit -> pure state
        Transition trans -> do
          state' <- trans
          run eventChan state'

maximalSize :: IO Widget.Size
maximalSize = do
  window <- Term.size
  pure $
    Widget.Size
      { Widget.sizeRows = maybe 24 Term.height window,
        Widget.sizeColumns = maybe 72 Term.width window
      }

withTerminalState :: IO a -> IO a
withTerminalState = bracket setup cleanup . const
  where
    setup = do
      echo <- IO.hGetEcho IO.stdin
      buffIn <- IO.hGetBuffering IO.stdin
      buffOut <- IO.hGetBuffering IO.stdout
      IO.hSetEcho IO.stdin False
      IO.hSetBuffering IO.stdin IO.NoBuffering
      IO.hSetBuffering IO.stdout (IO.BlockBuffering Nothing)
      pure (echo, buffIn, buffOut)

    cleanup (echo, buffIn, buffOut) = do
      Ansi.setSGR [Ansi.Reset]
      Ansi.clearScreen
      Ansi.showCursor
      Ansi.setCursorPosition 0 0
      IO.hSetEcho IO.stdin echo
      IO.hSetBuffering IO.stdin buffIn
      IO.hSetBuffering IO.stdout buffOut

-- IDEA: If we used the async package here, we could check whether any of the
-- spawned threads terminated and re-raise the exception in the main thread.
withEventThreads :: App widget state event -> (Chan event -> IO a) -> IO a
withEventThreads app action = bracket setup cleanup (action . fst)
  where
    setup = do
      eventChan <- newChan
      threadIds <- for (events app) $ \getEvent ->
        forkIO . forever $ getEvent >>= writeChan eventChan
      pure (eventChan, threadIds)

    cleanup (_eventChan, threadIds) = do
      traverse_ killThread threadIds
