module System.Console.ANSI.Declarative.Input.Tick where

import Control.Concurrent (threadDelay)

data Tick = Tick
  deriving (Eq, Ord, Show)

waitForTickMilliseconds :: Int -> IO Tick
waitForTickMilliseconds millis = do
  threadDelay (1000 * millis)
  pure Tick
