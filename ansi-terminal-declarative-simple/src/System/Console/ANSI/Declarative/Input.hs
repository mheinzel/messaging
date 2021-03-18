{-# LANGUAGE LambdaCase #-}

module System.Console.ANSI.Declarative.Input where

import Control.Concurrent (Chan, writeChan)
import Control.Monad (forever)
import qualified Data.Char as Char
import qualified System.IO as IO

-- | This is very limited, we'll expand it as needed.
data KeyboardInput
  = Printable Char
  | Enter
  deriving (Show)

-- | Blocks, so consider running it in the background.
readInputToChan :: (KeyboardInput -> e) -> Chan e -> IO ()
readInputToChan embed chan = forever $ do
  input <- readInput
  writeChan chan $ embed input

readInput :: IO KeyboardInput
readInput = do
  IO.getChar >>= \case
    '\n' -> pure Enter
    c | Char.isPrint c -> pure $ Printable c
    _ -> readInput
