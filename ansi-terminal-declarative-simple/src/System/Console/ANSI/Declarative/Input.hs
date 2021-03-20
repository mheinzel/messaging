{-# LANGUAGE LambdaCase #-}

module System.Console.ANSI.Declarative.Input where

import qualified Data.Char as Char
import qualified System.IO as IO
import System.Timeout (timeout)

data KeyboardInput
  = Printable Char
  | Enter
  | Tab
  | Backspace
  | Delete
  | PageUp
  | PageDown
  | Home
  | End
  | Escape
  | Arrow KeyboardArrow
  deriving (Show)

data KeyboardArrow
  = ArrowUp
  | ArrowDown
  | ArrowLeft
  | ArrowRight
  deriving (Show)

readInput :: IO KeyboardInput
readInput = do
  IO.getChar >>= \case
    '\n' -> pure Enter
    '\t' -> pure Tab
    '\DEL' -> pure Backspace
    '\ESC' -> readEscaped
    c
      | Char.isPrint c -> pure $ Printable c
      | otherwise -> readInput
  where
    readEscaped = do
      timeout 100 {- microseconds -} IO.getChar >>= \case
        Nothing -> pure Escape
        Just '[' -> readEscapedBracket
        Just _ -> readInput

    readEscapedBracket = do
      IO.getChar >>= \case
        'A' -> pure $ Arrow ArrowUp
        'B' -> pure $ Arrow ArrowDown
        'C' -> pure $ Arrow ArrowRight
        'D' -> pure $ Arrow ArrowLeft
        'H' -> pure Home
        'F' -> pure End
        -- there seems to be a tilde in my terminal, but not sure if it always is
        '3' -> Delete <$ skipOptional '~'
        '5' -> PageUp <$ skipOptional '~'
        '6' -> PageDown <$ skipOptional '~'
        _ -> readInput

    skipOptional skipped = do
      timeout 100 {- microseconds -} (IO.hLookAhead IO.stdin) >>= \case
        Just c | c == skipped -> () <$ IO.getChar
        _ -> pure ()
