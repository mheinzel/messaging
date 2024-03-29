module System.Console.ANSI.Declarative.Input
  ( -- * Keyboard input
    KeyboardInput (..),
    KeyboardArrow (..),
    readInput,

    -- * Tick
    Tick (..),
    waitForTickMilliseconds,
  )
where

import System.Console.ANSI.Declarative.Input.Keyboard
import System.Console.ANSI.Declarative.Input.Tick
