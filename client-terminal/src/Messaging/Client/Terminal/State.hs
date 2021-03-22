{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module Messaging.Client.Terminal.State where

import Data.Text (Text)
import Lens.Micro (over, set)
import Lens.Micro.TH (makeLenses)
import qualified Messaging.Client.Core.State as Core
import qualified Messaging.Shared.Conversation as Conv
import qualified System.Console.ANSI.Declarative.Input as Ansi
import qualified System.Console.ANSI.Declarative.Widget as Widget

data State = State
  { _inputs :: [Ansi.KeyboardInput]
  }
  deriving (Show)

makeLenses ''State

addInput :: Ansi.KeyboardInput -> State -> State
addInput i = over inputs (i :)

initialState :: State
initialState = State []
