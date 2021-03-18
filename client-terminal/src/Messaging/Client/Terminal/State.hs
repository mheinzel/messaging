{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module Messaging.Client.Terminal.State where

import Data.Text (Text)
import qualified Data.Text as Text
import Lens.Micro.TH (makeLenses)
import qualified Messaging.Client.Core.State as Core
import qualified Messaging.Shared.Conversation as Conv
import System.Console.ANSI.Declarative.Input (KeyboardInput (..))

data State = State
  { _inputState :: InputState,
    _coreState :: Core.State
  }
  deriving (Show)

-- TODO: Define some proper Editor widget in ansi-terminal-declarative-simple!
newtype InputState = InputState {_inputStateText :: Text}
  deriving (Show)

makeLenses ''State
makeLenses ''InputState

currentConversationName :: State -> Conv.ConversationName
currentConversationName =
  Core._conversationName . Core._currentConversation . _coreState

handleKeyboardInput :: KeyboardInput -> InputState -> InputState
handleKeyboardInput input (InputState txt) = case input of
  Printable c -> InputState $ Text.snoc txt c
  Enter -> InputState mempty

initialState :: State
initialState = State (InputState mempty) Core.emptyState
