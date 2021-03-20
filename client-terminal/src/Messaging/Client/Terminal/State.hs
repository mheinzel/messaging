{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module Messaging.Client.Terminal.State where

import Data.Text (Text)
import Lens.Micro (over, set)
import Lens.Micro.TH (makeLenses)
import qualified Messaging.Client.Core.State as Core
import qualified Messaging.Shared.Conversation as Conv
import qualified System.Console.ANSI.Declarative.Editor as Ansi
import qualified System.Console.ANSI.Declarative.Input as Ansi

data State = State
  { _coreState :: Core.State,
    _editor :: Ansi.Editor,
    _sidebarExpanded :: Bool,
    _unicodeEnabled :: Bool
  }
  deriving (Show)

makeLenses ''State

currentConversationName :: State -> Conv.ConversationName
currentConversationName =
  Core._conversationName . Core._currentConversation . _coreState

handleEditorInput :: Ansi.KeyboardInput -> State -> State
handleEditorInput input = over editor (Ansi.handleInput input)

resetEditor :: State -> State
resetEditor = set editor (Ansi.editor "")

editorContent :: State -> [Text]
editorContent = Ansi.editorContent . _editor

toggleSidebar :: State -> State
toggleSidebar = over sidebarExpanded not

toggleUnicode :: State -> State
toggleUnicode = over unicodeEnabled not

initialState :: State
initialState = State Core.emptyState (Ansi.editor "") True False
