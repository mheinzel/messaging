{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module Messaging.Client.Terminal.State where

import qualified Data.List as List
import qualified Data.Map as Map
import Data.Text (Text)
import Lens.Micro (over, set)
import Lens.Micro.TH (makeLenses)
import qualified Messaging.Client.Core.State as Core
import qualified Messaging.Shared.Conversation as Conv
import qualified Messaging.Shared.User as User
import qualified System.Console.ANSI.Declarative.Input as Ansi
import qualified System.Console.ANSI.Declarative.Widget as Widget

data State = State
  { _coreState :: Core.State,
    -- | Stack of most recently focused conversations. Not all of them
    -- necessarily exist anymore, so before being used they should always be
    -- checked against the conversations in the core 'Core.State'.
    -- The benefit of this is that we can always fall back to the previously
    -- focused conversation if the current one becomes unavailable.
    _focusedConversations :: [Conv.ConversationName],
    _editor :: Widget.Editor,
    _sidebarExpanded :: Bool,
    _unicodeEnabled :: Bool
  }
  deriving (Show)

makeLenses ''State

data FocusState
  = Focused
  | Unfocused
  deriving (Eq, Show)

currentConversationName :: State -> Maybe Conv.ConversationName
currentConversationName state =
  List.find
    (`Map.member` Core._joinedConversations (_coreState state))
    (_focusedConversations state)

currentConversation :: State -> Maybe Core.ConversationState
currentConversation state =
  currentConversationName state >>= flip Core.conversationState (_coreState state)

isFocused :: Conv.ConversationName -> State -> FocusState
isFocused name state = case currentConversationName state of
  Just convName | convName == name -> Focused
  _ -> Unfocused

handleEditorInput :: Ansi.KeyboardInput -> State -> State
handleEditorInput input = over editor (Widget.handleInput input)

resetEditor :: State -> State
resetEditor = set editor (Widget.editor "")

editorContent :: State -> [Text]
editorContent = Widget.editorContent . _editor

toggleSidebar :: State -> State
toggleSidebar = over sidebarExpanded not

toggleUnicode :: State -> State
toggleUnicode = over unicodeEnabled not

-- | This can technically grow unbounded, but should be slow enough to not matter.
setConversation :: Conv.ConversationName -> State -> State
setConversation name = over focusedConversations (name :)

initialState :: User.UserName -> State
initialState user =
  State
    (Core.emptyState user)
    [Conv.ConversationName "general"] -- fallback
    (Widget.editor "")
    True
    False
