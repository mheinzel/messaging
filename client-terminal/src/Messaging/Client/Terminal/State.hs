{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module Messaging.Client.Terminal.State where

import qualified Data.List as List
import qualified Data.Map as Map
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Lens.Micro (over, set, (^.))
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

initialState :: User.UserName -> State
initialState user =
  State
    (Core.emptyState user)
    [Conv.ConversationName "general"] -- fallback
    (Widget.editor "")
    True
    True

-- Get ------------------------------------------------------------------------

currentConversationName :: State -> Maybe Conv.ConversationName
currentConversationName state =
  List.find
    (`Map.member` Core._joinedConversations (_coreState state))
    (_focusedConversations state)

currentConversation :: State -> Maybe Core.ConversationState
currentConversation state =
  currentConversationName state >>= flip Core.conversationState (_coreState state)

data FocusState
  = Focused
  | Unfocused
  deriving (Eq, Show)

isFocused :: Conv.ConversationName -> State -> FocusState
isFocused name state = case currentConversationName state of
  Just convName | convName == name -> Focused
  _ -> Unfocused

editorContent :: State -> [Text]
editorContent = Widget.editorContent . _editor

-- Update ---------------------------------------------------------------------

-- | This can technically grow unbounded, but should be slow enough to not matter.
focusConversation :: Conv.ConversationName -> State -> State
focusConversation name = over focusedConversations (name :)

focusNextConversation :: State -> State
focusNextConversation = focusOffsetConversation 1

focusPreviousConversation :: State -> State
focusPreviousConversation = focusOffsetConversation (-1)

focusOffsetConversation :: Int -> State -> State
focusOffsetConversation offset state =
  maybe id focusConversation (conversationNameAtOffset offset state) state

conversationNameAtOffset :: Int -> State -> Maybe Conv.ConversationName
conversationNameAtOffset offset state =
  case (`List.elemIndex` convs) =<< currentConversationName state of
    Just i -> Just $ cyclicIndex (i + offset) convs -- We know list is non-empty
    Nothing -> listToMaybe convs -- No base for offset, just pick first one
  where
    convs = Map.keys $ state ^. coreState . Core.joinedConversations

    cyclicIndex :: Int -> [a] -> a
    cyclicIndex i xs = xs !! (i `mod` length xs)

handleEditorInput :: Ansi.KeyboardInput -> State -> State
handleEditorInput input = over editor (Widget.handleInput input)

resetEditor :: State -> State
resetEditor = set editor (Widget.editor "")

toggleSidebar :: State -> State
toggleSidebar = over sidebarExpanded not

toggleUnicode :: State -> State
toggleUnicode = over unicodeEnabled not
