{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module Messaging.Client.Terminal.State where

import Data.Text (Text)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Map (keys)
import Lens.Micro (over, set, (^.))
import Lens.Micro.TH (makeLenses)
import qualified Messaging.Client.Core.State as Core
import qualified Messaging.Shared.Conversation as Conv
import qualified Messaging.Shared.User as User
import qualified System.Console.ANSI.Declarative.Input as Ansi
import qualified System.Console.ANSI.Declarative.Widget as Widget

data State = State
  { _coreState :: Core.State,
    _currentConversation :: Maybe Conv.ConversationName,
    _editor :: Widget.Editor,
    _sidebarExpanded :: Bool,
    _unicodeEnabled :: Bool
  }
  deriving (Show)

makeLenses ''State

currentConversationName :: State -> Conv.ConversationName
currentConversationName = fromMaybe (Conv.ConversationName "No conversation") . _currentConversation

someConversation :: State -> Maybe Conv.ConversationName
someConversation state = listToMaybe . keys $ state ^. coreState . Core.joinedConversations

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

overJoinedConversations ::
  (Conv.ConversationName -> Core.State -> Core.State) ->
  Conv.ConversationName ->
  State ->
  State
overJoinedConversations f name = over coreState (f name)

setConversation :: Maybe Conv.ConversationName -> State -> State
setConversation Nothing state = set currentConversation Nothing state
setConversation (Just name) state
  | hasJoined name state = set currentConversation (Just name) state
  | otherwise = state

addConversation :: Conv.ConversationName -> State -> State
addConversation = overJoinedConversations Core.addConversation

removeConversation :: Conv.ConversationName -> State -> State
removeConversation = overJoinedConversations Core.removeConversation

hasJoined :: Conv.ConversationName -> State -> Bool
hasJoined name = Core.hasJoined name . _coreState

initialState :: User.UserName -> State
initialState user =
  State
    (Core.emptyState user)
    Nothing
    (Widget.editor "")
    True
    False
