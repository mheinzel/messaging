{-# LANGUAGE OverloadedStrings #-}

module Messaging.Client.Web.State where

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Messaging.Client.Core.State as Core
import qualified Messaging.Shared.Conversation as Conv
import qualified Messaging.Shared.User as User
import Miso.String (MisoString)

data Model
  = LoggingIn Login
  | WaitingForAuth Waiting
  | Chatting Chat
  deriving (Eq, Show)

data Login = Login
  { serverUrl :: MisoString,
    userName :: MisoString,
    errors :: [MisoString]
  }
  deriving (Eq, Show)

newtype Waiting = Waiting
  { validUserName :: User.UserName
  }
  deriving (Eq, Show)

data Chat = Chat
  { coreState :: Core.State,
    editor :: MisoString,
    socketOpen :: Bool,
    -- | Stack of most recently focused conversations. Not all of them
    -- necessarily exist anymore, so before being used they should always be
    -- checked against the conversations in the core 'Core.State'.
    -- The benefit of this is that we can always fall back to the previously
    -- focused conversation if the current one becomes unavailable.
    focusedConversations :: [Conv.ConversationName],
    newConversation :: MisoString
  }
  deriving (Eq, Show)

initialModel :: Model
initialModel = LoggingIn initialLogin

initialLogin :: Login
initialLogin = Login defaultBaseUrl "" []

defaultBaseUrl :: MisoString
defaultBaseUrl = "ws://127.0.0.1:8080/"

initialWaiting :: User.UserName -> Waiting
initialWaiting = Waiting

initialChat :: User.UserName -> Chat
initialChat user =
  Chat
    { coreState = Core.emptyState user,
      editor = "",
      socketOpen = True,
      focusedConversations = [Conv.conversationNameGeneral],
      newConversation = ""
    }

currentConversationName :: Chat -> Maybe Conv.ConversationName
currentConversationName chat =
  List.find
    (`Map.member` Core._joinedConversations (coreState chat))
    (focusedConversations chat)

currentConversation :: Chat -> Maybe Core.ConversationState
currentConversation model =
  currentConversationName model >>= flip Core.conversationState (coreState model)

-- | This can technically grow unbounded, but should be slow enough to not matter.
focusConversation :: Conv.ConversationName -> Chat -> Chat
focusConversation name chat =
  chat {focusedConversations = name : focusedConversations chat}

conversationNames :: Chat -> [Conv.ConversationName]
conversationNames = Map.keys . Core._joinedConversations . coreState
