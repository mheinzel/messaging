{-# LANGUAGE OverloadedStrings #-}

module Messaging.Client.Web.State where

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
  { backendUrl :: MisoString,
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
    socketOpen :: Bool
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
initialChat user = Chat (Core.emptyState user) mempty True

currentConversationName :: Chat -> Maybe Conv.ConversationName
currentConversationName _ =
  Just Conv.conversationNameGeneral

currentConversation :: Chat -> Maybe Core.ConversationState
currentConversation model =
  currentConversationName model >>= flip Core.conversationState (coreState model)
