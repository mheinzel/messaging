{-# LANGUAGE TemplateHaskell #-}

module Messaging.Client.Core.State where

import Data.Set (Set)
import Data.Text (Text)
import Data.Vector (Vector, empty, singleton)
import Lens.Micro
import Lens.Micro.TH
import Messaging.Shared.Message (messageContent)
import qualified Messaging.Shared.Response as Res
import Messaging.Shared.User (userName)
import qualified Messaging.Shared.User as User

newtype State = State
  { -- | Will turn into some collection of conversations soon.
    _currentConversation :: ConversationState
  }
  deriving (Show)

newtype ConversationState = ConversationState
  { --  _conversationMembers :: Set User.UserName,
    _conversationHistory :: ConversationHistory
  }
  deriving (Show)

newtype ConversationHistory = ConversationHistory
  { _historyEntries :: Vector ConversationHistoryEntry
  }
  deriving (Show)

data ConversationHistoryEntry
  = Message User.UserName Text
  | UserJoined User.UserName
  | UserLeft User.UserName
  deriving (Show)

makeLenses ''State
makeLenses ''ConversationState
makeLenses ''ConversationHistory

currentHistory :: Lens' State (Vector ConversationHistoryEntry)
currentHistory = currentConversation . conversationHistory . historyEntries

emptyState :: State
emptyState = State $ ConversationState $ ConversationHistory empty

handleServerResponse :: Res.Response -> State -> State
handleServerResponse (Res.ReceivedMessage user msg) st =
  st & currentHistory <>~ singleton (Message (userName user) (messageContent msg))
handleServerResponse (Res.JoinedConversation user _) st =
  st & currentHistory <>~ singleton (UserJoined $ userName user)
handleServerResponse (Res.LeftConversation user _) st =
  st & currentHistory <>~ singleton (UserLeft $ userName user)
