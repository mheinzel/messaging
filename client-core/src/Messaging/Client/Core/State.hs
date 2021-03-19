{-# LANGUAGE TemplateHaskell #-}

module Messaging.Client.Core.State where

import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Lens.Micro (Lens', over)
import Lens.Micro.TH (makeLenses)
import qualified Messaging.Shared.Conversation as Conv
import qualified Messaging.Shared.Message as Msg
import qualified Messaging.Shared.Response as Res
import qualified Messaging.Shared.User as User

newtype State = State
  { -- | Will turn into some collection of conversations soon.
    _currentConversation :: ConversationState
  }
  deriving (Show)

data ConversationState = ConversationState
  { _conversationName :: Conv.ConversationName,
    _conversationHistory :: ConversationHistory
    -- _conversationMembers :: Set User.UserName
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
emptyState =
  State $ ConversationState Conv.conversationNameGeneral $ ConversationHistory Vector.empty

handleServerResponse :: Res.Response -> State -> State
handleServerResponse (Res.ReceivedMessage user msg) =
  over currentHistory (`Vector.snoc` Message (User.userName user) (Msg.messageContent msg))
handleServerResponse (Res.JoinedConversation user _) =
  over currentHistory (`Vector.snoc` (UserJoined $ User.userName user))
handleServerResponse (Res.LeftConversation user _) =
  over currentHistory (`Vector.snoc` (UserLeft $ User.userName user))
