{-# LANGUAGE TemplateHaskell #-}

module Messaging.Client.Core.State where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Lens.Micro (Lens', over, to, (%~), (&), (<>~), (^.), _2)
import Lens.Micro.TH (makeLenses)
import qualified Messaging.Shared.Conversation as Conv
import qualified Messaging.Shared.Message as Msg
import qualified Messaging.Shared.Response as Res
import qualified Messaging.Shared.User as User

data State = State
  { _currentConversation :: (Conv.ConversationName, ConversationState),
    _otherConversations :: Map Conv.ConversationName ConversationState
  }
  deriving (Show)

data ConversationState = ConversationState
  { _conversationName :: Conv.ConversationName, -- is this name still necessary, we store it in the map already
    _conversationHistory :: ConversationHistory
    -- _conversationMembers :: Set User.UserName
  }
  deriving (Show)

data ConversationHistory = ConversationHistory
  { _newHistoryEntries :: Vector ConversationHistoryEntry,
    _oldHistoryEntries :: Vector ConversationHistoryEntry
  }
  deriving (Show)

data ConversationHistoryEntry
  = Message User.UserName Text
  | UserJoined User.UserName
  | UserLeft User.UserName
  | NewMessages Int
  deriving (Show)

makeLenses ''State
makeLenses ''ConversationState
makeLenses ''ConversationHistory


getConversation :: Conv.ConversationName -> State -> ConversationState
getConversation name (State (curName, curConv) other)
  | curName == name = curConv
  | otherwise = fromMaybe (newConversation name) $ Map.lookup name other

setConversation :: ConversationState -> State -> State
setConversation conv@(ConversationState name _) st@(State (curName, _) other)
  | curName == name = st {_currentConversation = (curName, conv)}
  | otherwise = st {_otherConversations = Map.insert name conv other}

markRead :: ConversationHistory -> ConversationHistory
markRead history = ConversationHistory mempty (history ^. oldHistoryEntries <> history ^. newHistoryEntries)

getHistoryEntries :: ConversationHistory -> Vector ConversationHistoryEntry
getHistoryEntries ConversationHistory {_oldHistoryEntries = old, _newHistoryEntries = new}
  | Vector.null new = old
  | otherwise = old <> Vector.singleton (NewMessages $ Vector.length new) <> new

addHistoryEntry :: ConversationHistoryEntry -> ConversationHistory -> ConversationHistory
addHistoryEntry entry ConversationHistory {_oldHistoryEntries = old, _newHistoryEntries = new}
  | Vector.null new = ConversationHistory (Vector.snoc old entry) new
  | otherwise = ConversationHistory old (Vector.snoc old entry)

currentHistory :: State -> Vector ConversationHistoryEntry
currentHistory st = getHistoryEntries $ st ^. currentConversation . _2 . conversationHistory

emptyState :: State
emptyState = State (Conv.conversationNameGeneral, newConversation Conv.conversationNameGeneral) Map.empty

newConversation :: Conv.ConversationName -> ConversationState
newConversation name = ConversationState name $ ConversationHistory Vector.empty Vector.empty

-- Handling here could be improved by considering 'who' left and joined when who is the user of the client 
handleServerResponse :: Res.Response -> State -> State
handleServerResponse res st = case res of
  (Res.ReceivedMessage user (Msg.Message convName msg)) ->
    updateState convName $ Message (User.userName user) msg
  (Res.JoinedConversation user convName) ->
    updateState convName $ UserJoined (User.userName user)
  (Res.LeftConversation user convName) ->
    updateState convName $ UserLeft (User.userName user)
  where
    updateState convName entry = setConversation (updateConv convName entry) st
    updateConv name newEntry = getConversation name st & conversationHistory %~ addHistoryEntry newEntry
