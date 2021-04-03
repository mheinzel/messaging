{-# LANGUAGE TemplateHaskell #-}

module Messaging.Client.Core.State where

import Control.Lens (over)
import Control.Lens.At (at)
import Control.Lens.TH (makeLenses)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Messaging.Shared.Conversation as Conv
import qualified Messaging.Shared.Message as Msg
import qualified Messaging.Shared.Response as Res
import qualified Messaging.Shared.User as User

data State = State
  { _joinedConversations :: Map Conv.ConversationName ConversationState
  }
  deriving (Show)

data ConversationState = ConversationState
  { _conversationName :: Conv.ConversationName,
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

history :: Conv.ConversationName -> State -> Maybe (Vector ConversationHistoryEntry)
history name = fmap (_historyEntries . _conversationHistory) . conversationState name

conversationState :: Conv.ConversationName -> State -> Maybe ConversationState
conversationState name = Map.lookup name . _joinedConversations

-- | Modifies the conversation state of the conversation with the given name,
-- or adds the conversation if it was not present yet and then performs the modification.
modifyConvState ::
  Conv.ConversationName ->
  (ConversationState -> ConversationState) ->
  State ->
  State
modifyConvState convName f =
  over
    (joinedConversations . at convName)
    (Just . f . fromMaybe (emptyConversation convName))

-- Modifies the conversation history in a conversation with a given name, or
-- does nothing if that conversation hasn't been joined
addHistoryEntry ::
  ConversationHistoryEntry ->
  ConversationState ->
  ConversationState
addHistoryEntry entry =
  over (conversationHistory . historyEntries) (`Vector.snoc` entry)

emptyConversation :: Conv.ConversationName -> ConversationState
emptyConversation convName = ConversationState convName $ ConversationHistory Vector.empty

emptyState :: State
emptyState = State mempty

addConversation :: Conv.ConversationName -> State -> State
addConversation name =
  over
    joinedConversations
    (Map.insertWith (\_new old -> old) name $ emptyConversation name)

removeConversation :: Conv.ConversationName -> State -> State
removeConversation name = over joinedConversations (Map.delete name)

hasJoined :: Conv.ConversationName -> State -> Bool
hasJoined name = Map.member name . _joinedConversations

handleServerResponse :: Res.Response -> State -> State
handleServerResponse (Res.ReceivedMessage user msg) =
  modifyConvState (Msg.messageConversation msg) $
    addHistoryEntry $ Message (User.userName user) (Msg.messageContent msg)
handleServerResponse (Res.JoinedConversation user convName) =
  modifyConvState convName $
    addHistoryEntry $ UserJoined $ User.userName user
handleServerResponse (Res.LeftConversation user convName) =
  modifyConvState convName $
    addHistoryEntry (UserLeft $ User.userName user)
