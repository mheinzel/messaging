{-# LANGUAGE TemplateHaskell #-}

module Messaging.Client.Core.State where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Lens.Micro (at, over)
-- Imported just to get the instance `At (Map k b)`.
import Lens.Micro.GHC ()
import Lens.Micro.TH (makeLenses)
import qualified Messaging.Shared.Conversation as Conv
import qualified Messaging.Shared.Message as Msg
import qualified Messaging.Shared.Response as Res
import qualified Messaging.Shared.User as User

data State = State
  { _currentUser :: User.UserName,
    _joinedConversations :: Map Conv.ConversationName ConversationState
  }
  deriving (Eq, Show)

emptyState :: User.UserName -> State
emptyState user = State user mempty

data ConversationState = ConversationState
  { _conversationName :: Conv.ConversationName,
    _conversationHistory :: ConversationHistory
  }
  deriving (Eq, Show)

emptyConversation :: Conv.ConversationName -> ConversationState
emptyConversation convName = ConversationState convName $ ConversationHistory Vector.empty

newtype ConversationHistory = ConversationHistory
  { _historyEntries :: Vector ConversationHistoryEntry
  }
  deriving (Eq, Show)

data ConversationHistoryEntry
  = Message User.UserName Text
  | UserJoined User.UserName
  | UserLeft User.UserName
  deriving (Eq, Show)

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
handleServerResponse (Res.LeftConversation user convName) = \state ->
  if User.userName user == _currentUser state
    then removeConversation convName state
    else
      modifyConvState
        convName
        (addHistoryEntry (UserLeft $ User.userName user))
        state
