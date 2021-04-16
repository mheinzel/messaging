{-# LANGUAGE TemplateHaskell #-}

-- | The core types that describe the user and conversation state of the client.
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

-- | Holds information about the client's user and the state of joined conversations.
data State = State
  { _currentUser :: User.UserName,
    _joinedConversations :: Map Conv.ConversationName ConversationState
  }
  deriving (Eq, Show)

-- | Default client state with no conversations.
emptyState :: User.UserName -> State
emptyState user = State user mempty

-- | The name and history entries of a conversation.
data ConversationState = ConversationState
  { _conversationName :: Conv.ConversationName,
    _conversationHistory :: ConversationHistory
  }
  deriving (Eq, Show)

-- | Default conversation state with no message entries.
emptyConversation :: Conv.ConversationName -> ConversationState
emptyConversation convName = ConversationState convName $ ConversationHistory Vector.empty

newtype ConversationHistory = ConversationHistory
  { _historyEntries :: Vector ConversationHistoryEntry
  }
  deriving (Eq, Show)

-- | An entry in the conversation history. Either a regular message or some type of system message.
data ConversationHistoryEntry
  = -- | A regular text message sent by a user.
    Message User.UserName Text
  | -- | A system message saying a user joined the conversation.
    UserJoined User.UserName
  | -- | A system message saying a user left the conversation.
    UserLeft User.UserName
  deriving (Eq, Show)

makeLenses ''State
makeLenses ''ConversationState
makeLenses ''ConversationHistory

history :: Conv.ConversationName -> State -> Maybe (Vector ConversationHistoryEntry)
history name = fmap (_historyEntries . _conversationHistory) . conversationState name

conversationState :: Conv.ConversationName -> State -> Maybe ConversationState
conversationState name = Map.lookup name . _joinedConversations

getAllConversationStates :: State -> Vector (Conv.ConversationName, ConversationState)
getAllConversationStates = Vector.fromList . Map.assocs . _joinedConversations

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
-- does nothing if that conversation hasn't been joined.
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

-- | Appropriately updates the state based on information given by a server message.
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
