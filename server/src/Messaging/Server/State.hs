{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StrictData #-}

-- | The core types that describe the server-side state.
-- Many of the query and update operations defined in this module evaluate to some value wrapped in
-- the 'Control.Concurrent.STM.STM' monad. These values can be retrieved in an atomic context, for
-- example using the 'Messaging.Server.App.runAtomically' function.
module Messaging.Server.State where

import Control.Concurrent.STM (STM, TVar, modifyTVar, newTVarIO, readTVar, writeTVar)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Messaging.Shared.Conversation (ConversationName)
import Messaging.Shared.User (User (userID), UserID, UserName)
import qualified Network.WebSockets as WS

-- | Holds information about the current server-side state, including conversations and active
-- users. Each field is wrapped in a transactional value ('Control.Concurrent.STM.TVar') to
-- preserve consistency when the state is affected from multiple threads.
data State = State
  { activeConversations :: TVar (Map ConversationName Conversation),
    connectedUsers :: TVar (Map UserID WS.Connection),
    users :: TVar (Map UserID User),
    takenUserNames :: TVar (Set UserName)
  }

-- | A conversation is a simple chat group of users who can send messages to each other.
data Conversation = Conversation
  { conversationName :: ConversationName,
    conversationMembers :: Set UserID
  }
  deriving stock (Show)

initialState :: IO State
initialState =
  State
    <$> newTVarIO mempty
    <*> newTVarIO mempty
    <*> newTVarIO mempty
    <*> newTVarIO mempty

-- Conversation queries -------------------------------------------------------

isMemberOfConversation :: State -> ConversationName -> User -> STM Bool
isMemberOfConversation state convName user =
  Set.member (userID user) <$> getConversationMembers state convName

getConversationMembers :: State -> ConversationName -> STM (Set UserID)
getConversationMembers state convName = do
  convs <- readTVar (activeConversations state)
  pure $ maybe mempty conversationMembers $ Map.lookup convName convs

-- | Not ideal in the long term, as we traverse all existing conversations.
getUserConversations :: State -> User -> STM (Set ConversationName)
getUserConversations state user = do
  convs <- readTVar (activeConversations state)
  pure $ Map.keysSet $ Map.filter (Set.member (userID user) . conversationMembers) convs

-- Conversation updates -------------------------------------------------------

data Change = Changed | NotChanged
  deriving (Eq, Show)

-- | Adds a user to a conversation. If the conversation did not exist yet, it is created. If the
-- user was already part of the conversation, the state doesn't change.
addToConversation ::
  State ->
  User ->
  ConversationName ->
  -- | Indicates whether or not the state has changed.
  STM Change
addToConversation state user convName = do
  convs <- readTVar (activeConversations state)
  case addUser convs of
    Nothing -> pure NotChanged
    Just newConvs -> do
      writeTVar (activeConversations state) newConvs
      pure Changed
  where
    -- Nothing if unchanged.
    addUser ::
      Map ConversationName Conversation ->
      Maybe (Map ConversationName Conversation)
    addUser convs =
      case Map.lookup convName convs of
        Nothing -> do
          let newConv = Conversation convName (Set.singleton $ userID user)
          Just $ Map.insert convName newConv convs
        Just conv ->
          if Set.member (userID user) (conversationMembers conv)
            then do
              Nothing -- nothing to update
            else do
              let newMembers = Set.insert (userID user) (conversationMembers conv)
              let newConv = conv {conversationMembers = newMembers}
              Just $ Map.insert convName newConv convs

-- | Removes a user from a conversation. If the conversation is left with no members, it is
-- removed.
removeFromConversation :: State -> User -> ConversationName -> STM ()
removeFromConversation state user convName = do
  modifyTVar (activeConversations state) $ Map.update removeUser convName
  where
    removeUser :: Conversation -> Maybe Conversation
    removeUser conv =
      let newMembers = Set.delete (userID user) (conversationMembers conv)
       in if Set.null newMembers
            then Nothing
            else Just (conv {conversationMembers = newMembers})
