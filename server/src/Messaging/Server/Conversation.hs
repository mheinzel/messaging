{-# LANGUAGE OverloadedStrings #-}

module Messaging.Server.Conversation
  ( addToConversation,
    removeFromConversation,
    removeFromAllConversations,
    broadcastMessage,
  )
where

import Control.Monad (when)
import Data.Foldable (for_)
import Messaging.Server.App (App, runAtomically)
import qualified Messaging.Server.Delivery as Delivery
import qualified Messaging.Server.Log as Log
import qualified Messaging.Server.State as State
import Messaging.Shared.Conversation (ConversationName (conversationNameText))
import Messaging.Shared.Message (Message)
import qualified Messaging.Shared.Message as Message
import qualified Messaging.Shared.Response as Res
import Messaging.Shared.User (User)

-- | Adds a user to a conversation and announces their arrival to other users in that conversation.
addToConversation :: User -> ConversationName -> App ()
addToConversation user convName = do
  change <- runAtomically $ \state -> do
    State.addToConversation state user convName
  when (change == State.Changed) $
    broadCastJoined user convName

-- | Removes a user from a conversation after announcing their departure to other users in that
-- conversation. Because this happens in this order, the removed user still receives confirmation
-- of being removed.
removeFromConversation :: User -> ConversationName -> App ()
removeFromConversation user convName = do
  wasMember <- runAtomically $ \state -> do
    State.isMemberOfConversation state convName user
  when wasMember $
    broadcastLeft user convName

  runAtomically $ \state -> do
    State.removeFromConversation state user convName

-- | Removes a user from all conversations they were in, and then announces their departure to
-- other users in each of those conversations.
removeFromAllConversations :: User -> App ()
removeFromAllConversations user = do
  convs <- runAtomically $ \state ->
    State.getUserConversations state user
  for_ convs $ \convName -> do
    runAtomically $ \state ->
      State.removeFromConversation state user convName
    broadcastLeft user convName

-- broadcasting ---------------------------------------------------------------

-- | Broadcast a message sent by a user to all other users in the conversation that the message was
-- sent in.
broadcastMessage :: User -> Message -> App ()
broadcastMessage user msg =
  broadcast (Message.messageConversation msg) $ Res.ReceivedMessage user msg

broadCastJoined :: User -> ConversationName -> App ()
broadCastJoined user conv =
  broadcast conv $ Res.JoinedConversation user conv

broadcastLeft :: User -> ConversationName -> App ()
broadcastLeft user conv =
  broadcast conv $ Res.LeftConversation user conv

broadcast :: ConversationName -> Res.Response -> App ()
broadcast convName response = do
  Log.debug $ conversationNameText convName <> ": " <> Res.serializeToText response
  users <- runAtomically $ \state ->
    State.getConversationMembers state convName
  _missing <- Delivery.deliver users response
  -- TODO: remove missing users from conversation?
  pure ()
