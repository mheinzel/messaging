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

-- | Also announce arrival.
addToConversation :: User -> ConversationName -> App ()
addToConversation user convName = do
  change <- runAtomically $ \state -> do
    State.addToConversation state user convName
  when (change == State.Changed) $
    broadCastJoined user convName

-- | First announce leaving, *then* remove from conversation
-- (so the user still gets a confirmation of being removed).
removeFromConversation :: User -> ConversationName -> App ()
removeFromConversation user convName = do
  wasMember <- runAtomically $ \state -> do
    State.isMemberOfConversation state convName user
  when wasMember $
    broadcastLeft user convName

  runAtomically $ \state -> do
    State.removeFromConversation state user convName

-- | Only announce after user is removed (assuming user disconnected).
removeFromAllConversations :: User -> App ()
removeFromAllConversations user = do
  convs <- runAtomically $ \state ->
    State.getUserConversations state user
  for_ convs $ \convName -> do
    runAtomically $ \state ->
      State.removeFromConversation state user convName
    broadcastLeft user convName

-- broadcasting ---------------------------------------------------------------

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
