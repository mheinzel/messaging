{-# LANGUAGE OverloadedStrings #-}

module Messaging.Server.Conversation
  ( addToConversation,
    removeFromConversation,
    broadcastMessage,
  )
where

import Control.Concurrent.STM (atomically, modifyTVar, readTVarIO)
import Control.Monad.Reader.Class (asks)
import Control.Monad.Trans (MonadIO (liftIO))
import Data.Foldable (toList)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Messaging.Server.App (App, Conversation (Conversation, conversationMembers), activeConversations)
import qualified Messaging.Server.Delivery as Delivery
import Messaging.Shared.Conversation (ConversationName)
import Messaging.Shared.Message (Message (..))
import qualified Messaging.Shared.Response as Res
import Messaging.Shared.User (User (userID), UserID)

addToConversation :: User -> ConversationName -> App ()
addToConversation user convName = do
  convs <- asks activeConversations
  let newConv = Conversation convName (Set.singleton $ userID user)
  liftIO $
    atomically $ do
      -- insert if there, merge if already existing
      modifyTVar convs (Map.insertWith addUser convName newConv)

  -- also announce arrival
  broadCastJoined user convName
  where
    addUser :: Conversation -> Conversation -> Conversation
    addUser new old =
      old {conversationMembers = conversationMembers new <> conversationMembers old}

removeFromConversation :: User -> ConversationName -> App ()
removeFromConversation user convName = do
  -- also announce leaving
  broadcastLeft user convName

  convs <- asks activeConversations
  liftIO $
    atomically $ do
      -- remove conversation if now empty
      modifyTVar convs (Map.update removeUser convName)
  where
    removeUser :: Conversation -> Maybe Conversation
    removeUser old =
      let newMembers = Set.delete (userID user) (conversationMembers old)
       in if Set.null newMembers
            then Nothing
            else Just (old {conversationMembers = newMembers})

broadcastMessage :: User -> Message -> App ()
broadcastMessage user msg =
  broadcast (messageConversation msg) $ Res.ReceivedMessage user msg

broadCastJoined :: User -> ConversationName -> App ()
broadCastJoined user conv =
  broadcast conv $ Res.JoinedConversation user conv

broadcastLeft :: User -> ConversationName -> App ()
broadcastLeft user conv =
  broadcast conv $ Res.LeftConversation user conv

broadcast :: ConversationName -> Res.Response -> App ()
broadcast convName response = do
  -- TODO: introduce proper logging
  liftIO $ print response

  users <- getConversationMembers convName
  _missing <- Delivery.deliver users response
  -- TODO: remove missing users from conversation
  pure ()

getConversationMembers :: ConversationName -> App [UserID]
getConversationMembers convName = do
  convs <- asks activeConversations
  convMap <- liftIO $ readTVarIO convs
  pure $ case Map.lookup convName convMap of
    Nothing -> []
    Just c -> toList (conversationMembers c)
