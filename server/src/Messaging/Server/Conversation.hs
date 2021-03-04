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
import Data.Text (Text)
import qualified Data.Text.IO as Text (putStrLn)
import Messaging.Server.App (App, Conversation (Conversation, conversationMembers), activeConversations)
import qualified Messaging.Server.Delivery as Delivery
import Messaging.Shared.Conversation (ConversationName (conversationNameText))
import Messaging.Shared.User (User(..), UserID, UserName (userNameText))

addToConversation :: User -> ConversationName -> App ()
addToConversation user convName = do
  convs <- asks activeConversations
  let newConv = Conversation convName (Set.singleton $ userID user)
  liftIO $
    atomically $ do
      -- insert if there, merge if already existing
      modifyTVar convs (Map.insertWith addUser convName newConv)

  -- also announce arrival
  broadcast convName $ userNameText (userName user) <> " JOINED"
  where
    addUser :: Conversation -> Conversation -> Conversation
    addUser new old =
      old {conversationMembers = conversationMembers new <> conversationMembers old}

removeFromConversation :: User -> ConversationName -> App ()
removeFromConversation user convName = do
  -- also announce leaving
  broadcast convName $ userNameText (userName user) <> " LEFT"

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

broadcastMessage :: UserName -> ConversationName -> Text -> App ()
broadcastMessage userName convName msg = do
  broadcast convName $ userNameText userName <> ": " <> msg

broadcast :: ConversationName -> Text -> App ()
broadcast convName msgPart = do
  -- At some point, we want to properly return the conversation, so messages
  -- for different conversations can be displayed separately on the client.
  let msg = conversationNameText convName <> " | " <> msgPart
  -- TODO: introduce proper logging
  liftIO $ Text.putStrLn msg

  users <- getConversationMembers convName
  _missing <- Delivery.deliver users msg
  -- TODO: remove missing users from conversation
  pure ()

getConversationMembers :: ConversationName -> App [UserID]
getConversationMembers convName = do
  convs <- asks activeConversations
  convMap <- liftIO $ readTVarIO convs
  pure $ case Map.lookup convName convMap of
    Nothing -> []
    Just c -> toList (conversationMembers c)
