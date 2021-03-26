{-# LANGUAGE OverloadedStrings #-}

import Data.Foldable (for_)
import qualified Messaging.Shared.Conversation as Conv
import qualified Messaging.Shared.Message as Msg
import qualified Messaging.Shared.User as User
import Server
import Test.HUnit (assertEqual)
import Test.Hspec

-- Simple tests for the "happy path".
-- We should also add some for requests like joining a conversation twice,
-- leaving a conversation the user is not a member of etc.
-- Maybe also a client closing a connection unexpectedly.
main :: IO ()
main = hspec $ do
  describe "server" $ do
    it "delivers messages to sender" $ do
      withServer $ \server -> do
        client <- mkClient "someone" server
        -- user is automatically added to #general
        assertJoinedConversation client $ \user conv -> do
          assertEqual "user name" (User.UserName "someone") user
          assertEqual "conversation" (Conv.ConversationName "general") conv
        -- receive confirmation
        sendMessage client "general" "hello #general"
        assertReceivedMessage client $ \user msg -> do
          assertEqual "user name" (User.UserName "someone") user
          assertEqual "message conversation" (Conv.ConversationName "general") (Msg.messageConversation msg)
          assertEqual "message conversation" "hello #general" (Msg.messageContent msg)

    it "delivers messages to other clients" $ do
      withServer $ \server -> do
        -- user is automatically added to #general
        client1 <- mkClient "user1" server
        assertJoinedConversation client1 $ \user conv -> do
          assertEqual "user name" (User.UserName "user1") user
          assertEqual "conversation" (Conv.ConversationName "general") conv
        -- joining is announced to all members
        client2 <- mkClient "user2" server
        for_ [client1, client2] $ \client ->
          assertJoinedConversation client $ \user conv -> do
            assertEqual "user name" (User.UserName "user2") user
            assertEqual "conversation" (Conv.ConversationName "general") conv
        client3 <- mkClient "user3" server
        for_ [client1, client2, client3] $ \client ->
          assertJoinedConversation client $ \user conv -> do
            assertEqual "user name" (User.UserName "user3") user
            assertEqual "conversation" (Conv.ConversationName "general") conv
        -- messages are delivered to all members
        sendMessage client1 "general" "hello #general"
        for_ [client1, client2, client3] $ \client -> do
          assertReceivedMessage client $ \user msg -> do
            assertEqual "user name" (User.UserName "user1") user
            assertEqual "message conversation" (Conv.ConversationName "general") (Msg.messageConversation msg)
            assertEqual "message conversation" "hello #general" (Msg.messageContent msg)
        -- leaving is announced to all members
        leaveConversation client2 "general"
        for_ [client1, client2, client3] $ \client ->
          assertLeftConversation client $ \user conv -> do
            assertEqual "user name" (User.UserName "user2") user
            assertEqual "conversation" (Conv.ConversationName "general") conv
        -- further messages are not delivered to member that left
        sendMessage client3 "general" "user2 cannot read this"
        for_ [client1, client3] $ \client ->
          assertReceivedMessage client $ \user msg -> do
            assertEqual "user name" (User.UserName "user3") user
            assertEqual "message conversation" (Conv.ConversationName "general") (Msg.messageConversation msg)
            assertEqual "message conversation" "user2 cannot read this" (Msg.messageContent msg)
        assertNoFurtherResponse client2
