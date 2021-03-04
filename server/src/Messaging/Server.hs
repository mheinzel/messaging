{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Messaging.Server where

import qualified Control.Exception.Safe as Exc
import Control.Monad (forever)
import Control.Monad.Trans (MonadIO (liftIO))
import Messaging.Server.App (State, initialState, runApp)
import qualified Messaging.Server.Auth as Auth
import qualified Messaging.Server.Conversation as Conv
import qualified Messaging.Server.Delivery as Delivery
import Messaging.Shared.Conversation (conversationNameGeneral)
import Messaging.Shared.User (User (userID, userName), UserID)
import qualified Network.WebSockets as WS

runServer :: IO ()
runServer = do
  -- TODO: read from command line args
  state <- initialState
  WS.runServer "127.0.0.1" 8080 (app state)

app :: State -> WS.ServerApp
app state pending = do
  withAcceptedConnection state pending $ \user conn ->
    WS.withPingThread conn 30 (return ()) $ do
      handleConnection state user conn

withAcceptedConnection :: State -> WS.PendingConnection -> (User -> WS.Connection -> IO ()) -> IO ()
withAcceptedConnection state pending action =
  runApp state (Auth.authenticate pending) >>= \case
    Left Auth.MissingUserName ->
      WS.rejectRequest pending "No UserName provided"
    Left Auth.InvalidUserName ->
      WS.rejectRequest pending "Invalid Username"
    Left Auth.UserNameTaken ->
      WS.rejectRequest pending "UserName taken"
    Right user ->
      -- for debugging
      flip Exc.withException (print @Exc.SomeException) $
        Exc.bracket
          (acceptConnection state (userID user) pending)
          -- TODO: we shouldn't do a bunch of IO in the cleanup part of 'bracket'.
          -- Catch exceptions manually or use some async work queue?
          (cleanUpConnection state user)
          (action user)

acceptConnection :: State -> UserID -> WS.PendingConnection -> IO WS.Connection
acceptConnection state uID pending = do
  conn <- WS.acceptRequest pending
  runApp state $ Delivery.addConnection uID conn
  pure conn

cleanUpConnection :: State -> User -> WS.Connection -> IO ()
cleanUpConnection state user _conn = runApp state $ do
  Delivery.removeConnection $ userID user

  -- Later, we'll have to fetch a list of conversations the user is part of.
  let convName = conversationNameGeneral
  Conv.removeFromConversation user convName

handleConnection :: State -> User -> WS.Connection -> IO ()
handleConnection state user conn = runApp state $ do
  -- For now just pretend there's only one conversation.
  let convName = conversationNameGeneral
  Conv.addToConversation user convName

  forever $ do
    received <- liftIO $ WS.receiveData conn
    Conv.broadcastMessage (userName user) convName received
