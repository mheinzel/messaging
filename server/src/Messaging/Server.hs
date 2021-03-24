{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Messaging.Server where

import qualified Control.Exception.Safe as Exc
import Control.Monad (forever)
import Control.Monad.Trans (MonadIO (liftIO))
import Messaging.Server.App (State, initialState, runApp)
import qualified Messaging.Server.Auth as Auth
import qualified Messaging.Server.Conversation as Conv
import qualified Messaging.Server.Delivery as Delivery
import Messaging.Shared.Conversation (conversationNameGeneral)
import qualified Messaging.Shared.Request as Req
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
      logExceptions $
        Exc.bracket
          (acceptConnection state (userID user) pending)
          -- TODO: we shouldn't do a bunch of IO in the cleanup part of 'bracket'.
          -- Catch exceptions manually or use some async work queue?
          (cleanUpConnection state user)
          (action user)
  where
    -- for debugging
    logExceptions :: IO a -> IO a
    logExceptions = flip Exc.withException $ \e ->
      putStrLn $ "exception: " <> show (e :: Exc.SomeException)

acceptConnection :: State -> UserID -> WS.PendingConnection -> IO WS.Connection
acceptConnection state uID pending = do
  conn <- WS.acceptRequest pending
  runApp state $ Delivery.addConnection uID conn
  pure conn

cleanUpConnection :: State -> User -> WS.Connection -> IO ()
cleanUpConnection state user _conn = runApp state $ do
  Delivery.removeConnection $ userID user

  -- Later the user name won't be tied to the connection anymore and will have
  -- to be cleaned up elsewhere.
  Auth.freeUserName $ userName user

  Conv.removeFromAllConversations user

handleConnection :: State -> User -> WS.Connection -> IO ()
handleConnection state user conn = runApp state $ do
  -- For now just pretend there's only one conversation.
  let defaultConvName = conversationNameGeneral
  Conv.addToConversation user defaultConvName

  forever $ do
    received <- liftIO $ WS.receiveData conn
    case Req.deserialize received of
      Left err ->
        liftIO $ putStrLn (Req.errorMessage err)
      Right (Req.SendMessage msg) ->
        Conv.broadcastMessage user msg
      Right (Req.JoinConversation convName) ->
        Conv.addToConversation user convName
      Right (Req.LeaveConversation convName) ->
        Conv.removeFromConversation user convName
