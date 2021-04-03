{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Messaging.Server where

import Control.Monad (forever)
import Control.Monad.Trans (MonadIO (liftIO))
import Messaging.Server.App (App, Settings (..), initialState, runApp)
import qualified Messaging.Server.Auth as Auth
import qualified Messaging.Server.Conversation as Conv
import qualified Messaging.Server.Delivery as Delivery
import qualified Messaging.Server.Log as Log
import Messaging.Shared.Conversation (conversationNameGeneral)
import qualified Messaging.Shared.Request as Req
import Messaging.Shared.User (User (userID, userName), UserID, UserName (userNameText))
import qualified Network.WebSockets as WS
import qualified UnliftIO
import qualified UnliftIO.Exception as Exc

runServer :: IO ()
runServer = do
  state <- initialState
  -- TODO: read from command line args
  let port = 8080
  let logging = Log.Settings Log.LoggingStderr Log.LevelDebug
  let settings = Settings port logging
  runApp settings state $ do
    Log.info $ "running on port " <> show (serverPort settings)
  WS.runServer "127.0.0.1" port $ \pending -> do
    runApp settings state (app pending)

app :: WS.PendingConnection -> App ()
app pending = do
  withAcceptedConnection pending $ \user conn ->
    -- withPingThread expects an `IO ()`, but we want to run in `App`.
    UnliftIO.withRunInIO $ \runInIO ->
      WS.withPingThread conn 30 (return ()) . runInIO $
        handleConnection user conn

withAcceptedConnection :: WS.PendingConnection -> (User -> WS.Connection -> App ()) -> App ()
withAcceptedConnection pending action =
  Auth.authenticate (WS.pendingRequest pending) >>= \case
    Left err -> do
      Log.info $ "authentication failure: " <> show err
      liftIO $ WS.rejectRequestWith pending (rejection err)
    Right user -> do
      Log.debug $ "authenticated: " <> userNameText (userName user)
      Log.logExceptions $
        Exc.bracket
          (acceptConnection (userID user) pending)
          -- TODO: we shouldn't do a bunch of IO in the cleanup part of 'bracket'.
          -- Catch exceptions manually or use some async work queue?
          (cleanUpConnection user)
          (action user)
  where
    rejection :: Auth.Error -> WS.RejectRequest
    rejection err =
      WS.defaultRejectRequest
        { WS.rejectMessage = case err of
            Auth.MissingUserName -> "No UserName provided"
            Auth.InvalidUserName -> "Invalid Username"
            Auth.UserNameTaken -> "UserName taken"
        }

acceptConnection :: UserID -> WS.PendingConnection -> App WS.Connection
acceptConnection uID pending = do
  conn <- liftIO $ WS.acceptRequest pending
  Delivery.addConnection uID conn
  pure conn

cleanUpConnection :: User -> WS.Connection -> App ()
cleanUpConnection user _conn = do
  Delivery.removeConnection $ userID user

  -- Later the user name won't be tied to the connection anymore and will have
  -- to be cleaned up elsewhere.
  Auth.freeUserName $ userName user

  -- Later, we'll have to fetch a list of conversations the user is part of.
  let convName = conversationNameGeneral
  Conv.removeFromConversation user convName

handleConnection :: User -> WS.Connection -> App ()
handleConnection user conn = do
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
