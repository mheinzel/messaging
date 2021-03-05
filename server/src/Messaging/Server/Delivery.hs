{-# LANGUAGE OverloadedStrings #-}

module Messaging.Server.Delivery where

import Control.Concurrent.STM (atomically, modifyTVar, readTVarIO)
import Control.Monad.Reader.Class (asks)
import Control.Monad.Trans (liftIO)
import Data.Either (partitionEithers)
import Data.Foldable (traverse_)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import Data.Text (Text)
import Messaging.Server.App (App, State (connectedUsers))
import Messaging.Shared.User (UserID)
import qualified Network.WebSockets as WS

addConnection :: UserID -> WS.Connection -> App ()
addConnection userID conn = do
  users <- asks connectedUsers
  liftIO $ atomically $ modifyTVar users (Map.insert userID conn)

removeConnection :: UserID -> App ()
removeConnection userID = do
  users <- asks connectedUsers
  liftIO $ atomically $ modifyTVar users (Map.delete userID)

isConnected :: UserID -> App Bool
isConnected userID = do
  users <- asks connectedUsers
  fmap (isJust . Map.lookup userID) $ liftIO $ readTVarIO users

newtype MissingConnections = MissingConnections {missingConnectionUsers :: [UserID]}

deliver :: [UserID] -> Text -> App MissingConnections
deliver users msg = do
  (missing, conns) <- lookupConnections users
  -- Could be done concurrently.
  liftIO $ traverse_ (flip WS.sendTextData msg) conns
  pure missing

lookupConnections :: [UserID] -> App (MissingConnections, [WS.Connection])
lookupConnections users = do
  conns <- asks connectedUsers
  connectionMap <- liftIO $ readTVarIO conns
  let (missing, found) = partitionEithers . flip map users $ \u ->
        case Map.lookup u connectionMap of
          Just c -> Right c
          Nothing -> Left u
  pure (MissingConnections missing, found)