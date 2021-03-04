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
import Messaging.Server.App (App, ConnectedUser (ConnectedUser, userConnection), State (connectedUsers))
import Messaging.Shared (UserName)
import qualified Network.WebSockets as WS

addConnection :: UserName -> WS.Connection -> App ()
addConnection userName conn = do
  let user = ConnectedUser userName conn
  users <- asks connectedUsers
  liftIO $ atomically $ modifyTVar users (Map.insert userName user)

removeConnection :: UserName -> App ()
removeConnection userName = do
  users <- asks connectedUsers
  liftIO $ atomically $ modifyTVar users (Map.delete userName)

isConnected :: UserName -> App Bool
isConnected userName = do
  users <- asks connectedUsers
  fmap (isJust . Map.lookup userName) $ liftIO $ readTVarIO users

newtype MissingConnections = MissingConnections {missingConnectionUsers :: [UserName]}

deliver :: [UserName] -> Text -> App MissingConnections
deliver users msg = do
  (missing, conns) <- lookupConnections users
  -- Could be done concurrently.
  liftIO $ traverse_ (flip WS.sendTextData msg) conns
  pure missing

lookupConnections :: [UserName] -> App (MissingConnections, [WS.Connection])
lookupConnections users = do
  conns <- asks connectedUsers
  connectionMap <- liftIO $ readTVarIO conns
  let (missing, found) = partitionEithers . flip map users $ \u ->
        case Map.lookup u connectionMap of
          Just c -> Right (userConnection c)
          Nothing -> Left u
  pure (MissingConnections missing, found)
