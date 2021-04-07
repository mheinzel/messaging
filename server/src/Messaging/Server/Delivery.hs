{-# LANGUAGE OverloadedStrings #-}

module Messaging.Server.Delivery where

import Control.Concurrent.STM (atomically, modifyTVar, readTVarIO)
import Control.Monad.Reader.Class (asks)
import Control.Monad.Trans (liftIO)
import Data.Either (partitionEithers)
import Data.Foldable (for_, toList)
import qualified Data.Map.Strict as Map
import Messaging.Server.App (App)
import qualified Messaging.Server.State as State
import qualified Messaging.Shared.Response as Res
import Messaging.Shared.User (UserID)
import qualified Network.WebSockets as WS

addConnection :: UserID -> WS.Connection -> App ()
addConnection userID conn = do
  users <- asks State.connectedUsers
  liftIO $ atomically $ modifyTVar users (Map.insert userID conn)

removeConnection :: UserID -> App ()
removeConnection userID = do
  users <- asks State.connectedUsers
  liftIO $ atomically $ modifyTVar users (Map.delete userID)

isConnected :: UserID -> App Bool
isConnected userID = do
  users <- asks State.connectedUsers
  fmap (Map.member userID) $ liftIO $ readTVarIO users

newtype MissingConnections = MissingConnections
  {missingConnectionUsers :: [UserID]}

deliver :: Foldable f => f UserID -> Res.Response -> App MissingConnections
deliver users msg = do
  (missing, conns) <- lookupConnections (toList users)
  -- Could be done concurrently.
  liftIO . for_ conns $ \conn ->
    WS.sendTextData conn (Res.serialize msg)
  pure missing

lookupConnections :: [UserID] -> App (MissingConnections, [WS.Connection])
lookupConnections users = do
  conns <- asks State.connectedUsers
  connectionMap <- liftIO $ readTVarIO conns
  let (missing, found) = partitionEithers . flip map users $ \u ->
        case Map.lookup u connectionMap of
          Just c -> Right c
          Nothing -> Left u
  pure (MissingConnections missing, found)
