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

-- | Adds a connection with the ID of the corresponding user to the state.
addConnection :: UserID -> WS.Connection -> App ()
addConnection userID conn = do
  users <- asks State.connectedUsers
  liftIO $ atomically $ modifyTVar users (Map.insert userID conn)

-- | Removes the connection corresponding to a user from the state.
removeConnection :: UserID -> App ()
removeConnection userID = do
  users <- asks State.connectedUsers
  liftIO $ atomically $ modifyTVar users (Map.delete userID)

isConnected :: UserID -> App Bool
isConnected userID = do
  users <- asks State.connectedUsers
  fmap (Map.member userID) $ liftIO $ readTVarIO users

-- | A list of IDs of users to which data could not be sent because they are not connected anymore.
newtype MissingConnections = MissingConnections
  {missingConnectionUsers :: [UserID]}

-- | Delivers a response to the users with the given IDs, and returns the 'MissingConnections'.
deliver :: Foldable f => f UserID -> Res.Response -> App MissingConnections
deliver users msg = do
  (missing, conns) <- lookupConnections (toList users)
  -- Could be done concurrently.
  liftIO . for_ conns $ \conn ->
    WS.sendTextData conn (Res.serialize msg)
  pure missing

-- | Looks up the connections corresponding to the users with the given IDs. Returns a tuple of the
-- 'MissingConnections' and the connections that were found.
lookupConnections :: [UserID] -> App (MissingConnections, [WS.Connection])
lookupConnections users = do
  conns <- asks State.connectedUsers
  connectionMap <- liftIO $ readTVarIO conns
  let (missing, found) = partitionEithers . flip map users $ \u ->
        case Map.lookup u connectionMap of
          Just c -> Right c
          Nothing -> Left u
  pure (MissingConnections missing, found)
