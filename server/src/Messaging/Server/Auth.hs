{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Module header
module Messaging.Server.Auth
  ( Auth.Error (..),
    authenticate,
    freeUserName,
  )
where

import Control.Concurrent.STM (atomically, modifyTVar, readTVar)
import Control.Monad.Reader.Class (asks)
import Control.Monad.Trans (liftIO)
import qualified Data.Set as S (delete, insert, member)
import qualified Data.UUID.V4 as UUID
import Messaging.Server.App (App)
import qualified Messaging.Server.State as State
import Messaging.Shared.Auth as Auth
import Messaging.Shared.User (User (User), UserID (UserID), UserName)
import qualified Network.WebSockets as WS

-- | Reads the request header for the requested username and then tries to mark that username as
-- taken. Evaluates to a Right of the resulting user if this was successful. Otherwise evaluates to
-- a Left of an error describing why it was not successful.
authenticate :: WS.RequestHead -> App (Either Auth.Error User)
authenticate req =
  case Auth.parsePath (WS.requestPath req) of
    Left err -> pure (Left err)
    Right name ->
      claimUserName name >>= \case
        AlreadyTaken -> do
          pure $ Left Auth.UserNameTaken
        SuccessfullyClaimed -> do
          userID <- UserID <$> liftIO UUID.nextRandom
          pure $ Right (User userID name)

data ClaimResult = SuccessfullyClaimed | AlreadyTaken

claimUserName :: UserName -> App ClaimResult
claimUserName name = do
  takenNames <- asks State.takenUserNames
  liftIO . atomically $ do
    names <- readTVar takenNames
    if S.member name names
      then return AlreadyTaken
      else do
        modifyTVar takenNames (S.insert name)
        return SuccessfullyClaimed

-- | Marks a taken username as available again. This has no effect if the username was not taken.
freeUserName :: UserName -> App ()
freeUserName name = do
  takenNames <- asks State.takenUserNames
  liftIO . atomically $ do
    modifyTVar takenNames $ S.delete name
