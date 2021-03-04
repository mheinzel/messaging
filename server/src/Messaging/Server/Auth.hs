{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Module header
module Messaging.Server.Auth
  ( AuthError (..),
    authenticate,
  )
where

import Control.Concurrent.STM (atomically, modifyTVar, readTVar)
import Control.Monad ((<=<))
import Control.Monad.Reader.Class (asks)
import Control.Monad.Trans (liftIO)
import qualified Data.Set as S (insert, member)
import qualified Data.Text.Encoding as Text (decodeUtf8')
import qualified Data.UUID.V4 as U (nextRandom)
import Messaging.Server.App (App, takenUserNames)
import Messaging.Shared.User (User (..), UserID (..), UserName, mkUserName)
import qualified Network.WebSockets as WS

data AuthError
  = MissingUserName
  | InvalidUserName
  | UserNameTaken

authenticate :: WS.PendingConnection -> App (Either AuthError User)
authenticate pending =
  case parseUserName pending of
    Left err -> pure (Left err)
    Right name ->
      claimUserName name >>= \case
        AlreadyTaken -> pure $ Left UserNameTaken
        SuccessfullyClaimed -> liftIO U.nextRandom >>= pure . Right . User name . UserID
  where
    parseUserName =
      addError InvalidUserName . mkUserName
        <=< replaceError InvalidUserName . Text.decodeUtf8' -- drop error message
        <=< addError MissingUserName . lookup "UserName" . WS.requestHeaders . WS.pendingRequest

data ClaimResult = SuccessfullyClaimed | AlreadyTaken

claimUserName :: UserName -> App ClaimResult
claimUserName name = do
  takenNames <- asks takenUserNames
  liftIO $
    atomically $ do
      names <- readTVar takenNames
      if S.member name names
        then return AlreadyTaken
        else do
          modifyTVar takenNames (S.insert name)
          return SuccessfullyClaimed

addError :: e -> Maybe a -> Either e a
addError err = maybe (Left err) Right

replaceError :: e2 -> Either e1 a -> Either e2 a
replaceError err = either (const (Left err)) Right
