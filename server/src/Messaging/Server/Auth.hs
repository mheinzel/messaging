{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Messaging.Server.Auth where

import qualified Data.Text.Encoding as Text (decodeUtf8')
import Messaging.Server.App (App)
import qualified Messaging.Server.Delivery as Delivery
import Messaging.Shared (UserName, mkUserName)
import qualified Network.WebSockets as WS
import Control.Monad ((<=<))

data AuthError
  = MissingUserName
  | InvalidUserName
  | UserNameTaken

authenticate :: WS.PendingConnection -> App (Either AuthError UserName)
authenticate pending =
  case parseUserName pending of
    Left err -> pure (Left err)
    Right userName ->
      Delivery.isConnected userName >>= \case
        True -> pure (Left UserNameTaken)
        False -> pure (Right userName)
  where
    parseUserName =
      addError InvalidUserName . mkUserName
        <=< replaceError InvalidUserName . Text.decodeUtf8' -- drop error message
        <=< addError MissingUserName . lookup "UserName" . WS.requestHeaders . WS.pendingRequest

addError :: e -> Maybe a -> Either e a
addError err = maybe (Left err) Right

replaceError :: e2 -> Either e1 a -> Either e2 a
replaceError err = either (const (Left err)) Right
