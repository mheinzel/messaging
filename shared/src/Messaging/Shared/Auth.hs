{-# LANGUAGE OverloadedStrings #-}

module Messaging.Shared.Auth
  ( buildHeaders,
    Error (..),
    parseHeaders,
  )
where

import Control.Monad ((<=<))
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import qualified Messaging.Shared.User as User
import qualified Network.WebSockets as WS

buildHeaders :: User.UserName -> WS.Headers
buildHeaders userName =
  [("UserName", encodeUtf8 (User.userNameText userName))]

data Error
  = MissingUserName
  | InvalidUserName
  | UserNameTaken
  deriving (Show)

parseHeaders :: WS.Headers -> Either Error User.UserName
parseHeaders =
  addError InvalidUserName . User.mkUserName
    <=< replaceError InvalidUserName . decodeUtf8' -- drop error message
    <=< addError MissingUserName . lookup "UserName"

addError :: e -> Maybe a -> Either e a
addError err = maybe (Left err) Right

replaceError :: e2 -> Either e1 a -> Either e2 a
replaceError err = either (const (Left err)) Right
