{-# LANGUAGE OverloadedStrings #-}

module Messaging.Shared.Auth
  ( buildPath,
    Error (..),
    parsePath,
  )
where

import Control.Monad ((<=<))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Maybe (mapMaybe)
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import qualified Messaging.Shared.User as User

-- | Turn @/path@ and @user@ into @/path?userName=user@.
buildPath :: ByteString -> User.UserName -> ByteString
buildPath basePath userName =
  basePath <> "?userName=" <> encodeUtf8 (User.userNameText userName)

-- | Describes why parsing a path did not result in a valid, available username.
data Error
  = MissingUserName
  | InvalidUserName
  | UserNameTaken
  deriving (Show)

-- | Turn @/path?userName=user@ into @user@.
-- If successful, evaluates to a Right of the username.
-- Otherwise, evaluates to a Left of an error describing why it was not successful.
-- Implementation could be improved, but works for now.
parsePath :: ByteString -> Either Error User.UserName
parsePath =
  addError InvalidUserName . User.mkUserName
    <=< replaceError InvalidUserName . decodeUtf8' -- drop error message
    <=< addError MissingUserName . lookup "userName" . parseQueries
  where
    parseQueries :: ByteString -> [(ByteString, ByteString)]
    parseQueries = mapMaybe parseQuery . BS.split '&' . dropPrefix

    parseQuery :: ByteString -> Maybe (ByteString, ByteString)
    parseQuery bs = case BS.split '=' bs of
      [k, v] -> Just (k, v)
      _ -> Nothing

    dropPrefix = BS.dropWhile (== '?') . BS.dropWhile (/= '?')

addError :: e -> Maybe a -> Either e a
addError err = maybe (Left err) Right

replaceError :: e2 -> Either e1 a -> Either e2 a
replaceError err = either (const (Left err)) Right
