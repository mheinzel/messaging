{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData #-}

module Messaging.Shared.User where

import Control.Monad ((<=<))
import qualified Data.Aeson as Aeson
import qualified Data.Char as Char
import Data.Text (Text)
import qualified Data.Text as Text
import Data.UUID (UUID)
import GHC.Generics (Generic)

newtype UserID = UserID {getID :: UUID}
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (Aeson.ToJSON, Aeson.FromJSON)

-- | Only use the constructor directly if you really know the name is valid.
-- Otherwise, use 'mkUserName'.
newtype UserName = UserName {userNameText :: Text}
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (Aeson.ToJSON)

-- | Manual instance to enforce validation.
instance Aeson.FromJSON UserName where
  parseJSON =
    maybe (fail "invalid UserName") pure . mkUserName
      <=< Aeson.parseJSON

data User = User
  { userID :: UserID,
    userName :: UserName
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Aeson.ToJSON, Aeson.FromJSON)

mkUserName :: Text -> Maybe UserName
mkUserName name
  | isValidUserName name = Just (UserName name)
  | otherwise = Nothing

isValidUserName :: Text -> Bool
isValidUserName name =
  Text.length name >= 3
    && Text.length name <= 24
    && Text.all (\c -> Char.isAlphaNum c || c `elem` ['-', '_']) name
