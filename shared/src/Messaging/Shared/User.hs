{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData #-}

module Messaging.Shared.User where

import qualified Data.Aeson as Aeson
import qualified Data.Char as Char
import Data.Text (Text)
import qualified Data.Text as Text
import Data.UUID (UUID)
import GHC.Generics (Generic)

newtype UserID = UserID {getID :: UUID}
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (Aeson.ToJSON, Aeson.FromJSON)

newtype UserName = UserName {userNameText :: Text}
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (Aeson.ToJSON, Aeson.FromJSON)

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
isValidUserName name = Text.length name <= 32 && Text.all Char.isAlphaNum name
