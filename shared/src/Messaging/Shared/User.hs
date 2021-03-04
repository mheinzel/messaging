{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StrictData #-}

module Messaging.Shared.User where

import qualified Data.Char as Char
import Data.Text (Text)
import qualified Data.Text as Text
import Data.UUID (UUID (..))

newtype UserID = UserID {getID :: UUID}

newtype UserName = UserName {userNameText :: Text}
  deriving stock (Eq, Ord, Show)

data User = User
  { userName :: UserName,
    identifier :: UserID
  }

mkUserName :: Text -> Maybe UserName
mkUserName name
  | isValidUserName name = Just (UserName name)
  | otherwise = Nothing

isValidUserName :: Text -> Bool
isValidUserName name = Text.length name <= 32 && Text.all Char.isAlphaNum name
