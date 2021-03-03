{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Messaging.Shared where

import qualified Data.Char as Char
import Data.Text (Text)
import qualified Data.Text as Text

newtype UserName = UserName {userNameText :: Text}
  deriving stock (Eq, Ord, Show)

mkUserName :: Text -> Maybe UserName
mkUserName name
  | isValidUserName name = Just (UserName name)
  | otherwise = Nothing

isValidUserName :: Text -> Bool
isValidUserName name = Text.length name <= 32 && Text.all Char.isAlphaNum name

newtype ConversationName = ConversationName {conversationNameText :: Text}
  deriving stock (Eq, Ord, Show)

conversationNameGeneral :: ConversationName
conversationNameGeneral = ConversationName "general"
