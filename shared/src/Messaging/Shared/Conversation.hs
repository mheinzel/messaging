{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Messaging.Shared.Conversation where

import Control.Monad ((<=<))
import qualified Data.Aeson as Aeson
import qualified Data.Char as Char
import Data.Text (Text)
import qualified Data.Text as Text

-- | Only use the constructor directly if you really know the name is valid.
-- Otherwise, use 'mkConversationName'.
newtype ConversationName = ConversationName {conversationNameText :: Text}
  deriving stock (Eq, Ord, Show)
  deriving newtype (Aeson.ToJSON)

-- | Manual instance to enforce validation.
instance Aeson.FromJSON ConversationName where
  parseJSON =
    maybe (fail "invalid ConversationName") pure . mkConversationName
      <=< Aeson.parseJSON

mkConversationName :: Text -> Maybe ConversationName
mkConversationName name
  | isValidConversationName name = Just (ConversationName name)
  | otherwise = Nothing

isValidConversationName :: Text -> Bool
isValidConversationName name =
  Text.length name >= 3
    && Text.length name <= 24
    && Text.all (\c -> Char.isAlphaNum c || c `elem` ['-', '_']) name

-- | Plays a special role, as the server will add all new users to @#general@
-- automatically.
conversationNameGeneral :: ConversationName
conversationNameGeneral = ConversationName "general"
