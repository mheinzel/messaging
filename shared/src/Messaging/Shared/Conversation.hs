{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Messaging.Shared.Conversation where

import Data.Text (Text)

newtype ConversationName = ConversationName {conversationNameText :: Text}
  deriving stock (Eq, Ord, Show)

conversationNameGeneral :: ConversationName
conversationNameGeneral = ConversationName "general"
