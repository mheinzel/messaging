{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Messaging.Shared.Conversation where

import qualified Data.Aeson as Aeson
import Data.Text (Text)

newtype ConversationName = ConversationName {conversationNameText :: Text}
  deriving stock (Eq, Ord, Show)
  deriving newtype (Aeson.ToJSON, Aeson.FromJSON)

conversationNameGeneral :: ConversationName
conversationNameGeneral = ConversationName "general"
