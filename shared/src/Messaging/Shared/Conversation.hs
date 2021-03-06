{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Messaging.Shared.Conversation where

import qualified Data.Aeson as Aeson
import Data.Text (Text)
import GHC.Generics (Generic)

newtype ConversationName = ConversationName {conversationNameText :: Text}
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Aeson.ToJSON, Aeson.FromJSON)

conversationNameGeneral :: ConversationName
conversationNameGeneral = ConversationName "general"
