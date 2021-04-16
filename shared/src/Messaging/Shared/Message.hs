{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StrictData #-}

module Messaging.Shared.Message
  ( Message (..),
  )
where

import qualified Data.Aeson as Aeson
import Data.Text (Text)
import GHC.Generics (Generic)
import Messaging.Shared.Conversation (ConversationName)

-- | A text message in a conversation.
data Message = Message
  { messageConversation :: ConversationName,
    messageContent :: Text
  }
  deriving stock (Show, Generic, Eq)
  deriving anyclass (Aeson.ToJSON, Aeson.FromJSON)
