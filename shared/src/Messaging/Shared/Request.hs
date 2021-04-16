{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StrictData #-}

module Messaging.Shared.Request
  ( Request (..),
    DeserializeError (..),
    deserialize,
    serialize,
    serializeToText,
  )
where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Text as Aeson (encodeToLazyText)
import Data.Bifunctor (first)
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import qualified Data.Text.Lazy as Text.Lazy
import GHC.Generics (Generic)
import Messaging.Shared.Conversation (ConversationName)
import Messaging.Shared.Message (Message)

-- | A request to the server.
data Request
  = -- | Send a message.
    SendMessage Message
  | -- | Join a conversation.
    JoinConversation ConversationName
  | -- | Leave a conversation.
    LeaveConversation ConversationName
  deriving stock (Show, Generic)
  deriving anyclass (Aeson.ToJSON, Aeson.FromJSON)

-- | Describes why a deserialization failed.
data DeserializeError = DeserializeError
  { invalidInput :: ByteString,
    errorMessage :: String
  }
  deriving stock (Show)

serialize :: Request -> ByteString
serialize = Aeson.encode

serializeToText :: Request -> Text
serializeToText = Text.Lazy.toStrict . Aeson.encodeToLazyText

deserialize :: ByteString -> Either DeserializeError Request
deserialize input = first (DeserializeError input) $ Aeson.eitherDecode input
