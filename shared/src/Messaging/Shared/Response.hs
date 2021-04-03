{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StrictData #-}

module Messaging.Shared.Response
  ( Response (..),
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
import Messaging.Shared.User (User)

data Response
  = ReceivedMessage User Message
  | JoinedConversation User ConversationName
  | LeftConversation User ConversationName
  deriving stock (Show, Generic, Eq)
  deriving anyclass (Aeson.ToJSON, Aeson.FromJSON)

-- TODO: avoid duplication?
data DeserializeError = DeserializeError
  { invalidInput :: ByteString,
    errorMessage :: String
  }
  deriving stock (Show)

serialize :: Response -> ByteString
serialize = Aeson.encode

serializeToText :: Response -> Text
serializeToText = Text.Lazy.toStrict . Aeson.encodeToLazyText

deserialize :: ByteString -> Either DeserializeError Response
deserialize input = first (DeserializeError input) $ Aeson.eitherDecode input
