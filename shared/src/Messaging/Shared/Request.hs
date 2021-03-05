{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Messaging.Shared.Request where

import qualified Data.Aeson as Aeson
import Data.Bifunctor (first)
import Data.ByteString.Lazy (ByteString)
import GHC.Generics (Generic)
import Messaging.Shared.Request.Message (Message)

data Request = SendMessage Message
  deriving stock (Show, Generic)
  deriving anyclass (Aeson.ToJSON, Aeson.FromJSON)

data DeserializeError = DeserializeError ByteString String
  deriving stock (Show)

serialize :: Request -> ByteString
serialize = Aeson.encode

deserialize :: ByteString -> Either DeserializeError Request
deserialize input = first (DeserializeError input) $ Aeson.eitherDecode input
