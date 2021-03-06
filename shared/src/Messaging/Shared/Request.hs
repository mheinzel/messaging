{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StrictData #-}

module Messaging.Shared.Request
  ( Request (..),
    DeserializeError (..),
    deserialize,
    serialize,
  )
where

import qualified Data.Aeson as Aeson
import Data.Bifunctor (first)
import Data.ByteString.Lazy (ByteString)
import GHC.Generics (Generic)
import Messaging.Shared.Message (Message)

data Request = SendMessage Message
  deriving stock (Show, Generic)
  deriving anyclass (Aeson.ToJSON, Aeson.FromJSON)

data DeserializeError = DeserializeError
  { invalidInput :: ByteString,
    errorMessage :: String
  }
  deriving stock (Show)

serialize :: Request -> ByteString
serialize = Aeson.encode

deserialize :: ByteString -> Either DeserializeError Request
deserialize input = first (DeserializeError input) $ Aeson.eitherDecode input
