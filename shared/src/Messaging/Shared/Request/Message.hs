{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StrictData #-}

module Messaging.Shared.Request.Message where

import qualified Data.Aeson as Aeson
import Data.Text (Text)
import GHC.Generics (Generic)

data Message = Message
  { content :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (Aeson.ToJSON, Aeson.FromJSON)
