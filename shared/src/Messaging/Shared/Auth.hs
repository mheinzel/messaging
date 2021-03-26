{-# LANGUAGE OverloadedStrings #-}

module Messaging.Shared.Auth where

import Data.Text.Encoding (encodeUtf8)
import Messaging.Shared.User (UserName, userNameText)
import qualified Network.WebSockets as WS

authenticationHeaders :: UserName -> WS.Headers
authenticationHeaders userName = [("UserName", encodeUtf8 (userNameText userName))]
