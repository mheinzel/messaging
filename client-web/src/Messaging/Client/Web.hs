module Messaging.Client.Web where

import qualified Messaging.Client.Web.UI as UI
import qualified Miso

runClient :: IO ()
runClient =
  Miso.startApp UI.app
