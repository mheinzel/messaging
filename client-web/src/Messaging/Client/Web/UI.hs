module Messaging.Client.Web.UI where

import Messaging.Client.Web.State (Model, initialModel)
import Messaging.Client.Web.Update (Action (NoOp), updateModel)
import Messaging.Client.Web.View (viewModel)
import qualified Miso

app :: Miso.App Model Action
app =
  Miso.App
    { Miso.initialAction = NoOp,
      Miso.model = initialModel,
      Miso.events = Miso.defaultEvents,
      Miso.subs = [],
      Miso.update = flip updateModel,
      Miso.view = viewModel,
      Miso.mountPoint = Nothing,
      Miso.logLevel = Miso.Off
    }
