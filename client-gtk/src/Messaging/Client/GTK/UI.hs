module Messaging.Client.GTK.UI where

import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.Monad (forever)
import Data.Functor (void, ($>))
import GI.Gtk (Window)
import GI.Gtk.Declarative.App.Simple (App (..), Transition (Exit, Transition), run)
import Lens.Micro ((&))
import qualified Messaging.Client.Core.State as Core
import Messaging.Client.GTK.View (Event (..))
import qualified Messaging.Client.GTK.View as View
import qualified Messaging.Shared.Request as Req
import qualified Messaging.Shared.Response as Res

runUI :: Chan Res.Response -> Chan Req.Request -> IO ()
runUI incomingChan outgoingChan = do
  eventChan <- mapChan Inbound incomingChan
  void $ run $ app eventChan outgoingChan

-- Verify if this is desirable
mapChan :: (a -> b) -> Chan a -> IO (Chan b)
mapChan f a = do
  b <- newChan
  _ <- forkIO $ forever $ readChan a >>= writeChan b . f
  return b

app :: Chan Event -> Chan Req.Request -> App Window Core.State Event
app eventChan outgoingChan =
  App
    { view = View.view,
      update = updateState outgoingChan,
      inputs = eventChan,
      initialState = Core.emptyState
    }

updateState :: Chan Req.Request -> Core.State -> Event -> Transition Core.State Event
updateState _ st (Inbound res) =
  Transition (st & Core.handleServerResponse res) (pure Nothing)
--updateState _ st (Stick b) =
--  Transition (st & historySticky .~ b) (pure Nothing)
updateState out st (Outbound req) =
  Transition st $ writeChan out req $> Nothing
updateState _ st Ignore = Transition st (pure Nothing)
updateState _ _ Closed = Exit
