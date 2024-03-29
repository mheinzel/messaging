module Messaging.Client.GTK.UI where

import Control.Concurrent.Chan (Chan, readChan, writeChan)
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import Data.Functor (void, ($>))
import GI.Gtk (Window)
import GI.Gtk.Declarative.App.Simple (App (..), Transition (Exit, Transition), run)
import Lens.Micro ((%~), (&), (.~))
import qualified Messaging.Client.Core.State as Core
import Messaging.Client.GTK.View (Event (..))
import qualified Messaging.Client.GTK.View as View
import qualified Messaging.Shared.Request as Req
import qualified Messaging.Shared.Response as Res
import qualified Messaging.Shared.User as User
import qualified Pipes

runUI :: User.UserName -> Chan Res.Response -> Chan Req.Request -> IO ()
runUI userName incomingChan outgoingChan = do
  let eventProducer = produceFromChan Inbound incomingChan
  void $ run $ app userName eventProducer outgoingChan

produceFromChan :: (a -> b) -> Chan a -> Pipes.Producer b IO ()
produceFromChan f chan = forever $ do
  x <- liftIO $ readChan chan
  Pipes.yield $ f x

app :: User.UserName -> Pipes.Producer Event IO () -> Chan Req.Request -> App Window View.State Event
app userName eventProducer outgoingChan =
  App
    { view = View.view,
      update = updateState outgoingChan,
      inputs = [eventProducer],
      initialState = View.State (Core.emptyState userName) True
    }

updateState :: Chan Req.Request -> View.State -> Event -> Transition View.State Event
updateState _ st (Inbound res) = Transition (st & View.core %~ Core.handleServerResponse res) (pure Nothing)
updateState out st (Outbound req) = Transition st $ writeChan out req $> Nothing
updateState _ st (StickyConversation b) = Transition (st & View.stickyMsgBox .~ b) (pure Nothing)
updateState _ st Ignore = Transition st (pure Nothing)
updateState _ _ Closed = Exit
