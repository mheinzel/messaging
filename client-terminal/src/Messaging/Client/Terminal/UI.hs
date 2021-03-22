{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Messaging.Client.Terminal.UI where

import Control.Concurrent (Chan, readChan, writeChan)
import qualified Data.List as List
import Data.Text (Text)
import qualified Data.Text as Text
import Lens.Micro (over)
import qualified Messaging.Client.Core.State as Core
import Messaging.Client.Terminal.State
import Messaging.Client.Terminal.View (viewState)
import qualified Messaging.Shared.Message as Msg
import qualified Messaging.Shared.Request as Req
import qualified Messaging.Shared.Response as Res
import qualified System.Console.ANSI.Declarative.Input as Input
import qualified System.Console.ANSI.Declarative.Simple as Simple
import qualified System.Console.ANSI.Declarative.Widget as Widget

runUI :: Chan Res.Response -> Chan Req.Request -> IO ()
runUI _incomingChan _outgoingChan = do
  () <$ Simple.runApp app

app ::  Simple.App Widget.SomeWidget State Input.KeyboardInput
app =
  Simple.App
    { Simple.update = handleEvent,
      Simple.view = const viewState,
      Simple.events = [ Input.readInput],
      Simple.initialState = initialState
    }

handleEvent :: State -> Input.KeyboardInput -> Simple.Transition State
handleEvent state input=
  Simple.Transition $ pure $ addInput input state
