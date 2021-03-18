{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Messaging.Client.Terminal.UI where

import Control.Concurrent (Chan, forkIO, newChan, readChan, writeChan)
import Control.Monad (forever)
import Data.Text (Text)
import qualified Data.Text as Text
import Lens.Micro (over, (&))
import qualified Messaging.Client.Core.State as Core
import Messaging.Client.Terminal.State
import Messaging.Client.Terminal.View (viewState)
import qualified Messaging.Shared.Message as Msg
import qualified Messaging.Shared.Request as Req
import qualified Messaging.Shared.Response as Res
import qualified System.Console.ANSI.Declarative.Input as Ansi.Input
import qualified System.Console.ANSI.Declarative.Simple as Ansi.Simple

runUI :: Chan Res.Response -> Chan Req.Request -> IO ()
runUI incomingChan outgoingChan = do
  eventChan <- newChan
  -- TODO: lots of resource cleanup to take care of here
  _ <- forkIO $ forwardChan ServerResponse incomingChan eventChan
  _ <- forkIO $ Ansi.Input.readInputToChan Input eventChan
  () <$ Ansi.Simple.runApp (app eventChan outgoingChan)

app :: Chan Event -> Chan Req.Request -> Ansi.Simple.App State Event
app eventChan outgoingChan =
  Ansi.Simple.App
    { Ansi.Simple.update = handleEvent outgoingChan,
      Ansi.Simple.view = viewState,
      Ansi.Simple.inputs = eventChan,
      Ansi.Simple.initialState = initialState
    }

data Event = ServerResponse Res.Response | Input Ansi.Input.KeyboardInput
  deriving (Show)

forwardChan :: (a -> b) -> Chan a -> Chan b -> IO ()
forwardChan f inChan outChan = forever $ do
  x <- readChan inChan
  writeChan outChan (f x)

handleEvent :: Chan Req.Request -> State -> Event -> Ansi.Simple.Transition State
handleEvent outgoingChan state = \case
  ServerResponse res -> Ansi.Simple.Transition $ do
    pure $ state & over coreState (Core.handleServerResponse res)
  Input Ansi.Input.Enter ->
    case typedCommand (_inputState state) of
      Just CmdQuit -> Ansi.Simple.Exit
      Just (CmdSend txt) -> Ansi.Simple.Transition $ do
        let convName = currentConversationName state
        let msg = Msg.Message convName txt
        writeChan outgoingChan (Req.SendMessage msg)
        pure $ state & over inputState (handleKeyboardInput Ansi.Input.Enter)
      Nothing -> Ansi.Simple.Transition $ do
        pure $ state & over inputState (handleKeyboardInput Ansi.Input.Enter)
  Input kb -> Ansi.Simple.Transition $ do
    pure $ state & over inputState (handleKeyboardInput kb)

data Command = CmdQuit | CmdSend Text

typedCommand :: InputState -> Maybe Command
typedCommand = command . Text.strip . _inputStateText
  where
    command txt
      | Text.null txt = Nothing
      | Text.isPrefixOf "/quit" txt = Just CmdQuit
      | otherwise = Just (CmdSend txt)
