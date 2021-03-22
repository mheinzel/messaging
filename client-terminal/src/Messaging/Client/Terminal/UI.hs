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
runUI incomingChan outgoingChan = do
  () <$ Simple.runApp (app incomingChan outgoingChan)

app :: Chan Res.Response -> Chan Req.Request -> Simple.App Widget.SomeWidget State Event
app incomingChan outgoingChan =
  Simple.App
    { Simple.update = handleEvent outgoingChan,
      Simple.view = viewState,
      Simple.useKeyboardInput = Just (Just . Input),
      Simple.getAdditionalEvents = [ServerResponse <$> readChan incomingChan],
      Simple.initialState = initialState
    }

data Event = ServerResponse Res.Response | Input Input.KeyboardInput
  deriving (Show)

handleEvent :: Chan Req.Request -> State -> Event -> Simple.Transition State
handleEvent outgoingChan state = \case
  ServerResponse res ->
    Simple.Transition $
      pure $ over coreState (Core.handleServerResponse res) state
  Input Input.Escape -> Simple.Exit
  Input Input.Tab -> Simple.Transition $ do
    pure $ toggleSidebar state
  Input Input.Enter ->
    case typedCommand state of
      Just CmdQuit -> Simple.Exit
      Just CmdSidebar -> Simple.Transition $ do
        pure $ resetEditor $ toggleSidebar state
      Just CmdUnicode -> Simple.Transition $ do
        pure $ resetEditor $ toggleUnicode state
      Just (CmdSend txt) -> Simple.Transition $ do
        let convName = currentConversationName state
        let msg = Msg.Message convName txt
        writeChan outgoingChan (Req.SendMessage msg)
        pure $ resetEditor state
      Nothing -> Simple.Transition $ do
        -- Don't send or delete if no command was detected.
        pure state
  Input kb -> Simple.Transition $ do
    pure $ handleEditorInput kb state

data Command
  = CmdQuit
  | CmdSidebar
  | CmdUnicode
  | CmdSend Text

typedCommand :: State -> Maybe Command
typedCommand = command . Text.strip . Text.unlines . editorContent
  where
    command txt
      | Text.null txt = Nothing
      | Text.isPrefixOf "/" txt = findCommand txt
      | otherwise = Just (CmdSend txt)

    findCommand txt =
      fmap snd . List.find (flip Text.isPrefixOf txt . fst) $
        [ ("/quit", CmdQuit),
          ("/sidebar", CmdSidebar),
          ("/unicode", CmdUnicode)
        ]
