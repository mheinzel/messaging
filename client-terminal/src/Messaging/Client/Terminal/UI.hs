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
import qualified System.Console.ANSI.Declarative.Input as Ansi.Input
import qualified System.Console.ANSI.Declarative.Simple as Ansi.Simple

runUI :: Chan Res.Response -> Chan Req.Request -> IO ()
runUI incomingChan outgoingChan = do
  () <$ Ansi.Simple.runApp (app incomingChan outgoingChan)

app :: Chan Res.Response -> Chan Req.Request -> Ansi.Simple.App State Event
app incomingChan outgoingChan =
  Ansi.Simple.App
    { Ansi.Simple.update = handleEvent outgoingChan,
      Ansi.Simple.view = viewState,
      Ansi.Simple.useKeyboardInput = Just (Just . Input),
      Ansi.Simple.getAdditionalEvents = [ServerResponse <$> readChan incomingChan],
      Ansi.Simple.initialState = initialState
    }

data Event = ServerResponse Res.Response | Input Ansi.Input.KeyboardInput
  deriving (Show)

handleEvent :: Chan Req.Request -> State -> Event -> Ansi.Simple.Transition State
handleEvent outgoingChan state = \case
  ServerResponse res ->
    Ansi.Simple.Transition $
      pure $ over coreState (Core.handleServerResponse res) state
  Input Ansi.Input.Escape -> Ansi.Simple.Exit
  Input Ansi.Input.Tab -> Ansi.Simple.Transition $ do
    pure $ toggleSidebar state
  Input Ansi.Input.Enter ->
    case typedCommand state of
      Just CmdQuit -> Ansi.Simple.Exit
      Just CmdSidebar -> Ansi.Simple.Transition $ do
        pure $ resetEditor $ toggleSidebar state
      Just CmdUnicode -> Ansi.Simple.Transition $ do
        pure $ resetEditor $ toggleUnicode state
      Just (CmdSend txt) -> Ansi.Simple.Transition $ do
        let convName = currentConversationName state
        let msg = Msg.Message convName txt
        writeChan outgoingChan (Req.SendMessage msg)
        pure $ resetEditor state
      Nothing -> Ansi.Simple.Transition $ do
        -- Don't send or delete if no command was detected.
        pure state
  Input kb -> Ansi.Simple.Transition $ do
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
      fmap snd .  List.find (flip Text.isPrefixOf txt . fst) $
          [ ("/quit", CmdQuit),
            ("/sidebar", CmdSidebar),
            ("/unicode", CmdUnicode)
          ]
