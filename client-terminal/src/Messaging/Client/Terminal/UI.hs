{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Messaging.Client.Terminal.UI where

import Control.Concurrent (Chan, readChan, writeChan)
import Data.Bifunctor (first)
import Data.Text (Text)
import qualified Data.Text as Text
import Lens.Micro (over, (^.))
import qualified Messaging.Client.Core.State as Core
import Messaging.Client.Terminal.State
import Messaging.Client.Terminal.View (viewState)
import qualified Messaging.Shared.Conversation as Conv
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
      Simple.view = const viewState,
      Simple.events =
        [ ServerResponse <$> readChan incomingChan,
          Input <$> Input.readInput,
          Tick <$ Input.waitForTickMilliseconds 1000
        ],
      Simple.initialState = initialState
    }

data Event
  = ServerResponse Res.Response
  | Input Input.KeyboardInput
  | -- | This makes sure we occasionally re-render the UI, e.g. after resizing
    -- the terminal.
    Tick
  deriving (Show)

handleEvent :: Chan Req.Request -> State -> Event -> Simple.Transition State
handleEvent outgoingChan state = \case
  Tick -> Simple.Transition $ pure state
  ServerResponse res ->
    Simple.Transition $
      pure $ over coreState (Core.handleServerResponse res) state
  Input Input.Escape -> Simple.Exit
  Input Input.Tab -> Simple.Transition $ do
    pure $ toggleSidebar state
  Input Input.Enter ->
    case typedCommand state of
      Right (Just CmdQuit) -> Simple.Exit
      Right (Just CmdSidebar) -> Simple.Transition $ do
        pure $ resetEditor $ toggleSidebar state
      Right (Just CmdUnicode) -> Simple.Transition $ do
        pure $ resetEditor $ toggleUnicode state
      Right (Just (CmdJoin conv)) -> Simple.Transition $ do
        let convName = Conv.ConversationName conv
        if hasJoined convName state
          then pure $ resetEditor state
          else do
            writeChan outgoingChan $ Req.JoinConversation convName
            pure $ resetEditor $ addConversation convName state
      Right (Just (CmdLeave conv)) -> Simple.Transition $ do
        let convName = Conv.ConversationName conv
        if hasJoined convName state
          then do
            writeChan outgoingChan (Req.LeaveConversation convName)
            let newConv
                  | state ^. currentConversation == Just convName = someConversation state
                  | otherwise = Just convName
            pure $
              resetEditor $
                setConversation newConv $
                  removeConversation convName state
          else pure $ resetEditor state
      Right (Just (CmdSwitch conv)) -> Simple.Transition $ do
        let convName = Conv.ConversationName conv
        pure $ resetEditor $ setConversation (Just convName) state
      Right (Just (CmdSend txt)) -> Simple.Transition $ do
        let convName = currentConversationName state
        let msg = Msg.Message convName txt
        writeChan outgoingChan (Req.SendMessage msg)
        pure $ resetEditor state
      Right Nothing -> Simple.Transition $ do
        -- Don't send or delete if no command was detected.
        pure state
      Left _ -> Simple.Transition $ do
        -- TODO: display error message
        pure $ resetEditor state
  Input kb -> Simple.Transition $ do
    pure $ handleEditorInput kb state

data Command
  = CmdQuit
  | CmdSidebar
  | CmdUnicode
  | -- | Join a conversation
    CmdJoin Text
  | -- | Leave a conversation
    CmdLeave Text
  | -- | Switch to a conversation
    CmdSwitch Text
  | CmdSend Text

data CommandError = CommandError
  { failedCommand :: Text,
    errorMsg :: Text
  }

typedCommand :: State -> Either CommandError (Maybe Command)
typedCommand = command . Text.strip . Text.unlines . editorContent
  where
    command txt
      | Text.null txt = Right Nothing
      | Text.isPrefixOf "/" txt = findCommand txt
      | otherwise = Right . Just $ CmdSend txt

    parse = first head . splitAt 1 . Text.words

    findCommand txt = case parse txt of
      ("/quit", []) -> Right . Just $ CmdQuit
      ("/quit", _) -> Left $ CommandError "/quit" "Usage: /quit"
      ("/sidebar", []) -> Right . Just $ CmdSidebar
      ("/sidebar", _) -> Left $ CommandError "/sidebar" "Usage: /sidebar"
      ("/unicode", []) -> Right . Just $ CmdUnicode
      ("/unicode", _) -> Left $ CommandError "/unicode" "Usage: /unicode"
      ("/join", [conv]) -> Right . Just $ CmdJoin conv
      ("/join", _) -> Left $ CommandError "/join" "Usage: /join <conversation name>"
      ("/leave", [conv]) -> Right . Just $ CmdLeave conv
      ("/leave", _) -> Left $ CommandError "/leave" "Usage: /leave <conversation name>"
      ("/switch", [conv]) -> Right . Just $ CmdSwitch conv
      ("/switch", _) -> Left $ CommandError "/switch" "Usage: /switch <conversation name>"
      (unknownCmd, _) -> Left $ CommandError unknownCmd "Not a valid command"
