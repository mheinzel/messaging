{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Messaging.Client.Terminal.UI where

import Control.Concurrent (Chan, readChan, writeChan)
import Data.Text (Text)
import qualified Data.Text as Text
import Lens.Micro (over)
import qualified Messaging.Client.Core.State as Core
import Messaging.Client.Terminal.State
import Messaging.Client.Terminal.View (viewState)
import qualified Messaging.Shared.Conversation as Conv
import qualified Messaging.Shared.Message as Msg
import qualified Messaging.Shared.Request as Req
import qualified Messaging.Shared.Response as Res
import qualified Messaging.Shared.User as User
import qualified System.Console.ANSI.Declarative.Input as Input
import qualified System.Console.ANSI.Declarative.Simple as Simple
import qualified System.Console.ANSI.Declarative.Widget as Widget

runUI :: User.UserName -> Chan Res.Response -> Chan Req.Request -> IO ()
runUI user incomingChan outgoingChan = do
  () <$ Simple.runApp (app user incomingChan outgoingChan)

app ::
  User.UserName ->
  Chan Res.Response ->
  Chan Req.Request ->
  Simple.App Widget.SomeWidget State Event
app user incomingChan outgoingChan =
  Simple.App
    { Simple.update = handleEvent outgoingChan,
      Simple.view = const viewState,
      Simple.events =
        [ ServerResponse <$> readChan incomingChan,
          Input <$> Input.readInput,
          Tick <$ Input.waitForTickMilliseconds 1000
        ],
      Simple.initialState = initialState user
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
  Input (Input.Arrow Input.ArrowUp) -> Simple.Transition $ do
    pure $ focusPreviousConversation state
  Input (Input.Arrow Input.ArrowDown) -> Simple.Transition $ do
    pure $ focusNextConversation state
  Input Input.Enter ->
    case typedCommand state of
      Right (Just CmdQuit) -> Simple.Exit
      Right (Just CmdSidebar) -> Simple.Transition $ do
        pure $ resetEditor $ toggleSidebar state
      Right (Just CmdUnicode) -> Simple.Transition $ do
        pure $ resetEditor $ toggleUnicode state
      Right (Just (CmdJoin convName)) -> Simple.Transition $ do
        writeChan outgoingChan $ Req.JoinConversation convName
        -- already mark this conversation as focussed, so it will be the
        -- current one once the server adds us to it.
        pure $ resetEditor $ focusConversation convName state
      Right (Just (CmdLeave convName)) -> Simple.Transition $ do
        writeChan outgoingChan (Req.LeaveConversation convName)
        pure $ resetEditor state
      Right (Just (CmdSwitch convName)) -> Simple.Transition $ do
        pure $ resetEditor $ focusConversation convName state
      Right (Just (CmdSend txt)) -> Simple.Transition $ do
        case currentConversationName state of
          Just convName -> do
            let msg = Msg.Message convName txt
            writeChan outgoingChan (Req.SendMessage msg)
            pure $ resetEditor state
          Nothing ->
            -- TODO: display error message
            pure state
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
    CmdJoin Conv.ConversationName
  | -- | Leave a conversation
    CmdLeave Conv.ConversationName
  | -- | Switch to a conversation
    CmdSwitch Conv.ConversationName
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

    findCommand txt = case Text.words txt of
      -- ignore text after these commands
      ("/quit" : _) -> Right . Just $ CmdQuit
      ("/sidebar" : _) -> Right . Just $ CmdSidebar
      ("/unicode" : _) -> Right . Just $ CmdUnicode
      -- parse conversation name
      ("/join" : rest) -> conversationCommand CmdJoin "/join" rest
      ("/leave" : rest) -> conversationCommand CmdLeave "/leave" rest
      ("/switch" : rest) -> conversationCommand CmdSwitch "/switch" rest
      -- fallback
      other -> Left $ CommandError (head other) "Not a valid command"

    conversationCommand cmd _ [c]
      | Just convName <- Conv.mkConversationName c = Right . Just $ cmd convName
    conversationCommand _ txtCmd _ =
      Left $ CommandError txtCmd $ "Usage: " <> txtCmd <> " <conversation name>"
