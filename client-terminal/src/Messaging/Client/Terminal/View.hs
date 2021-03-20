{-# LANGUAGE OverloadedStrings #-}

module Messaging.Client.Terminal.View where

import Data.Text (Text)
import qualified Messaging.Client.Core.State as Core
import Messaging.Client.Terminal.State
import qualified Messaging.Shared.User as User
import qualified System.Console.ANSI as Ansi
import qualified System.Console.ANSI.Declarative.Editor as Ansi
import qualified System.Console.ANSI.Declarative.View as Ansi

viewState :: State -> Ansi.View
viewState state =
  (Ansi.Split Ansi.Horizontal (Ansi.FromEnd 2))
    (viewCoreState (_coreState state))
    (Ansi.BarAtTop '-' (Ansi.viewEditor (_editor state)))

viewCoreState :: Core.State -> Ansi.View
viewCoreState state =
  viewHistory $ Core._conversationHistory (Core._currentConversation state)

viewHistory :: Core.ConversationHistory -> Ansi.View
viewHistory history =
  Ansi.Block $ renderHistoryEntry <$> Core._historyEntries history

-------------------------------------------------------------------------------

renderHistoryEntry :: Core.ConversationHistoryEntry -> Ansi.StyledLine
renderHistoryEntry (Core.Message sender msg) =
  renderUserName sender <> renderMessageBody (": " <> msg)
renderHistoryEntry (Core.UserJoined user) =
  renderSystemMessage (User.userNameText user <> " JOINED")
renderHistoryEntry (Core.UserLeft user) =
  renderSystemMessage (User.userNameText user <> " LEFT")

renderSystemMessage :: Text -> Ansi.StyledLine
renderSystemMessage =
  Ansi.styled [Ansi.SetColor Ansi.Foreground Ansi.Dull Ansi.Cyan]

renderUserName :: User.UserName -> Ansi.StyledLine
renderUserName =
  Ansi.styled [Ansi.SetColor Ansi.Foreground Ansi.Dull Ansi.Red] . User.userNameText

renderMessageBody :: Text -> Ansi.StyledLine
renderMessageBody = Ansi.unstyled
