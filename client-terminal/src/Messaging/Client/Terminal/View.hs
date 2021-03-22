{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Messaging.Client.Terminal.View where

import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Messaging.Client.Core.State as Core
import Messaging.Client.Terminal.State
import qualified Messaging.Shared.Conversation as Conv
import qualified Messaging.Shared.User as User
import qualified System.Console.ANSI as Ansi
import qualified System.Console.ANSI.Declarative.Widget as Widget

-- This allows aligning the last few arguments of a function nicely without the
-- formatter trying to also align the first arguments.
-- Maybe there's a better way to keep things nice.
(|>) :: (a -> b) -> a -> b
(|>) = ($)

infixl 9 |>

viewState :: State -> Widget.SomeWidget
viewState state
  | _sidebarExpanded state =
    Widget.SomeWidget $
      Widget.splitRight 26
        |> viewMainWindow borderChars state
        |> viewSideBar borderChars (_coreState state)
  | otherwise =
    Widget.SomeWidget $
      viewMainWindow borderChars state
  where
    borderChars =
      if _unicodeEnabled state
        then Widget.unicodeChars
        else Widget.asciiChars

viewSideBar :: Widget.BorderCharacters -> Core.State -> Widget.Border
viewSideBar borderChars state =
  Widget.border borderChars $
    Widget.padding (Widget.padVertical 1) $
      Widget.splitTop 4
        |> do
          Widget.border borderChars $
            Widget.alignCenter . Widget.block $
              Vector.fromList
                [ renderSystemMessage "Conversations",
                  Widget.unstyled "(just a mockup)"
                ]
        |> do
          Widget.splitBottom 10
            |> do
              Widget.padding (Widget.padHorizontal 1) $
                viewConversationList $
                  pure . Core._conversationName $ Core._currentConversation state
            |> do
              viewInstructions

viewInstructions :: Widget.Block
viewInstructions =
  Widget.alignBottom . Widget.block $
    Vector.fromList
      [ Widget.unstyled "/quit",
        Widget.unstyled "/sidebar",
        Widget.unstyled "/unicode",
        Widget.unstyled "",
        Widget.unstyled "Enter: send message",
        Widget.unstyled "Up/Down: select conv",
        Widget.unstyled "Tab: toggle sidebar",
        Widget.unstyled "Escape: quit"
      ]

viewConversationList :: Vector Conv.ConversationName -> Widget.Block
viewConversationList convs =
  Widget.block $
    renderConversationName <$> convs

-------------------------------------------------------------------------------

viewMainWindow :: Widget.BorderCharacters -> State -> Widget.Split
viewMainWindow borderChars state =
  Widget.splitBottom 5
    |> do
      Widget.padding (Widget.padVertical 1) $
        viewConversation (Core._currentConversation $ _coreState state)
    |> do
      Widget.border borderChars $
        _editor state

viewConversation :: Core.ConversationState -> Widget.Block
viewConversation conv =
  viewHistory $ Core._conversationHistory conv

viewHistory :: Core.ConversationHistory -> Widget.Block
viewHistory history =
  Widget.alignBottom . Widget.block $
    renderHistoryEntry <$> Core._historyEntries history

-------------------------------------------------------------------------------

renderHistoryEntry :: Core.ConversationHistoryEntry -> Widget.StyledLine
renderHistoryEntry (Core.Message sender msg) =
  renderUserName sender <> renderMessageBody (": " <> msg)
renderHistoryEntry (Core.UserJoined user) =
  renderSystemMessage (User.userNameText user <> " joined")
renderHistoryEntry (Core.UserLeft user) =
  renderSystemMessage (User.userNameText user <> " left")

renderSystemMessage :: Text -> Widget.StyledLine
renderSystemMessage =
  Widget.styled [Ansi.SetColor Ansi.Foreground Ansi.Dull Ansi.Cyan]

renderConversationName :: Conv.ConversationName -> Widget.StyledLine
renderConversationName =
  Widget.styled [Ansi.SetColor Ansi.Foreground Ansi.Dull Ansi.Red]
    . ("#" <>)
    . Conv.conversationNameText

renderUserName :: User.UserName -> Widget.StyledLine
renderUserName =
  Widget.styled
    [ Ansi.SetColor Ansi.Foreground Ansi.Dull Ansi.Red,
      Ansi.SetConsoleIntensity Ansi.BoldIntensity
    ]
    . User.userNameText

renderMessageBody :: Text -> Widget.StyledLine
renderMessageBody = Widget.unstyled
