{-# LANGUAGE OverloadedStrings #-}

module Messaging.Client.Terminal.View where

import qualified Data.Char as Char
import Data.Foldable (toList)
import Data.Function (on)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Messaging.Client.Core.State as Core
import Messaging.Client.Terminal.State
import qualified Messaging.Shared.Conversation as Conv
import qualified Messaging.Shared.User as User
import Prettyprinter ((<+>))
import qualified Prettyprinter as PP
import qualified Prettyprinter.Render.Terminal as PP
import qualified System.Console.ANSI.Declarative.Widget as Widget

-- This allows aligning the last few arguments of a function nicely without
-- having to add parentheses (or the ormolu formatter trying to also align the
-- first arguments).
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
        |> viewSideBar borderChars state
  | otherwise =
    Widget.SomeWidget $
      viewMainWindow borderChars state
  where
    borderChars =
      if _unicodeEnabled state
        then Widget.unicodeChars
        else Widget.asciiChars

viewSideBar :: Widget.BorderCharacters -> State -> Widget.Border
viewSideBar borderChars state =
  Widget.border borderChars $
    Widget.padding (Widget.padLeft 1 <> Widget.padRight 1) $
      Widget.splitTop 3
        |> do
          Widget.border borderChars $
            Widget.prettyBlock $
              PP.annotate (PP.color PP.Cyan) $
                Widget.centeredText "Conversations"
        |> do
          Widget.splitBottom 12
            |> do
              Widget.padding (Widget.padTop 1) $
                viewConversationList state $
                  Core._conversationName <$> Core._joinedConversations (_coreState state)
            |> do
              viewInstructions

viewInstructions :: Widget.Block
viewInstructions =
  Widget.alignBottom . Widget.prettyBlock $
    PP.vsep
      [ "/quit",
        "/sidebar",
        "/unicode",
        "/join",
        "/leave",
        "/switch",
        "",
        "Enter: send message",
        "Up/Down: select conv",
        "Tab: toggle sidebar",
        "Escape: quit"
      ]

viewConversationList ::
  Foldable f =>
  State ->
  f Conv.ConversationName ->
  Widget.Block
viewConversationList state convs =
  Widget.prettyBlock $
    PP.vsep $
      map (\convName -> renderConversationName (isFocused convName state) convName) (toList convs)
        <> pure ""

viewMainWindow :: Widget.BorderCharacters -> State -> Widget.Split
viewMainWindow borderChars state =
  Widget.splitBottom 5
    |> do
      Widget.padding (Widget.padLeft 1 <> Widget.padRight 1) $
        maybe
          (Widget.block "")
          viewConversation
          (currentConversation state)
    |> do
      Widget.border borderChars $
        _editor state

viewConversation :: Core.ConversationState -> Widget.Block
viewConversation conv =
  viewHistory $ Core._conversationHistory conv

viewHistory :: Core.ConversationHistory -> Widget.Block
viewHistory history =
  Widget.alignBottom . Widget.prettyBlock $
    PP.vsep $
      renderHistoryEntry <$> toList (Core._historyEntries history)

renderHistoryEntry :: Core.ConversationHistoryEntry -> PP.Doc PP.AnsiStyle
renderHistoryEntry (Core.Message sender msg) =
  (renderUserName sender <> ":")
    -- Allow wrapping text at each beginning and end of whitespace
    <+> PP.fillCat (PP.pretty <$> Text.groupBy ((==) `on` Char.isSpace) msg)
renderHistoryEntry (Core.UserJoined user) =
  renderSystemMessage (User.userNameText user <> " joined")
renderHistoryEntry (Core.UserLeft user) =
  renderSystemMessage (User.userNameText user <> " left")

renderSystemMessage :: Text -> PP.Doc PP.AnsiStyle
renderSystemMessage =
  PP.annotate (PP.color PP.Cyan) . PP.pretty

renderConversationName ::
  FocusState ->
  Conv.ConversationName ->
  PP.Doc PP.AnsiStyle
renderConversationName focusState =
  let color = case focusState of
        Focused -> PP.Cyan
        Unfocused -> PP.Red
  in PP.annotate (PP.color color) . PP.pretty . ("#" <>) . Conv.conversationNameText

renderUserName :: User.UserName -> PP.Doc PP.AnsiStyle
renderUserName =
  PP.annotate (PP.color PP.Red <> PP.bold) . PP.pretty . User.userNameText
