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
import qualified System.Console.ANSI.Declarative.Editor as Ansi
import qualified System.Console.ANSI.Declarative.View as Ansi

(|>) :: (a -> b) -> a -> b
(|>) = ($)

infixl 9 |>

viewState :: State -> Ansi.View
viewState state
  | _sidebarExpanded state =
    Ansi.Split Ansi.Vertical (Ansi.FromEnd 26)
      |> viewMainWindow borderChars state
      |> viewSideBar borderChars (_coreState state)
  | otherwise =
    viewMainWindow borderChars state
  where
    borderChars =
      if _unicodeEnabled state
        then Ansi.unicodeChars
        else Ansi.asciiChars

viewSideBar :: Ansi.BorderCharacters -> Core.State -> Ansi.View
viewSideBar borderChars state =
  Ansi.Border borderChars $
    Ansi.Padding (Ansi.padVertical 1) $
      Ansi.Split Ansi.Horizontal (Ansi.FromStart 4)
        |> do
          Ansi.Border borderChars $
            Ansi.Block Ansi.AlignTop Ansi.AlignCenter $
              Vector.fromList
                [ renderSystemMessage "Conversations",
                  Ansi.unstyled "(just a mockup)"
                ]
        |> do
          Ansi.Split Ansi.Horizontal (Ansi.FromEnd 10)
            |> do
              Ansi.Padding (Ansi.padHorizontal 1) $
                viewConversationList $
                  pure . Core._conversationName $ Core._currentConversation state
            |> do
              viewInstructions

viewInstructions :: Ansi.View
viewInstructions =
  Ansi.Block Ansi.AlignBottom Ansi.AlignLeft $
    Vector.fromList
      [ Ansi.unstyled "/quit",
        Ansi.unstyled "/sidebar",
        Ansi.unstyled "/unicode",
        Ansi.unstyled "",
        Ansi.unstyled "Enter: send message",
        Ansi.unstyled "Up/Down: select conv",
        Ansi.unstyled "Tab: toggle sidebar",
        Ansi.unstyled "Escape: quit"
      ]

viewConversationList :: Vector Conv.ConversationName -> Ansi.View
viewConversationList convs =
  Ansi.Block Ansi.AlignTop Ansi.AlignLeft $
    renderConversationName <$> convs

-------------------------------------------------------------------------------

viewMainWindow :: Ansi.BorderCharacters -> State -> Ansi.View
viewMainWindow borderChars state =
  Ansi.Split Ansi.Horizontal (Ansi.FromEnd 5)
    |> do
      Ansi.Padding (Ansi.padVertical 1) $
        viewConversation (Core._currentConversation $ _coreState state)
    |> do
      Ansi.Border borderChars $
        Ansi.viewEditor (_editor state)

viewConversation :: Core.ConversationState -> Ansi.View
viewConversation conv =
  viewHistory $ Core._conversationHistory conv

viewHistory :: Core.ConversationHistory -> Ansi.View
viewHistory history =
  Ansi.Block Ansi.AlignBottom Ansi.AlignLeft $
    renderHistoryEntry <$> Core._historyEntries history

-------------------------------------------------------------------------------

renderHistoryEntry :: Core.ConversationHistoryEntry -> Ansi.StyledLine
renderHistoryEntry (Core.Message sender msg) =
  renderUserName sender <> renderMessageBody (": " <> msg)
renderHistoryEntry (Core.UserJoined user) =
  renderSystemMessage (User.userNameText user <> " joined")
renderHistoryEntry (Core.UserLeft user) =
  renderSystemMessage (User.userNameText user <> " left")

renderSystemMessage :: Text -> Ansi.StyledLine
renderSystemMessage =
  Ansi.styled [Ansi.SetColor Ansi.Foreground Ansi.Dull Ansi.Cyan]

renderConversationName :: Conv.ConversationName -> Ansi.StyledLine
renderConversationName =
  Ansi.styled [Ansi.SetColor Ansi.Foreground Ansi.Dull Ansi.Red]
    . ("#" <>)
    . Conv.conversationNameText

renderUserName :: User.UserName -> Ansi.StyledLine
renderUserName =
  Ansi.styled
    [ Ansi.SetColor Ansi.Foreground Ansi.Dull Ansi.Red,
      Ansi.SetConsoleIntensity Ansi.BoldIntensity
    ]
    . User.userNameText

renderMessageBody :: Text -> Ansi.StyledLine
renderMessageBody = Ansi.unstyled
