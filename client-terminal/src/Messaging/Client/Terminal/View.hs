{-# LANGUAGE OverloadedStrings #-}

module Messaging.Client.Terminal.View where

import Data.Foldable (fold)
import Data.Text (Text)
import Data.Vector (Vector)
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
viewState state =
  Ansi.Split Ansi.Vertical (Ansi.FromStart 20)
    |> viewSideBar (_coreState state)
    |> viewMainWindow state

viewSideBar :: Core.State -> Ansi.View
viewSideBar state =
  Ansi.Split Ansi.Horizontal (Ansi.FromStart 1)
    |> Ansi.Block (pure $ Ansi.unstyled "Conversations")
    |> viewConversationList (pure . Core._conversationName $ Core._currentConversation state)

viewConversationList :: Vector Conv.ConversationName -> Ansi.View
viewConversationList convs =
  Ansi.Block . fold $
    [ renderConversationName <$> convs,
      pure $ Ansi.unstyled "(just a mockup)"
    ]

viewMainWindow :: State -> Ansi.View
viewMainWindow state =
  Ansi.Split Ansi.Horizontal (Ansi.FromEnd 1)
    |> viewConversation (Core._currentConversation $ _coreState state)
    |> Ansi.viewEditor (_editor state)

viewConversation :: Core.ConversationState -> Ansi.View
viewConversation conv =
  viewHistory $ Core._conversationHistory conv

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
