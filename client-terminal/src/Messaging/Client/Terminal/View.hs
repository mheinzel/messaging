{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Messaging.Client.Terminal.View where

import Data.Text (Text)
import qualified Messaging.Client.Core.State as Core
import Messaging.Client.Terminal.State
import qualified Messaging.Shared.User as User
import qualified System.Console.ANSI as Ansi
import qualified System.Console.ANSI.Declarative.View as Ansi

viewState :: State -> Ansi.View
viewState state =
  -- maybe it would be nicer to make it greedy/adaptive?
  (Ansi.Split Ansi.Horizontal (Ansi.FromEnd 2))
    (viewCoreState (_coreState state))
    (viewInputState (_inputState state))

viewInputState :: InputState -> Ansi.View
viewInputState state =
  Ansi.BarAtTop '-' $
    Ansi.Block $ pure $ styleInput $ _inputStateText state

viewCoreState :: Core.State -> Ansi.View
viewCoreState state =
  viewHistory $ Core._conversationHistory (Core._currentConversation state)

viewHistory :: Core.ConversationHistory -> Ansi.View
viewHistory history =
  Ansi.Block $ styleLine <$> Core._historyEntries history

-------------------------------------------------------------------------------

styleLine :: Core.ConversationHistoryEntry -> Ansi.StyledLine
styleLine (Core.Message sender msg) =
  styleSender (User.userNameText sender)
    <> styleMessage (": " <> msg)
styleLine (Core.UserJoined user) =
  styleSystemMessage (User.userNameText user <> " JOINED")
styleLine (Core.UserLeft user) =
  styleSystemMessage (User.userNameText user <> " LEFT")

styleSystemMessage :: Text -> Ansi.StyledLine
styleSystemMessage =
  Ansi.StyledLine
    . pure
    . Ansi.StyledSegment
      [Ansi.SetColor Ansi.Foreground Ansi.Dull Ansi.Cyan]

styleSender :: Text -> Ansi.StyledLine
styleSender =
  Ansi.StyledLine
    . pure
    . Ansi.StyledSegment
      [Ansi.SetColor Ansi.Foreground Ansi.Dull Ansi.Red]

styleMessage :: Text -> Ansi.StyledLine
styleMessage =
  Ansi.StyledLine
    . pure
    . Ansi.StyledSegment
      [Ansi.SetColor Ansi.Foreground Ansi.Dull Ansi.Black]

styleInput :: Text -> Ansi.StyledLine
styleInput = styleMessage

styleSeparator :: Text -> Ansi.StyledLine
styleSeparator = styleMessage
