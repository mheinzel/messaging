{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Messaging.Client.GTK.View where

import Data.Maybe (fromMaybe)
import Data.Text as Text
import GI.Gdk (EventKey, getEventKeyString)
import GI.Gtk
  ( Align (..),
    Box (..),
    Entry (..),
    Label (..),
    ListBox (..),
    ListBoxRow (..),
    Orientation (..),
    Window (..),
    entryGetText,
    entrySetText,
  )
import GI.Gtk.Declarative
import GI.Gtk.Declarative.App.Simple (AppView)
import GI.Gtk.Declarative.Container.Class (Children)
import Lens.Micro ((<&>), (^.))
import Lens.Micro.TH
import qualified Messaging.Client.Core.State as Core
import Messaging.Client.GTK.UI.MessageBox (MessageBoxEvent (..), MessageBoxProps (..), messageBox)
import qualified Messaging.Shared.Conversation as Conv
import qualified Messaging.Shared.Message as Msg
import qualified Messaging.Shared.Request as Req
import qualified Messaging.Shared.Response as Res
import qualified Messaging.Shared.User as User
import Debug.Trace (trace)

data Event
  = Inbound Res.Response
  | Outbound Req.Request
  | StickyConversation Bool
  | Closed
  | Ignore

data State = State
  { _core :: Core.State,
    _stickyMsgBox :: Bool
  }

makeLenses ''State

view :: State -> AppView Window Event
view st =
  bin
    Window
    [ #title := "Title",
      #widthRequest := 800,
      #heightRequest := 600,
      on #deleteEvent (const (True, Closed))
    ]
    $ notebook [] $ mkConversationPage (st ^. stickyMsgBox) <$> Core.getAllConversationStates (_core st)

mkConversationPage :: Bool -> (Conv.ConversationName, Core.ConversationState) -> Page Event
mkConversationPage sticky (name, convSt) =
  page (Conv.conversationNameText name) $
    container
      Box
      [#orientation := OrientationVertical]
      [ BoxChild defaultBoxChildProperties {expand = True, fill = True} $ mapMsgBoxEvents <$> msgBox,
        widget Entry [onM #keyPressEvent (conversationKeyPressHandler name)]
      ]
  where
    msgBox = trace ("MSGS: " ++ show msgs) $ messageBox [] (MessageBoxProps msgs sticky)
    msgs = fmap renderHistoryEntry $ convSt ^. Core.conversationHistory . Core.historyEntries
    mapMsgBoxEvents ~(ScrolledToBottom b) = StickyConversation b

renderHistoryEntry :: Core.ConversationHistoryEntry -> Text
renderHistoryEntry (Core.Message user msg) =
  User.userNameText user <> ": " <> msg
renderHistoryEntry (Core.UserJoined user) =
  User.userNameText user <> " JOINED"
renderHistoryEntry (Core.UserLeft user) =
  User.userNameText user <> " LEFT"

conversationKeyPressHandler :: Conv.ConversationName -> EventKey -> Entry -> IO (Bool, Event)
conversationKeyPressHandler name eventKey entry = do
  key <- getEventKeyString eventKey
  case key of
    Just "\ESC" -> return (True, Closed)
    Just "\r" -> do
      msg <- entryGetText entry
      entrySetText entry ""

      if Text.null msg
        then return (True, Ignore)
        else return (True, Outbound $ Req.SendMessage $ Msg.Message name msg)
    _ -> return (False, Ignore)
