{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Messaging.Client.GTK.View where

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

data Event
  = Inbound Res.Response
  | Outbound Req.Request
  | StickyMessages Bool
  | Closed
  | Ignore

data State = State
  { _core :: Core.State,
    _stickyMessages :: Bool
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
    $ container
      Box
      [#orientation := OrientationVertical]
      [ BoxChild defaultBoxChildProperties {expand = True, fill = True} $
          mapMsgBoxEvents <$> msgBox,
        BoxChild defaultBoxChildProperties $
          widget Entry [onM #keyPressEvent windowKeyPressEventHandler]
      ]
  where
    msgBox = messageBox [] (MessageBoxProps msgs (st ^. stickyMessages))
    msgs = fmap renderHistoryEntry (st ^. core . Core.currentHistory)
    mapMsgBoxEvents ~(ScrolledToBottom b) = StickyMessages b

viewHistory :: FromWidget (Container ListBox (Children (Bin ListBoxRow))) target => State -> target event
viewHistory st =
  container ListBox [#valign := AlignEnd] $
    (st ^. core . Core.currentHistory) <&> \req ->
      bin ListBoxRow [#activatable := False, #selectable := False] $
        widget Label [#label := renderHistoryEntry req]

renderHistoryEntry :: Core.ConversationHistoryEntry -> Text
renderHistoryEntry (Core.Message user msg) =
  User.userNameText user <> ": " <> msg
renderHistoryEntry (Core.UserJoined user) =
  User.userNameText user <> " JOINED"
renderHistoryEntry (Core.UserLeft user) =
  User.userNameText user <> " LEFT"

windowKeyPressEventHandler :: EventKey -> Entry -> IO (Bool, Event)
windowKeyPressEventHandler eventKey entry = do
  key <- getEventKeyString eventKey
  case key of
    Just "\ESC" -> return (True, Closed)
    Just "\r" -> do
      msg <- entryGetText entry
      entrySetText entry ""

      -- Don't sent empty messages
      if Text.null msg
        then return (True, Ignore)
        else return (True, Outbound $ Req.SendMessage $ Msg.Message Conv.conversationNameGeneral msg)
    Just _ -> return (False, Ignore)
    _ -> return (False, Ignore)
