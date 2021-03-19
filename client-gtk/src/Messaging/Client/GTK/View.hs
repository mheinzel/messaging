{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Messaging.Client.GTK.View where

import Data.Text (Text)
import Debug.Trace (trace) -- TODO: get rid of this
import GI.Gdk (EventKey, getEventKeyString)
import GI.Gtk
  ( Align (..),
    Box (..),
    Entry (..),
    Label (..),
    ListBox (..),
    ListBoxRow (..),
    Orientation (..),
    ScrolledWindow (..),
    Window (..),
    entryGetText,
    entrySetText,
  )
import GI.Gtk.Declarative
import GI.Gtk.Declarative.App.Simple (AppView)
import GI.Gtk.Declarative.Container.Class (Children)
import Lens.Micro ((<&>), (^.))
import qualified Messaging.Client.Core.State as Core
import qualified Messaging.Shared.Conversation as Conv
import qualified Messaging.Shared.Message as Msg
import qualified Messaging.Shared.Request as Req
import qualified Messaging.Shared.Response as Res
import qualified Messaging.Shared.User as User

data Event
  = Inbound Res.Response
  | Outbound Req.Request
  | --  | Stick Bool
    Closed
  | Ignore

view :: Core.State -> AppView Window Event
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
      [#orientation := OrientationVertical, #valign := AlignEnd]
      [ bin ScrolledWindow [#propagateNaturalHeight := True] $ viewHistory st,
        widget Entry [onM #keyPressEvent windowKeyPressEventHandler]
      ]

viewHistory :: FromWidget (Container ListBox (Children (Bin ListBoxRow))) target => Core.State -> target event
viewHistory st =
  container ListBox [#valign := AlignEnd] $
    (st ^. Core.currentHistory) <&> \req ->
      bin ListBoxRow [#activatable := False, #selectable := False] $
        widget Label [#label := renderHistoryEntry req]

renderHistoryEntry :: Core.ConversationHistoryEntry -> Text
renderHistoryEntry (Core.Message user msg) =
  User.userNameText user <> ": " <> msg
renderHistoryEntry (Core.UserJoined user) =
  User.userNameText user <> " JOINED"
renderHistoryEntry (Core.UserLeft user) =
  User.userNameText user <> " LEFT"

--scrollEvent :: ScrollType -> Bool -> (Bool, Event)
--scrollEvent ScrollTypeEnd True = trace "stick" (True, Stick True)
--scrollEvent _ _ = trace "DoNt stick" (True, Stick False)

windowKeyPressEventHandler :: EventKey -> Entry -> IO (Bool, Event)
windowKeyPressEventHandler eventKey entry = do
  key <- getEventKeyString eventKey
  case key of
    Just "\ESC" -> return (True, Closed)
    Just "\r" -> do
      msg <- Msg.Message Conv.conversationNameGeneral <$> entryGetText entry
      entrySetText entry ""
      return (True, Outbound $ Req.SendMessage msg)
    Just k -> trace ("Pressed: " ++ show k) $ return (False, Ignore)
    _ -> return (False, Ignore)
