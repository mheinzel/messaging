{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Messaging.Client.GTK.UI where

import Control.Concurrent.Chan (Chan, writeChan)
import Data.Functor (($>))
import Data.Text (Text)
import Debug.Trace (trace)
import GI.Gdk (EventKey, getEventKeyString)
import GI.Gtk
  ( Align (..),
    Box (..),
    Entry (..),
    -- EventControllerScroll (..),
    Label (..),
    ListBox (..),
    ListBoxRow (..),
    Orientation (..),
    -- ScrollType (..),
    ScrolledWindow (..),
    Window (..),
    entryGetText,
    entrySetText,
  )
import GI.Gtk.Declarative
import GI.Gtk.Declarative.App.Simple (Transition (..), AppView)
import GI.Gtk.Declarative.Container.Class (Children)
import Lens.Micro ((&), (<&>), (^.))
import Messaging.Client.Core.State as Core
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

update :: Chan Req.Request -> State -> Event -> Transition State Event
update _ st (Inbound res) =
  Transition (st & handleServerResponse res) (pure Nothing)
--update _ st (Stick b) =
--  Transition (st & historySticky .~ b) (pure Nothing)
update out st (Outbound req) =
  Transition st $ writeChan out req $> Nothing
update _ st Ignore = Transition st (pure Nothing)
update _ _ Closed = Exit

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
      [#orientation := OrientationVertical, #valign := AlignEnd]
      [ bin ScrolledWindow [#propagateNaturalHeight := True] $ viewHistory st,
        widget Entry [onM #keyPressEvent windowKeyPressEventHandler]
      ]

viewHistory :: FromWidget (Container ListBox (Children (Bin ListBoxRow))) target => State -> target event
viewHistory st =
  container ListBox [#valign := AlignEnd] $
    (st ^. currentHistory) <&> \req ->
      bin ListBoxRow [#activatable := False, #selectable := False] $
        widget Label [#label := renderRequest req]

renderRequest :: ConversationHistoryEntry -> Text
renderRequest (Message user msg) =
  User.userNameText user <> ": " <> msg
renderRequest (UserJoined user) =
  User.userNameText user <> " JOINED"
renderRequest (UserLeft user) =
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

initialState :: State
initialState = emptyState
