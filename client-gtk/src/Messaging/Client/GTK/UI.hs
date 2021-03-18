{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Messaging.Client.GTK.UI where

import Control.Concurrent.Chan (Chan, writeChan)
import Data.Functor (($>))
import Data.Text (Text)
import Data.Vector (Vector)
import Debug.Trace (trace)
import GI.Gdk (EventKey, getEventKeyString)
import GI.Gtk
  ( Align (..),
    Box (..),
    Entry (..),
    EventControllerScroll (..),
    Label (..),
    ListBox (..),
    ListBoxRow (..),
    Orientation (..),
    ScrollType (..),
    ScrolledWindow (..),
    Window (..),
    entryGetText,
    entrySetText,
  )
import GI.Gtk.Declarative
import GI.Gtk.Declarative.App.Simple
import GI.Gtk.Declarative.Container.Class (Children)
import Lens.Micro
import Lens.Micro.TH
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

data State = State
  { _history :: Vector Res.Response,
    --    _historySticky :: Bool,
    _editor :: Text
  }

makeLenses ''State

update :: Chan Req.Request -> State -> Event -> Transition State Event
update _ st (Inbound res) =
  Transition (st & history %~ \l -> l <> [res]) (pure Nothing)
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
    (st ^. history) <&> \req ->
      bin ListBoxRow [#activatable := False, #selectable := False] $
        widget Label [#label := renderRequest req]

renderRequest :: Res.Response -> Text
renderRequest (Res.ReceivedMessage user msg) =
  User.userNameText (User.userName user) <> ": " <> Msg.messageContent msg
renderRequest (Res.JoinedConversation user _conv) =
  User.userNameText (User.userName user) <> " JOINED"
renderRequest (Res.LeftConversation user _conv) =
  User.userNameText (User.userName user) <> " LEFT"

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
initialState = State [] mempty
