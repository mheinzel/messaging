{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import qualified GI.Gtk as Gtk
import GI.Gtk.Declarative
import GI.Gtk.Declarative.App.Simple
import GI.Gtk.Declarative.Container.Class (Children)
import Lens.Micro
import Lens.Micro.TH
import Messaging.Client.Core.State
import Messaging.Client.GTK.UI.MessageBox
import qualified Messaging.Shared.Conversation as Conv
import qualified Messaging.Shared.Message as Msg
import qualified Messaging.Shared.Request as Req
import qualified Messaging.Shared.Response as Res
import qualified Messaging.Shared.User as User

data Event
  = Inbound Res.Response
  | Outbound Req.Request
  | StickMessages Bool
  | Closed
  | Ignore

data UIState = UIState
  { _cientState :: State,
    _isSticky :: Bool
  }

makeLenses ''UIState

update :: Chan Req.Request -> UIState -> Event -> Transition UIState Event
update _ st (Inbound res) =
  Transition (st & cientState %~ handleServerResponse res) (pure Nothing)
update out st (Outbound req) =
  Transition st $ writeChan out req $> Nothing
update _ st (StickMessages b) =
  Transition (st & isSticky .~ b) (pure Nothing)
update _ st Ignore =
  Transition st (pure Nothing)
update _ _ Closed =
  Exit

view :: UIState -> AppView Window Event
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
      [ BoxChild defaultBoxChildProperties $ ignoreEvent <$> msgBox,
        widget Entry [onM #keyPressEvent windowKeyPressEventHandler]
      ]
  where
    msgBox = messageBox [classes ["message-MessageBoxProps msgs Trueox"]] (MessageBoxProps msgs (st ^. isSticky))
    msgs = (st ^. cientState . currentHistory) & fmap renderRequest
    ignoreEvent (SrolledToBottom b) = StickMessages b

renderRequest :: ConversationHistoryEntry -> Text
renderRequest (Message user msg) =
  User.userNameText user <> ": " <> msg
renderRequest (UserJoined user) =
  User.userNameText user <> " JOINED"
renderRequest (UserLeft user) =
  User.userNameText user <> " LEFT"

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

initialState :: UIState
initialState = UIState emptyState True
