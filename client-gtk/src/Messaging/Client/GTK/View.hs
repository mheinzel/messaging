{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Messaging.Client.GTK.View where

import Data.Maybe (fromMaybe)
import Data.Text as Text
import Data.Vector as Vec
import Debug.Trace (trace)
import GI.Gdk (EventKey, getEventKeyString)
import GI.Gtk
  ( Align (..),
    Box (..),
    Button (..),
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
    $ notebook [] $
      trace (show $ Core.getAllConversationStates (_core st)) $
      (mkConversationPage (st ^. stickyMsgBox) <$> Core.getAllConversationStates (_core st))
        <> Vec.singleton addConversationPage

addConversationPage :: Page Event
addConversationPage =
  page "+" $
    centered $
      container
        Box
        [#orientation := OrientationHorizontal, #valign := AlignCenter, #spacing := 20]
        [ BoxChild defaultBoxChildProperties $ widget Label [#label := "Join conversation: "],
          BoxChild defaultBoxChildProperties $ widget Entry [onM #keyPressEvent newConversationEntryHandler]
        ]

mkConversationPage :: Bool -> (Conv.ConversationName, Core.ConversationState) -> Page Event
mkConversationPage sticky (name, convSt) =
  pageWithTab
    ( container
        Box
        [#orientation := OrientationHorizontal, #spacing := 20]
        [ widget Label [#label := Conv.conversationNameText name],
          widget Button [#label := "X", on #clicked (Outbound $ Req.LeaveConversation name)]
        ]
    )
    $ container
      Box
      [#orientation := OrientationVertical]
      [ BoxChild defaultBoxChildProperties {expand = True, fill = True} $ mapMsgBoxEvents <$> msgBox,
        BoxChild defaultBoxChildProperties $ widget Entry [onM #keyPressEvent (conversationEntryHandler name)]
      ]
  where
    msgBox = messageBox [] (MessageBoxProps msgs sticky)
    msgs = fmap renderHistoryEntry $ convSt ^. Core.conversationHistory . Core.historyEntries
    mapMsgBoxEvents ~(ScrolledToBottom b) = StickyConversation b

renderHistoryEntry :: Core.ConversationHistoryEntry -> Text
renderHistoryEntry (Core.Message user msg) =
  User.userNameText user <> ": " <> msg
renderHistoryEntry (Core.UserJoined user) =
  User.userNameText user <> " JOINED"
renderHistoryEntry (Core.UserLeft user) =
  User.userNameText user <> " LEFT"

newConversationEntryHandler :: EventKey -> Entry -> IO (Bool, Event)
newConversationEntryHandler = entryHandler $ \msg -> Outbound $ Req.JoinConversation $ Conv.ConversationName msg

conversationEntryHandler :: Conv.ConversationName -> EventKey -> Entry -> IO (Bool, Event)
conversationEntryHandler name = entryHandler $ \msg -> Outbound $ Req.SendMessage $ Msg.Message name msg

entryHandler :: (Text -> Event) -> EventKey -> Entry -> IO (Bool, Event)
entryHandler handleFun eventKey entry = do
  key <- getEventKeyString eventKey
  case key of
    Just "\ESC" -> return (True, Closed)
    Just "\r" -> do
      msg <- entryGetText entry
      entrySetText entry ""

      if Text.null msg
        then return (True, Ignore)
        else return (True, handleFun msg)
    _ -> return (False, Ignore)

centered :: Widget e -> Widget e
centered w =
  container
    Box
    [#orientation := OrientationVertical]
    [ BoxChild defaultBoxChildProperties {expand = True, padding = 10} $
        container
          Box
          [#orientation := OrientationHorizontal]
          [BoxChild defaultBoxChildProperties {expand = True, padding = 10} w]
    ]
