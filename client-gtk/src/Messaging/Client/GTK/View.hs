{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Messaging.Client.GTK.View where

import Data.Text as Text
import Data.Vector as Vec
import GI.Gdk (EventKey, getEventKeyString)
import GI.Gtk
  ( Align (..),
    Box (..),
    Button (..),
    Entry (..),
    Label (..),
    Orientation (..),
    Window (..),
    entryGetText,
    entrySetText,
  )
import GI.Gtk.Declarative
import GI.Gtk.Declarative.App.Simple (AppView)
import Lens.Micro ((^.))
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

-- | The 'view' function defines the structure of the UI.
-- We make use of the notebook widget of the gi-gtk-declarative library to
-- display multiple conversations by using tabs.
--
-- The notebook widget is easy to use but lacks some desirable functionality.
-- If we have a lot of conversations, and thus a lot of tabs, the viewport of
-- the UI gets stretched, and no scrolling seems to be possible.
-- Furthermore, GTK has trouble aligning the tab widgets when updating the view.
--
--  Instead of updating the whole UI structure for every updated render of the
--  view, GTK finds the changes and only partially updates the view (see the
--  'customPatch' function in our custom MessageBox widget).
--  However, to achieve these partial updates, GTK needs to align the structure
--  of the old and new version of the widget. In the case of the notebook widget,
--  if we add a new tab in the beginning of the notebook, GTK will pair this
--  new first tab with the first tab of the previous UI view, which for our UI
--  means it mismatches the MessageBox widgets of different conversations,
--  and thus we have to update the MessageBoxes in a less than desirable way.
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

-- | A simple keystroke event handler for our message input widget.
-- If escape is pressed, the client application is closed.
-- If the enter button (\r) is pressed, we send the current contents of the
-- input widget as a messages to the server.
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
