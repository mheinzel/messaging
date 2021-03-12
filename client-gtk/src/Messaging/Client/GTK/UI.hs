{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Messaging.Client.GTK.UI where

import Control.Concurrent.Chan (Chan)
import GI.Gdk (EventKey, getEventKeyString)
import GI.Gtk
  ( Box (..),
    Entry (..),
    Orientation (..),
    Window (..),
  )
import GI.Gtk.Declarative
import GI.Gtk.Declarative.App.Simple

data State = Initial

data Event
  = Closed
  | Ignore

-- | The update function of an application reduces the current state and
-- a new event to a 'Transition', which decides if and how to transition
-- to the next state.
update :: State -> Event -> Transition State Event
update _ Closed = Exit
update s _ = Transition s $ pure Nothing

-- | The view renders a state value as a window, parameterized by the
-- 'App's event type.
view :: State -> AppView Window Event
view s =
  bin
    Window
    [ #title := "Title",
      on #deleteEvent (const (True, Closed)),
      #widthRequest := 400,
      #heightRequest := 300
    ]
    $ container
      Box
      [#orientation := OrientationVertical]
      [widget Entry [onM #keyPressEvent windowKeyPressEventHandler]]

windowKeyPressEventHandler :: EventKey -> Entry -> IO (Bool, Event)
windowKeyPressEventHandler eventKey _ = do
  key <- getEventKeyString eventKey
  case key of
    Just "m" -> return (True, Closed)
    _ -> return (False, Ignore)

-- | Inputs come through a channel that feed events into the application.
inputs :: Chan Event
inputs = undefined

-- | The initial state value of the state reduction loop.
initialState :: State
initialState = Initial
