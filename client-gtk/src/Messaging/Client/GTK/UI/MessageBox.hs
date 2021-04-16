{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}

module Messaging.Client.GTK.UI.MessageBox where

import Data.Foldable (fold)
import Data.Text (Text)
import Data.Vector (Vector)
import qualified GI.GObject as GI
import GI.Gtk
  ( Align (..),
    ListBox (..),
    ScrolledWindow (..),
  )
import qualified GI.Gtk as Gtk
import GI.Gtk.Declarative
import GI.Gtk.Declarative.EventSource (Subscription, fromCancellation)

newtype MessageBoxEvent = ScrolledToBottom Bool

data MessageBoxProps = MessageBoxProps
  { messages :: Vector Text,
    stickToBottom :: Bool
  }
  deriving (Show, Eq)

-- | The 'messageBox' function can be use to create a custom message box widget,
-- which functions as a container for a list of messages. The widget is
-- scrollable and has custom behaviour defined such that the widget sticks to
-- the bottom if a new message is added to the list when the viewport is
-- scrolled all the way down.
--
-- The widget emits MessageBoxEvent events and the MessageBoxProps represents
-- the state of the message box, which holds the messages and whether the
-- message box should stick to the bottom of the viewport.
messageBox :: Vector (Attribute Gtk.ScrolledWindow MessageBoxEvent) -> MessageBoxProps -> Widget MessageBoxEvent
messageBox customAttributes customParams =
  Widget $
    CustomWidget
      { customWidget,
        customCreate,
        customPatch,
        customSubscribe,
        customAttributes,
        customParams
      }
  where
    customWidget = Gtk.ScrolledWindow

    customCreate :: MessageBoxProps -> IO (ScrolledWindow, ListBox)
    customCreate props = do
      window <- Gtk.new Gtk.ScrolledWindow [#propagateNaturalHeight Gtk.:= True]
      msgBox <- Gtk.new Gtk.ListBox [#valign Gtk.:= AlignEnd]
      Gtk.containerAdd window msgBox

      listRows <- mapM toListRow $ messages props
      mapM_ (Gtk.containerAdd msgBox) listRows

      Gtk.widgetShowAll window
      return (window, msgBox)

    -- The 'customPatch' function defines the update behaviour of widgets.
    -- Currently it is lacking and recreates all the messages on an update.
    -- This is because GTK derives which old and new widget states belong
    -- together according to their order within the structure of the UI,
    -- and mismatches of the states currently seem unavoidable.
    customPatch :: MessageBoxProps -> MessageBoxProps -> ListBox -> CustomPatch ScrolledWindow ListBox
    customPatch old new msgBox
      | new == old = CustomKeep
      | otherwise = CustomModify $ \window -> do
        oldRows <- Gtk.containerGetChildren msgBox
        mapM_ Gtk.widgetDestroy oldRows

        listRows <- mapM toListRow $ messages new
        mapM_ (Gtk.containerAdd msgBox) listRows

        Gtk.widgetShowAll window
        return msgBox

    customSubscribe :: MessageBoxProps -> ListBox -> ScrolledWindow -> (MessageBoxEvent -> IO ()) -> IO Subscription
    customSubscribe props _ scrollWindow callback = do
      vAdjustment <- #getVadjustment scrollWindow

      fold
        [ fromCancellation . GI.signalHandlerDisconnect vAdjustment <$> do
            Gtk.on vAdjustment #valueChanged $ do
              value <- #getValue vAdjustment
              upper <- #getUpper vAdjustment
              height <- #getAllocatedHeight scrollWindow
              callback $ ScrolledToBottom $ fromIntegral height == (upper - value),
          fromCancellation . GI.signalHandlerDisconnect scrollWindow <$> do
            Gtk.after scrollWindow #sizeAllocate $ \_ ->
              -- Do not send an event, just stick to the bottom if necessary.
              setSticky scrollWindow (stickToBottom props)
        ]

toListRow :: Text -> IO Gtk.ListBoxRow
toListRow msg = do
  listBoxRow <- Gtk.new Gtk.ListBoxRow []
  label <- Gtk.new Gtk.Label [#label Gtk.:= msg]
  Gtk.setLabelWrap label True
  #setXalign label 0.0
  Gtk.containerAdd listBoxRow label
  return listBoxRow

setSticky :: ScrolledWindow -> Bool -> IO ()
setSticky _ False = pure ()
setSticky window _ = do
  vAdjustment <- #getVadjustment window
  upper <- #getUpper vAdjustment
  #clampPage vAdjustment upper upper
