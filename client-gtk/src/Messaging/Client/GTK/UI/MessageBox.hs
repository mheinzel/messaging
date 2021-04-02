{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}

module Messaging.Client.GTK.UI.MessageBox where

import Control.Monad ((<=<))
import Data.Foldable (fold)
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import qualified GI.GObject as GI
import GI.Gtk
  ( Align (..),
    ListBox (..),
    ScrolledWindow (..),
  )
import qualified GI.Gtk as Gtk
import GI.Gtk.Declarative
import GI.Gtk.Declarative.EventSource (Subscription, fromCancellation)

--
-- Custom widget
--

newtype MessageBoxEvent
  = ScrolledToBottom Bool

data MessageBoxState = MessageBoxState
  { messages :: Vector Text,
    stickToBottom :: Bool
  }
  deriving (Show, Eq)

update :: MessageBoxState -> MessageBoxEvent -> MessageBoxState
update st event = undefined

messageBox :: Vector (Attribute Gtk.ScrolledWindow MessageBoxEvent) -> MessageBoxState -> Widget MessageBoxEvent
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

    customCreate :: MessageBoxState -> IO (ScrolledWindow, ListBox)
    customCreate props = do
      window <- Gtk.new Gtk.ScrolledWindow [#propagateNaturalHeight Gtk.:= True]
      msgBox <- Gtk.new Gtk.ListBox [#valign Gtk.:= AlignEnd]
      Gtk.containerAdd window msgBox

      listRows <- mapM toListRow $ messages props
      mapM_ (Gtk.containerAdd msgBox) listRows

      return (window, msgBox)

    customPatch :: MessageBoxState -> MessageBoxState -> ListBox -> CustomPatch ScrolledWindow ListBox
    customPatch old new msgBox
      | Just newMsgs <- getNew (messages old) (messages new) = CustomModify $ \_ -> do
        mapM_ (Gtk.containerAdd msgBox <=< toListRow) newMsgs
        return msgBox
      | otherwise = CustomKeep

    customSubscribe :: MessageBoxState -> ListBox -> ScrolledWindow -> (MessageBoxEvent -> IO ()) -> IO Subscription
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
  Gtk.widgetShow listBoxRow
  Gtk.widgetShow label
  return listBoxRow

getNew :: (Eq a) => Vector a -> Vector a -> Maybe (Vector a)
getNew old new
  | Vec.null old = Just new
  | Vec.null new = Nothing
  | Vec.head old == Vec.head new = getNew (Vec.tail old) (Vec.tail new)
  | otherwise = error "This case should not occur?"

setSticky :: ScrolledWindow -> Bool -> IO ()
setSticky _ False = pure ()
setSticky window _ = do
  vAdjustment <- #getVadjustment window
  upper <- #getUpper vAdjustment
  #clampPage vAdjustment upper upper
