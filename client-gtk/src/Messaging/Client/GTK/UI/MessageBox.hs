{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE StrictData #-}

module Messaging.Client.GTK.UI.MessageBox where

import Control.Monad ((<=<))
import Data.Foldable (fold)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
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

newtype MessageBoxProps = MessageBoxProps
  { messages :: Vector Text
  }
  deriving (Show, Eq)

data InternalState = InternalState
  { childListBox :: ListBox,
    stickToBottom :: IORef Bool
  }

messageBox :: Vector (Attribute Gtk.ScrolledWindow e) -> MessageBoxProps -> Widget e
messageBox customAttributes customParams =
  Widget $ customMessageBox customAttributes customParams

customMessageBox ::
  Vector (Attribute Gtk.ScrolledWindow e) ->
  MessageBoxProps ->
  CustomWidget Gtk.ScrolledWindow MessageBoxProps InternalState e
customMessageBox customAttributes customParams =
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

    customCreate :: MessageBoxProps -> IO (ScrolledWindow, InternalState)
    customCreate props = do
      window <- Gtk.new Gtk.ScrolledWindow [#propagateNaturalHeight Gtk.:= True]
      msgBox <- Gtk.new Gtk.ListBox [#valign Gtk.:= AlignEnd]
      Gtk.containerAdd window msgBox

      listRows <- mapM toListRow $ messages props
      mapM_ (Gtk.containerAdd msgBox) listRows

      stick <- newIORef True
      let internalState = InternalState msgBox stick

      return (window, internalState)

    customPatch :: MessageBoxProps -> MessageBoxProps -> InternalState -> CustomPatch ScrolledWindow InternalState
    customPatch old new internal
      | Just newMsgs <- getNew (messages old) (messages new) = CustomModify $ \_ -> do
        mapM_ (Gtk.containerAdd (childListBox internal) <=< toListRow) newMsgs
        return internal
      | otherwise = CustomKeep

    customSubscribe :: MessageBoxProps -> InternalState -> ScrolledWindow -> (e -> IO ()) -> IO Subscription
    customSubscribe _ internal scrollWindow _ = do
      vAdjustment <- #getVadjustment scrollWindow
      -- Do not send events, just update internal state and scroll to bottom if necessary.
      fold
        [ fromCancellation . GI.signalHandlerDisconnect vAdjustment <$> do
            Gtk.on vAdjustment #valueChanged $ do
              value <- #getValue vAdjustment
              upper <- #getUpper vAdjustment
              height <- #getAllocatedHeight scrollWindow
              writeIORef (stickToBottom internal) $ fromIntegral height == (upper - value),
          fromCancellation . GI.signalHandlerDisconnect scrollWindow <$> do
            Gtk.after scrollWindow #sizeAllocate $ \_ ->
              setSticky scrollWindow =<< readIORef (stickToBottom internal)
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
