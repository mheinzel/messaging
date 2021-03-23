{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Messaging.Client.GTK.UI.MessageBox where

import Control.Monad ((<=<))
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

newtype MessageBoxEvent = ScrolledToBottom Bool

data MessageBoxProps = MessageBoxProps {
    messages :: Vector Text,
    stickToBottom :: Bool
  } deriving (Show, Eq)

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

      return (window, msgBox)

    customPatch :: MessageBoxProps -> MessageBoxProps -> ListBox -> CustomPatch ScrolledWindow ListBox
    customPatch old new msgBox
      | old == new = CustomKeep
      | Just newMsgs <- getNew (messages old) (messages new) = CustomModify $ \window -> do
        mapM_ (Gtk.containerAdd msgBox <=< toListRow) newMsgs
        setSticky window (stickToBottom new)
        return msgBox
      | otherwise = CustomModify $ \window -> do
        setSticky window True
        return msgBox

    customSubscribe :: MessageBoxProps -> ListBox -> ScrolledWindow -> (MessageBoxEvent -> IO ()) -> IO Subscription
    customSubscribe _ _ scrollWindow callback = do
      vAdjustment <- #getVadjustment scrollWindow

      handler <-
        Gtk.on vAdjustment #valueChanged $
          callback . ScrolledToBottom =<< do
            value <- #getValue vAdjustment
            upper <- #getUpper vAdjustment
            height <- #getAllocatedHeight scrollWindow 
            return $ fromIntegral height == (upper - value)

      return (fromCancellation (GI.signalHandlerDisconnect vAdjustment handler))

toListRow :: Text -> IO Gtk.ListBoxRow
toListRow msg = do
  listBoxRow <- Gtk.new Gtk.ListBoxRow []
  lbl <- Gtk.new Gtk.Label [#label Gtk.:= msg]
  Gtk.containerAdd listBoxRow lbl
  Gtk.widgetShow listBoxRow
  Gtk.widgetShow lbl
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
  #clampPage vAdjustment upper (upper + 10)