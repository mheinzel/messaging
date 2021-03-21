{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Messaging.Client.GTK.UI.MessageBox where

import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import Debug.Trace (trace)
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

newtype MessageBoxEvent = SetSticky Bool

messageBox :: Vector (Attribute Gtk.ScrolledWindow MessageBoxEvent) -> Vector Text -> Widget MessageBoxEvent
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

    customCreate :: Vector Text -> IO (ScrolledWindow, ListBox)
    customCreate msgs = do
      window <- Gtk.new Gtk.ScrolledWindow []
      msgBox <- Gtk.new Gtk.ListBox [#valign Gtk.:= AlignEnd]
      Gtk.containerAdd window msgBox

      listRows <- mapM toListRow msgs
      mapM_ (\row -> Gtk.listBoxInsert msgBox row (-1)) listRows 

      return (window, msgBox)

    customPatch :: Vector Text -> Vector Text -> ListBox -> CustomPatch ScrolledWindow ListBox
    customPatch old new msgBox
      | old == new = CustomReplace
      --      | (Just newMsgs) <- getNew old new = CustomModify $ \_ -> do
      --        listRows <- mapM toListRow newMsgs
      --        mapM_ (\row -> #insert msgBox row (-1)) listRows
      --        return msgBox
      | otherwise = CustomReplace

    customSubscribe :: Vector Text -> ListBox -> ScrolledWindow -> (MessageBoxEvent -> IO ()) -> IO Subscription
    customSubscribe _ _ scrollWindow cb = do
      vAdjustment <- Gtk.scrolledWindowGetVadjustment scrollWindow
      h <-
        Gtk.on vAdjustment #valueChanged $
          cb . SetSticky =<< do
            value <- Gtk.adjustmentGetValue vAdjustment
            upper <- Gtk.adjustmentGetUpper vAdjustment
            trace "VAdjustment changed!" $
              return $ value == upper

      return (fromCancellation (GI.signalHandlerDisconnect vAdjustment h))

toListRow :: Text -> IO Gtk.ListBoxRow
toListRow msg = do
  listBoxRow <- Gtk.new Gtk.ListBoxRow []
  lbl <- Gtk.new Gtk.Label [#label Gtk.:= msg]
  Gtk.containerAdd listBoxRow lbl
  return listBoxRow

getNew :: (Eq a) => Vector a -> Vector a -> Maybe (Vector a)
getNew old new
  | Vec.null old = Just new
  | Vec.null new = Nothing
  | Vec.head old == Vec.head new = getNew (Vec.tail old) (Vec.tail new)
  | otherwise = error "This case should not occur?"
