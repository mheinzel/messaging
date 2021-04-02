{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}

module Messaging.Client.GTK.UI.ConversationBar where

import Data.Text (Text, pack)
import Data.Vector (Vector)
import qualified GI.Gtk as Gtk
import GI.Gtk.Declarative
import GI.Gtk.Declarative.EventSource (Subscription, fromCancellation)
import Messaging.Shared.Conversation

data ConversationBarEvent
  = FocusConversation ConversationName
  | LeaveConversation ConversationName

data ConversationBarProps = ConversationBarProps
  { conversations :: Vector ConversationName,
    focusedConversation :: ConversationName -- could also be an index
  }

conversationBar :: Vector (Attribute Gtk.MenuBar ConversationBarEvent) -> ConversationBarProps -> Widget ConversationBarEvent
conversationBar customAttributes customParams =
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
    customWidget = Gtk.MenuBar

    customCreate props = do
      menu <- Gtk.menuBarNew
      items <- mapM mkMenuItem $ conversations props
      mapM_ (Gtk.containerAdd menu) items
      return (menu, items)
    
    customPatch old new items
      | old = new = CustomKeep
      | otherwise = 
    

mkMenuItem :: ConversationName -> IO Gtk.MenuItem
mkMenuItem name = do
  item <- Gtk.menuItemNew
  box <- Gtk.new Gtk.Box [#orientation Gtk.:= Gtk.OrientationHorizontal]
  Gtk.containerAdd item box

  lbl <- Gtk.new Gtk.Label [#label Gtk.:= conversationNameText name]
  #packStart box lbl True True 0

  button <- Gtk.new Gtk.Button [#label Gtk.:= pack "X"]
  #packStart box button True True 0

  return item
