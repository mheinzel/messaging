{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Messaging.Client.Terminal.UI where

import qualified Brick.AttrMap as A
import qualified Brick.Focus as F
import qualified Brick.Main as M
import qualified Brick.Types as T
import Brick.Util (on)
import Brick.Widgets.Core (txt, vBox, vLimit)
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.List as List
import Data.Foldable (fold)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Graphics.Vty as V
import Lens.Micro
import Lens.Micro.TH
import Messaging.Shared.Conversation as Conv
import Messaging.Shared.Message (Message)
import qualified Messaging.Shared.Message as Msg

data Name
  = History
  | Edit
  deriving (Ord, Show, Eq)

data St = St
  { _focusRing :: F.FocusRing Name,
    _history :: List.List Name Message,
    _edit :: E.Editor Text Name
  }

makeLenses ''St

theApp :: M.App St e Name
theApp =
  M.App
    { M.appDraw = drawUI,
      M.appChooseCursor = appCursor,
      M.appHandleEvent = appEvent,
      M.appStartEvent = return,
      M.appAttrMap = const theMap
    }

drawUI :: St -> [T.Widget Name]
drawUI st = [ui]
  where
    editor = F.withFocusRing (st ^. focusRing) (E.renderEditor (txt . Text.unlines)) (st ^. edit)

    ui =
      vBox $
        [ F.withFocusRing (st ^. focusRing) (List.renderList drawMessage) (st ^. history),
          vLimit 5 editor
        ]

drawMessage :: Bool -> Message -> T.Widget n
drawMessage _ = txt . Msg.messageContent

appEvent :: St -> T.BrickEvent Name e -> T.EventM Name (T.Next St)
appEvent st (T.VtyEvent ev) =
  case ev of
    V.EvKey V.KEsc [] -> M.halt st
    V.EvKey (V.KChar '\t') [] -> M.continue $ st & focusRing %~ F.focusNext
    V.EvKey V.KBackTab [] -> M.continue $ st & focusRing %~ F.focusPrev
    V.EvKey V.KEnter [] -> do
      let msg = Msg.Message Conv.conversationNameGeneral (fold (E.getEditContents (st ^. edit)))
          moveToEnd = List.listMoveTo (-1)
          appendMsg l = List.listInsert (length l) msg l
      M.continue $ st
        & history %~ moveToEnd . appendMsg
        & edit .~ emptyEditor
    _ ->
      M.continue =<< case F.focusGetCurrent (st ^. focusRing) of
        Just History -> return st
        Just Edit -> T.handleEventLensed st edit E.handleEditorEvent ev
        Nothing -> return st
appEvent st _ = M.continue st

initialState :: St
initialState =
  St
    (F.focusRing [Edit, History])
    (List.list History mempty 1)
    emptyEditor

emptyEditor :: E.Editor Text Name
emptyEditor = E.editorText Edit Nothing ""

theMap :: A.AttrMap
theMap =
  A.attrMap
    V.defAttr
    [ (E.editAttr, V.white `on` V.blue),
      (E.editFocusedAttr, V.black `on` V.yellow)
    ]

appCursor :: St -> [T.CursorLocation Name] -> Maybe (T.CursorLocation Name)
appCursor = F.focusRingCursor (^. focusRing)
