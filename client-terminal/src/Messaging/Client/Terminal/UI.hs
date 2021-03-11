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
import Control.Concurrent (Chan, writeChan)
import Control.Monad.Trans (liftIO)
import Data.Foldable (fold)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Graphics.Vty as V
import Lens.Micro
import Lens.Micro.TH
import qualified Messaging.Shared.Conversation as Conv
import qualified Messaging.Shared.Message as Msg
import qualified Messaging.Shared.Request as Req
import qualified Messaging.Shared.Response as Res
import qualified Messaging.Shared.User as User

data Name
  = History
  | Edit
  deriving (Ord, Show, Eq)

data St = St
  { _focusRing :: F.FocusRing Name,
    _history :: List.List Name Res.Response,
    _edit :: E.Editor Text Name
  }

makeLenses ''St

clientUI :: Chan Req.Request -> M.App St Res.Response Name
clientUI outgoingChan =
  M.App
    { M.appDraw = drawUI,
      M.appChooseCursor = appCursor,
      M.appHandleEvent = appEvent outgoingChan,
      M.appStartEvent = return,
      M.appAttrMap = const theMap
    }

drawUI :: St -> [T.Widget Name]
drawUI st = [ui]
  where
    editor = F.withFocusRing (st ^. focusRing) (E.renderEditor (txt . Text.unlines)) (st ^. edit)

    ui =
      vBox
        [ F.withFocusRing (st ^. focusRing) (List.renderList renderRequest) (st ^. history),
          vLimit 1 editor
        ]

renderRequest :: Bool -> Res.Response -> T.Widget n
renderRequest _ (Res.ReceivedMessage user msg) =
  txt $ User.userNameText (User.userName user) <> ": " <> Msg.messageContent msg
renderRequest _ (Res.JoinedConversation user _conv) =
  txt $ User.userNameText (User.userName user) <> " JOINED"
renderRequest _ (Res.LeftConversation user _conv) =
  txt $ User.userNameText (User.userName user) <> " LEFT"

appEvent :: Chan Req.Request -> St -> T.BrickEvent Name Res.Response -> T.EventM Name (T.Next St)
-- keyboard inputs
appEvent outgoingChan st (T.VtyEvent ev) =
  case ev of
    V.EvKey V.KEsc [] -> M.halt st
    V.EvKey (V.KChar '\t') [] -> M.continue $ st & focusRing %~ F.focusNext
    V.EvKey V.KBackTab [] -> M.continue $ st & focusRing %~ F.focusPrev
    V.EvKey V.KEnter [] -> do
      let msgText = fold (E.getEditContents (st ^. edit))
          msg = Msg.Message Conv.conversationNameGeneral msgText
      liftIO $ writeChan outgoingChan $ Req.SendMessage msg
      -- reset text field
      M.continue $ st & edit .~ emptyEditor
    _ ->
      M.continue =<< case F.focusGetCurrent (st ^. focusRing) of
        Just History -> return st
        Just Edit -> T.handleEventLensed st edit E.handleEditorEvent ev
        Nothing -> return st
-- incoming messages
appEvent _ st (T.AppEvent res) =
  M.continue $
    st
      & history %~ (\l -> List.listInsert (length l) res l)
      & history %~ List.listMoveTo (-1)
appEvent _ st _ =
  M.continue st

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
