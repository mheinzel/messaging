{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Messaging.Client.Web.Update where

import Data.Bifunctor (bimap)
import qualified Messaging.Client.Core.State as Core
import Messaging.Client.Web.State
import qualified Messaging.Shared.Auth as Auth
import qualified Messaging.Shared.Conversation as Conv
import qualified Messaging.Shared.Message as Msg
import qualified Messaging.Shared.Request as Req
import qualified Messaging.Shared.Response as Res
import qualified Messaging.Shared.User as User
import Miso ((<#))
import qualified Miso
import Miso.String (MisoString, fromMisoString, toMisoString)

data Action
  = LoginAction LoginAction
  | ChatAction ChatAction
  | HandleWebsocket (Miso.WebSocket Res.Response)
  | NoOp

data LoginAction
  = UpdateBackendUrl MisoString
  | UpdateUserName MisoString
  | StartLogin

data ChatAction
  = SendMessage MisoString
  | UpdateEditor MisoString
  | UpdateNewConversation MisoString
  | JoinConversation
  | LeaveConversation Conv.ConversationName
  | SwitchConversation Conv.ConversationName

updateModel :: Model -> Action -> Miso.Effect Action Model
updateModel model@(LoggingIn login) = \case
  LoginAction action ->
    -- We can end up in either of these two states here.
    either LoggingIn WaitingForAuth
      <$> updateLogin login action
  _ ->
    Miso.noEff model
updateModel model@(WaitingForAuth waiting) = \case
  HandleWebsocket Miso.WebSocketOpen ->
    Miso.noEff . Chatting $
      initialChat (validUserName waiting)
  HandleWebsocket ws@(Miso.WebSocketMessage _) -> do
    -- We must have missed the SocketOpen, apply this message to fresh chat.
    let chat = initialChat (validUserName waiting)
    Chatting <$> updateChatWebSocket chat ws
  HandleWebsocket (Miso.WebSocketError err) ->
    Miso.noEff . LoggingIn $
      initialLogin {errors = ["socket error: " <> err]}
  HandleWebsocket ws@Miso.WebSocketClose {} ->
    Miso.noEff . LoggingIn $
      initialLogin {errors = ["socket closed: " <> toMisoString (show ws)]}
  _ ->
    Miso.noEff model
updateModel model@(Chatting chat) = \case
  ChatAction action ->
    -- Always stays in Chatting state.
    bimap (const NoOp) Chatting $ updateChat chat action
  HandleWebsocket ws ->
    -- Always stays in Chatting state.
    Chatting <$> updateChatWebSocket chat ws
  _ ->
    Miso.noEff model

updateLogin :: Login -> LoginAction -> Miso.Effect Action (Either Login Waiting)
updateLogin login = \case
  UpdateBackendUrl txt ->
    Miso.noEff $ Left $ login {backendUrl = txt}
  UpdateUserName txt ->
    Miso.noEff $ Left $ login {userName = txt}
  StartLogin ->
    case User.mkUserName (fromMisoString (userName login)) of
      Nothing ->
        Miso.noEff $ Left login
      Just name -> do
        let baseUrl = fromMisoString (backendUrl login)
        let url = Miso.URL $ toMisoString $ Auth.buildPath baseUrl name
        Miso.effectSub (Right (initialWaiting name)) $
          Miso.websocketSub url (Miso.Protocols []) HandleWebsocket

updateChat :: Chat -> ChatAction -> Miso.Effect () Chat
updateChat chat = \case
  SendMessage txt ->
    case (socketOpen chat, currentConversationName chat) of
      (True, Just conv) ->
        chat {editor = mempty} <# do
          Miso.send $ Req.SendMessage $ Msg.Message conv (fromMisoString txt)
      _ ->
        -- No focused conversation or socket closed, can't send.
        -- (we might want to raise an error here)
        Miso.noEff chat
  UpdateEditor txt ->
    Miso.noEff $ chat {editor = txt}
  UpdateNewConversation txt ->
    Miso.noEff $ chat {newConversation = txt}
  JoinConversation ->
    case Conv.mkConversationName (fromMisoString $ newConversation chat) of
      Nothing -> Miso.noEff chat
      Just conv ->
        (focusConversation conv chat) {newConversation = mempty} <# do
          Miso.send $ Req.JoinConversation conv
  LeaveConversation conv ->
    chat {newConversation = mempty} <# do
      Miso.send $ Req.LeaveConversation conv
  SwitchConversation conv ->
    Miso.noEff $ focusConversation conv chat

updateChatWebSocket :: Chat -> Miso.WebSocket Res.Response -> Miso.Effect a Chat
updateChatWebSocket chat = \case
  Miso.WebSocketMessage res ->
    Miso.noEff $ chat {coreState = Core.handleServerResponse res (coreState chat)}
  Miso.WebSocketOpen ->
    Miso.noEff $ chat {socketOpen = True}
  -- TODO: somehow surface these issues to the user
  Miso.WebSocketClose _code _clean _reason ->
    Miso.noEff $ chat {socketOpen = False}
  Miso.WebSocketError _ ->
    Miso.noEff chat
