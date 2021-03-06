{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Messaging.Client where

import Control.Concurrent (forkIO)
import Control.Monad (forever, unless)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text (encodeUtf8)
import qualified Data.Text.IO as T
import Messaging.Shared.Conversation (conversationNameGeneral, ConversationName (conversationNameText))
import Messaging.Shared.Message (Message (..))
import qualified Messaging.Shared.Request as Req
import qualified Messaging.Shared.Response as Res
import Messaging.Shared.User (User (User), UserName (userNameText), mkUserName)
import Network.Socket (withSocketsDo)
import qualified Network.WebSockets as WS
import qualified System.Environment as Env
import qualified System.Exit as Exit

runClient :: IO ()
runClient = do
  -- TODO: proper command line argument parser, also read URL and port
  userName <- do
    progName <- Env.getProgName
    Env.getArgs >>= \case
      [name] -> case mkUserName (Text.pack name) of
        Just userName -> pure userName
        Nothing -> Exit.die "error: invalid user name"
      _ -> Exit.die $ "usage: " <> progName <> " USERNAME"

  let options = WS.defaultConnectionOptions
  let headers = [("UserName", Text.encodeUtf8 (userNameText userName))]

  withSocketsDo $
    WS.runClientWith "127.0.0.1" 8080 "/" options headers client

client :: WS.ClientApp ()
client conn = do
  putStrLn "Connected!"

  _threadId <- forkIO $ recvThread conn
  sendThread conn
  WS.sendClose conn ("Bye!" :: Text)

recvThread :: WS.Connection -> IO ()
recvThread conn = forever $ do
  received <- WS.receiveData conn
  case Res.deserialize received of
    Left err ->
      printError err
    Right (Res.ReceivedMessage (User _id sender) (Message conv msg)) ->
      printInConversation conv $ userNameText sender <> ": " <> msg
    Right (Res.JoinedConversation (User _id user) conv) ->
      printInConversation conv $userNameText user <> " JOINED"
    Right (Res.LeftConversation (User _id user) conv) ->
      printInConversation conv $userNameText user <> " LEFT"
  where
    printInConversation :: ConversationName -> Text -> IO ()
    printInConversation conv txt =
      T.putStrLn $ conversationNameText conv <> " | " <> txt

    printError :: Res.DeserializeError -> IO ()
    printError err =
      putStrLn $ "failed to deserialize message: " <> Res.errorMessage err

sendThread :: WS.Connection -> IO ()
sendThread conn = do
  -- Read from stdin and write to WS
  line <- T.getLine
  unless (T.null line) $ do
    -- For now assume everything is happening in there
    let conversation = conversationNameGeneral
    let request = Req.SendMessage $ Message conversation line
    WS.sendTextData conn (Req.serialize request)
    sendThread conn
