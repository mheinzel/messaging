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
import Messaging.Shared.Request (Request (..), serialize)
import Messaging.Shared.Request.Message (Message (..))
import Messaging.Shared.User (mkUserName, userNameText)
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
  msg <- WS.receiveData conn
  T.putStrLn msg

sendThread :: WS.Connection -> IO ()
sendThread conn = do
  -- Read from stdin and write to WS
  line <- T.getLine
  unless (T.null line) $ do
    let request = serialize $ SendMessage $ Message line
    WS.sendTextData conn request
    sendThread conn
