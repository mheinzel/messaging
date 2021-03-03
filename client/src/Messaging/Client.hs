{-# LANGUAGE OverloadedStrings #-}

module Messaging.Client where

import Control.Concurrent (forkIO)
import Control.Monad (forever, unless)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Network.Socket (withSocketsDo)
import qualified Network.WebSockets as WS

runClient :: IO ()
runClient = do
  -- TODO: read from command line args
  withSocketsDo $ WS.runClient "127.0.0.1" 8080 "/" client

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
    WS.sendTextData conn line
    sendThread conn
