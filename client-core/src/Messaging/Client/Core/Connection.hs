{-# LANGUAGE OverloadedStrings #-}

module Messaging.Client.Core.Connection where

import Control.Concurrent (Chan, forkIO, newChan, readChan, writeChan)
import Control.Monad (forever)
import qualified Messaging.Shared.Request as Req
import qualified Messaging.Shared.Response as Res
import qualified Network.WebSockets as WS

spawnSendThread :: WS.Connection -> IO (Chan Req.Request)
spawnSendThread conn = do
  outgoing <- newChan
  _ <- forkIO $ sendThread conn (readChan outgoing)
  pure outgoing

sendThread :: WS.Connection -> IO Req.Request -> IO ()
sendThread conn getRequest = forever $ do
  req <- getRequest
  WS.sendTextData conn (Req.serialize req)

spawnRecvThread :: WS.Connection -> IO (Chan Res.Response)
spawnRecvThread conn = do
  incoming <- newChan
  _ <- forkIO $ recvThread conn (writeChan incoming)
  pure incoming

recvThread :: WS.Connection -> (Res.Response -> IO ()) -> IO ()
recvThread conn putResponse = forever $ do
  received <- WS.receiveData conn
  case Res.deserialize received of
    Left err -> printError err
    Right res -> putResponse res
  where
    printError :: Res.DeserializeError -> IO ()
    printError err =
      putStrLn $ "failed to deserialize message: " <> Res.errorMessage err
