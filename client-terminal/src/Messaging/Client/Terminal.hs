{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Messaging.Client.Terminal where

import qualified Brick.BChan as Brick
import qualified Brick.Main as Brick
import Control.Concurrent (Chan, forkIO)
import Control.Monad (void)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text (encodeUtf8)
import qualified Graphics.Vty as Vty
import qualified Messaging.Client.Core.Connection as Conn
import qualified Messaging.Client.Terminal.UI as UI
import qualified Messaging.Shared.Request as Req
import qualified Messaging.Shared.Response as Res
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

  -- Can't use a normal Chan for brick, so we manually create a BChan
  incoming <- Brick.newBChan 100
  _threadId <- forkIO $ Conn.recvThread conn (Brick.writeBChan incoming)

  outgoing <- Conn.spawnSendThread conn

  runUI incoming outgoing

runUI :: Brick.BChan Res.Response -> Chan Req.Request -> IO ()
runUI incoming outgoing = do
  let vtyBuilder = Vty.mkVty Vty.defaultConfig
  initialVty <- vtyBuilder
  void $
    Brick.customMain
      initialVty
      vtyBuilder
      (Just incoming)
      (UI.clientUI outgoing)
      UI.initialState
