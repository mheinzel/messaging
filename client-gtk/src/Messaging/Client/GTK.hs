{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Messaging.Client.GTK where

import Control.Concurrent (Chan, forkIO)
import qualified Control.Concurrent as Ch
import Control.Monad (forever, void)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text (encodeUtf8)
import GI.Gtk.Declarative.App.Simple
import qualified Messaging.Client.Core.Connection as Conn
import qualified Messaging.Client.GTK.UI as UI
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

  incoming <- Conn.spawnRecvThread conn
  outgoing <- Conn.spawnSendThread conn

  runUI incoming outgoing
  -- We might want to also explicitly leave our conversations here.
  WS.sendClose conn ("Bye!" :: Text)

-- as is, takes input and crashes when m is pressed
runUI :: Chan Res.Response -> Chan Req.Request -> IO ()
runUI incoming outgoing = do
  incomingEvents <- mapChan UI.Inbound incoming

  void $
    run
      App
        { view = UI.view,
          update = UI.update outgoing,
          inputs = incomingEvents,
          initialState = UI.initialState
        }

-- Verify if this is desirable
mapChan :: (a -> b) -> Chan a -> IO (Chan b)
mapChan f a = do
  b <- Ch.newChan
  _ <- forkIO $ forever $ Ch.readChan a >>= Ch.writeChan b . f
  return b
