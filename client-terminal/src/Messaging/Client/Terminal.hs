{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Messaging.Client.Terminal where

import qualified Data.Text as Text
import qualified Messaging.Client.Core.Connection as Conn
import qualified Messaging.Client.Terminal.UI as UI
import Messaging.Shared.User (UserName, mkUserName)
import qualified Network.WebSockets as WS
import qualified System.Environment as Env
import qualified System.Exit as Exit

runClient :: IO ()
runClient = do
  -- TODO: proper command line argument parser, also read URI
  userName <- do
    progName <- Env.getProgName
    Env.getArgs >>= \case
      [name] -> case mkUserName (Text.pack name) of
        Just userName -> pure userName
        Nothing -> Exit.die "error: invalid user name"
      _ -> Exit.die $ "usage: " <> progName <> " USERNAME"

  let uri = Conn.defaultURI

  Conn.runClientApp uri userName (client userName)

client :: UserName -> WS.ClientApp ()
client user conn = do
  putStrLn "Connected!"
  Conn.withConnectionThreads conn (UI.runUI user)
