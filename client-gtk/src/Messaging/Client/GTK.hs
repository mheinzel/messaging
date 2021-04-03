{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Messaging.Client.GTK where

import qualified Data.Text as Text
import qualified Messaging.Client.Core.Connection as Conn
import qualified Messaging.Client.GTK.UI as UI
import qualified Messaging.Shared.User as User
import qualified Network.WebSockets as WS
import qualified System.Environment as Env
import qualified System.Exit as Exit

runClient :: IO ()
runClient = do
  -- TODO: proper command line argument parser, also read URI.
  -- Or even allow entering this information in some GUI widget.
  userName <- do
    progName <- Env.getProgName
    Env.getArgs >>= \case
      [name] -> case User.mkUserName (Text.pack name) of
        Just userName -> pure userName
        Nothing -> Exit.die "error: invalid user name"
      _ -> Exit.die $ "usage: " <> progName <> " USERNAME"

  let uri = Conn.defaultURI

  Conn.runClientApp uri userName (client userName)

client :: User.UserName -> WS.ClientApp ()
client userName conn = do
  putStrLn "Connected!"
  Conn.withConnectionThreads conn (UI.runUI userName)
