{-# LANGUAGE OverloadedStrings #-}

module Messaging.Client.GTK where

import qualified Messaging.Client.Core.Connection as Conn
import qualified Messaging.Client.Core.Parser as Par
import qualified Messaging.Client.GTK.UI as UI
import qualified Messaging.Shared.User as User
import qualified Network.WebSockets as WS

runClient :: IO ()
runClient = do
  input <- Par.runParse
  let userName = Par._username input

  Conn.runClientApp (Par._uri input) userName (client userName)

client :: User.UserName -> WS.ClientApp ()
client userName conn = do
  putStrLn "Connected!"
  Conn.withConnectionThreads conn (UI.runUI userName)
