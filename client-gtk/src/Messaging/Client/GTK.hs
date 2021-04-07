{-# LANGUAGE OverloadedStrings #-}

module Messaging.Client.GTK where

import qualified Messaging.Client.Core.Connection as Conn
import qualified Messaging.Client.GTK.UI as UI
import qualified Network.WebSockets as WS
import qualified System.Exit as Exit
import qualified Messaging.Shared.User as User
import qualified Messaging.Client.Core.Parser as Par

runClient :: IO ()
runClient = do
  input <- Par.runParse
  userName <- case User.mkUserName (Par._username input) of
        Just userName -> pure userName
        Nothing -> Exit.die "error: invalid user name"
  
  Conn.runClientApp (Par._uri input) userName (client userName)

client :: User.UserName -> WS.ClientApp ()
client userName conn = do
  putStrLn "Connected!"
  Conn.withConnectionThreads conn (UI.runUI userName)
