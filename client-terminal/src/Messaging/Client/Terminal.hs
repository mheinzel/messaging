{-# LANGUAGE OverloadedStrings #-}

module Messaging.Client.Terminal where

import qualified Messaging.Client.Core.Connection as Conn
import qualified Messaging.Client.Terminal.UI as UI
import Messaging.Shared.User (UserName, mkUserName)
import qualified Network.WebSockets as WS
import qualified System.Exit as Exit
import qualified Messaging.Client.Core.Parser as Par

runClient :: IO ()
runClient = do
  -- TODO: proper command line argument parser, also read URI
  input <- Par.runParse
  userName <- case mkUserName (Par._username input) of
        Just userName -> pure userName
        Nothing -> Exit.die "error: invalid user name"

  Conn.runClientApp (Par._uri input) userName (client userName)

client :: UserName -> WS.ClientApp ()
client user conn = do
  putStrLn "Connected!"
  Conn.withConnectionThreads conn (UI.runUI user)
