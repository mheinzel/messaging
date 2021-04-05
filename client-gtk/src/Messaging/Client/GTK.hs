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
import qualified Messaging.Shared.User as User
import qualified Messaging.Client.Core.Parser as Par

runClient :: IO ()
runClient = do
  input <- Par.runParse
  userName <- case mkUserName (Par._username input) of
        Just userName -> pure userName
        Nothing -> Exit.die "error: invalid user name"
  uri <- pure $ Par._uri input
  -- TODO: proper command line argument parser, also read URI.
  -- Or even allow entering this information in some GUI widget.


  Conn.runClientApp uri userName (client userName)

client :: User.UserName -> WS.ClientApp ()
client userName conn = do
  putStrLn "Connected!"
  Conn.withConnectionThreads conn (UI.runUI userName)
