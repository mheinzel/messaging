module Messaging.Server where

import           Control.Monad       (forever)
import qualified Data.Text.IO        as T
import qualified Network.WebSockets  as WS

runApp :: IO ()
runApp = do
  -- TODO: read from command line args
  WS.runServer "127.0.0.1" 8080 app

app :: WS.ServerApp
app pending = do
    conn <- WS.acceptRequest pending
    WS.withPingThread conn 30 (return ()) $ do
      putStrLn "Connected!"
      forever $ do
          msg <- WS.receiveData conn
          T.putStrLn msg
          WS.sendTextData conn msg
