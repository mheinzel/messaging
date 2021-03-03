module Messaging.Server where

import Control.Monad (forever)
import Control.Monad.Trans (MonadIO (liftIO))
import Data.Foldable (traverse_)
import qualified Data.Text.IO as T
import Messaging.Server.State (State, addConnection, getConnections, initialState, runApp)
import qualified Network.WebSockets as WS

runServer :: IO ()
runServer = do
  -- TODO: read from command line args
  state <- initialState
  WS.runServer "127.0.0.1" 8080 (app state)

app :: State -> WS.ServerApp
app state pending = do
  conn <- WS.acceptRequest pending
  WS.withPingThread conn 30 (return ()) $
    runApp state $ do
      addConnection conn
      liftIO $ putStrLn "Connected!"
      -- TODO: clean up closed connections
      forever $ do
        msg <- liftIO $ WS.receiveData conn
        liftIO $ T.putStrLn msg
        conns <- getConnections
        liftIO $ traverse_ (flip WS.sendTextData msg) conns
