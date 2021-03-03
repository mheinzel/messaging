{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Messaging.Server.App where

import Control.Concurrent.STM (TVar, atomically, modifyTVar, newTVar, readTVar)
import Control.Monad.Reader.Class (MonadReader, asks)
import Control.Monad.Trans (MonadIO (liftIO))
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import qualified Network.WebSockets as WS

newtype App a = App {unApp :: ReaderT State IO a}
  deriving (Functor, Applicative, Monad, MonadReader State, MonadIO)

runApp :: State -> App a -> IO a
runApp s = flip runReaderT s . unApp

data State = State
  { connections :: TVar [WS.Connection]
  }

initialState :: IO State
initialState = atomically $ do
  State <$> newTVar []

addConnection :: WS.Connection -> App ()
addConnection conn = do
  conns <- asks connections
  liftIO $ atomically $ modifyTVar conns (conn :)

getConnections :: App [WS.Connection]
getConnections = do
  conns <- asks connections
  liftIO $ atomically $ readTVar conns
