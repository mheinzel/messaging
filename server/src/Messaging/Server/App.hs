{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData #-}

module Messaging.Server.App where

import Control.Concurrent.STM (STM, atomically)
import Control.Monad.Logger (LoggingT, MonadLogger)
import Control.Monad.Reader.Class (MonadReader (ask))
import Control.Monad.Trans (MonadIO (liftIO))
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import qualified Messaging.Server.Log as Log
import Messaging.Server.State (State)
import UnliftIO (MonadUnliftIO)

newtype App a = App {unApp :: ReaderT State (LoggingT IO) a}
  deriving newtype (Functor, Applicative, Monad)
  deriving newtype (MonadReader State, MonadLogger, MonadIO, MonadUnliftIO)

runApp :: Settings -> State -> App a -> IO a
runApp settings state =
  Log.runLoggingT (logSettings settings) . flip runReaderT state . unApp

data Settings = Settings
  { serverPort :: Int,
    logSettings :: Log.Settings
  }

runAtomically :: (State -> STM a) -> App a
runAtomically stm = do
  state <- ask
  liftIO $ atomically $ stm state
