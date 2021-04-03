{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Messaging.Server.Log
  ( -- * App internals
    Settings (..),
    LoggingMode (..),
    Log.LogLevel (..),
    runLoggingT,

    -- * Convenience functions independent of underlying implementation
    debug,
    info,
    warn,
    error,
    logExceptions,
  )
where

import qualified Control.Monad.Logger as Log
import Control.Monad.Trans (MonadIO (liftIO))
import qualified Data.ByteString as BS
import qualified Data.Char as Char
import Data.Foldable (fold)
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Stack (HasCallStack, callStack)
import qualified System.IO as IO
import UnliftIO (MonadUnliftIO)
import qualified UnliftIO.Exception as Exc
import Prelude hiding (error)

data Settings = Settings
  { loggingMode :: LoggingMode,
    loggingLevel :: Log.LogLevel
  }

data LoggingMode
  = LoggingDisabled
  | LoggingStdout
  | LoggingStderr

runLoggingT :: MonadIO m => Settings -> Log.LoggingT m a -> m a
runLoggingT settings =
  run . Log.filterLogger (const (>= loggingLevel settings))
  where
    run = case loggingMode settings of
      LoggingDisabled -> flip Log.runLoggingT mempty
      LoggingStdout -> runLoggingToHandle IO.stdout
      LoggingStderr -> runLoggingToHandle IO.stderr

runLoggingToHandle :: MonadIO m => IO.Handle -> Log.LoggingT m a -> m a
runLoggingToHandle handle loggingT = do
  liftIO $ IO.hSetBuffering IO.stdout IO.LineBuffering
  Log.runLoggingT loggingT $ \loc _src level msg -> do
    BS.hPut handle $ Log.fromLogStr $ formatLogStr loc level msg

-- The library defines something like this, but it's just too verbose,
-- printing file, package and module name.
formatLogStr :: Log.Loc -> Log.LogLevel -> Log.LogStr -> Log.LogStr
formatLogStr loc level msg =
  fold
    [ "[",
      Log.toLogStr $ map Char.toUpper $ drop 5 $ show level, -- strip "Level" prefix
      "] ",
      Log.toLogStr $ Log.loc_filename loc,
      ":",
      Log.toLogStr $ show $ fst $ Log.loc_start loc,
      ": ",
      msg,
      "\n"
    ]

-- | To avoid some clunky Text/String conversion.
class Loggable t where
  loggableText :: t -> Text

instance Loggable Text where
  loggableText = id

instance Loggable String where
  loggableText = Text.pack

debug :: (Loggable t, Log.MonadLogger m, HasCallStack) => t -> m ()
debug = Log.logDebugCS callStack . loggableText

info :: (Loggable t, Log.MonadLogger m, HasCallStack) => t -> m ()
info = Log.logInfoCS callStack . loggableText

warn :: (Loggable t, Log.MonadLogger m, HasCallStack) => t -> m ()
warn = Log.logWarnCS callStack . loggableText

error :: (Loggable t, Log.MonadLogger m, HasCallStack) => t -> m ()
error = Log.logErrorCS callStack . loggableText

logExceptions :: (Log.MonadLogger m, MonadUnliftIO m, HasCallStack) => m a -> m a
logExceptions = flip Exc.withException $ \e ->
  warn $ "exception: " <> show (e :: Exc.SomeException)
