{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData #-}

module Messaging.Server.App where

import Control.Concurrent.STM (TVar, newTVarIO)
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.Trans (MonadIO)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Data.Map.Strict (Map)
import Data.Set (Set)
import Messaging.Shared.Conversation (ConversationName)
import Messaging.Shared.User (User, UserID, UserName)
import qualified Network.WebSockets as WS

newtype App a = App {unApp :: ReaderT State IO a}
  deriving newtype (Functor, Applicative, Monad, MonadReader State, MonadIO)

runApp :: State -> App a -> IO a
runApp s = flip runReaderT s . unApp

data State = State
  { activeConversations :: TVar (Map ConversationName Conversation),
    connectedUsers :: TVar (Map UserID WS.Connection),
    users :: TVar (Map UserID User),
    takenUserNames :: TVar (Set UserName)
  }

data Conversation = Conversation
  { conversationName :: ConversationName,
    conversationMembers :: Set UserID
  }
  deriving stock (Show)

initialState :: IO State
initialState =
  State
    <$> newTVarIO mempty
    <*> newTVarIO mempty
    <*> newTVarIO mempty
    <*> newTVarIO mempty
