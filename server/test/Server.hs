module Server where

import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (newChan, readChan, writeChan)
import Control.Exception
import Data.ByteString.Lazy (toChunks)
import Data.Foldable (traverse_)
import Data.Text (Text)
import Data.UUID (nil)
import Messaging.Client.Core.Auth (authenticationHeaders)
import Messaging.Server (app)
import Messaging.Server.App (initialState)
import Messaging.Shared.Conversation (ConversationName (..))
import qualified Messaging.Shared.Response as Res
import Messaging.Shared.User (User (..), UserID (..), UserName (..))
import qualified Network.WebSockets as WS
import Network.WebSockets.Stream (Stream, makeStream)
import Say
import Test.Hspec

hasLocked :: String -> IO a -> IO a
hasLocked msg action =
  action
    `catches` [ Handler $ \exc@BlockedIndefinitelyOnMVar -> sayString ("[MVar]: " ++ msg) >> throwIO exc,
                Handler $ \exc@BlockedIndefinitelyOnSTM -> sayString ("[STM]: " ++ msg) >> throwIO exc
              ]

data TestClient = TestClient
  { connection :: WS.Connection
  }

data TestServer = TestServer
  { serverApp :: WS.ServerApp
  }

mkStreams :: IO (Stream, Stream)
mkStreams = do
  clientToServer <- newChan
  serverToClient <- newChan
  clientStream <-
    makeStream
      (readChan' "clientStream" serverToClient)
      (traverse_ (writeChan clientToServer) . chunks)
  serverStream <-
    makeStream
      (readChan' "serverStream" clientToServer)
      (traverse_ (writeChan serverToClient) . chunks)
  return (clientStream, serverStream)
  where
    chunks Nothing = [Nothing]
    chunks (Just bs) = Just <$> toChunks bs
    readChan' msg = hasLocked msg . readChan

withServer :: (TestServer -> IO ()) -> IO ()
withServer f = do
  state <- initialState
  f $ TestServer (app state)

mkUser :: Text -> TestServer -> IO TestClient
mkUser username server = do
  (clientStream, serverStream) <- mkStreams
  let options = WS.defaultConnectionOptions
  _ <- forkIO $ do
    serverConnection <- WS.makePendingConnectionFromStream serverStream options
    serverApp server serverConnection
  let headers = authenticationHeaders $ UserName username
  clientConnection <-
    hasLocked "clientConnection" $
      WS.newClientConnection clientStream "example.com" "test" options headers
  return $ TestClient clientConnection

getResponse :: TestClient -> IO Res.Response
getResponse client = do
  let c = connection client
  response <- WS.receiveData c
  case Res.deserialize response of
    Left e -> error $ show e
    Right r -> return r

dummyUser :: Text -> User
dummyUser uName = User {userID = UserID nil, userName = UserName uName}

joinedResponse :: Text -> Text -> Res.Response
joinedResponse uName conversationName =
  Res.JoinedConversation
    (dummyUser uName)
    (ConversationName conversationName)

normalizeUser :: User -> User
normalizeUser user = user {userID = UserID nil}

normalizeResponse :: Res.Response -> Res.Response
normalizeResponse (Res.ReceivedMessage user msg) = Res.ReceivedMessage (normalizeUser user) msg
normalizeResponse (Res.JoinedConversation user conv) = Res.JoinedConversation (normalizeUser user) conv
normalizeResponse (Res.LeftConversation user conv) = Res.LeftConversation (normalizeUser user) conv

assertResponseIs :: Res.Response -> Res.Response -> Expectation
assertResponseIs response1 response2 = normalizeResponse response1 `shouldBe` normalizeResponse response2 where
