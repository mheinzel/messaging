{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}

module Server
  ( -- * Setting up server
    TestServer (..),
    withServer,

    -- * Setting up clients
    TestClient (..),
    mkClient,

    -- * Sending requests
    sendMessage,
    joinConversation,
    leaveConversation,

    -- * Inspecting responses
    getResponse,
    assertNoFurtherResponse,
    assertReceivedResponse,
    assertReceivedMessage,
    assertJoinedConversation,
    assertLeftConversation,
    assertResponseIs,
    joinedConversation,
  )
where

import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (newChan, readChan, writeChan)
import qualified Control.Exception as Exc
import Data.ByteString.Lazy (toChunks)
import Data.Foldable (traverse_)
import Data.Text (Text)
import Data.UUID (nil)
import qualified Messaging.Server as Server
import qualified Messaging.Server.App as App
import qualified Messaging.Server.Log as Log
import qualified Messaging.Shared.Auth as Auth
import qualified Messaging.Shared.Conversation as Conv
import qualified Messaging.Shared.Message as Msg
import qualified Messaging.Shared.Request as Req
import qualified Messaging.Shared.Response as Res
import qualified Messaging.Shared.User as User
import qualified Network.WebSockets as WS
import Network.WebSockets.Stream (Stream, makeStream)
import Say (sayString) -- concurrent output without interleaving
import System.Timeout (timeout)
import Test.HUnit (assertEqual, assertFailure)
import Test.Hspec (Expectation, HasCallStack)

-- Server ---------------------------------------------------------------------

newtype TestServer = TestServer
  { serverApp :: WS.ServerApp
  }

withServer :: (TestServer -> IO ()) -> IO ()
withServer f = do
  state <- App.initialState
  f $ TestServer (app testSettings state)
  where
    app :: App.Settings -> App.State -> WS.ServerApp
    app settings state pending =
      App.runApp settings state $
        Server.withAcceptedConnection pending Server.handleConnection

    testSettings :: App.Settings
    testSettings =
      App.Settings 8080 (Log.Settings Log.LoggingDisabled Log.LevelDebug)

-- Client ---------------------------------------------------------------------

newtype TestClient = TestClient
  { connection :: WS.Connection
  }

mkClient :: Text -> TestServer -> IO TestClient
mkClient username server = do
  (clientStream, serverStream) <- mkStreams
  let options = WS.defaultConnectionOptions
  -- not cleaned up, but should be okay for tests
  _ <- forkIO $ do
    serverConnection <- WS.makePendingConnectionFromStream serverStream options
    serverApp server serverConnection
  let headers = Auth.buildHeaders $ User.UserName username
  clientConnection <-
    logDeadlocks "clientConnection" $
      WS.newClientConnection clientStream "example.com" "test" options headers
  return $ TestClient clientConnection

mkStreams :: IO (Stream, Stream)
mkStreams = do
  clientToServer <- newChan
  serverToClient <- newChan
  clientStream <-
    makeStream
      (logDeadlocks "clientStream" $ readChan serverToClient)
      (traverse_ (writeChan clientToServer) . chunks)
  serverStream <-
    makeStream
      (logDeadlocks "serverStream" $ readChan clientToServer)
      (traverse_ (writeChan serverToClient) . chunks)
  return (clientStream, serverStream)
  where
    chunks Nothing = [Nothing]
    chunks (Just bs) = Just <$> toChunks bs

logDeadlocks :: String -> IO a -> IO a
logDeadlocks msg action =
  Exc.catches
    action
    [ Exc.Handler $ \e@Exc.BlockedIndefinitelyOnMVar -> do
        sayString ("blocked on MVar: " <> msg)
        Exc.throwIO e,
      Exc.Handler $ \e@Exc.BlockedIndefinitelyOnSTM -> do
        sayString ("blocked on STM: " <> msg)
        Exc.throwIO e
    ]

-- Requests -------------------------------------------------------------------

sendRequest :: TestClient -> Req.Request -> IO ()
sendRequest client = WS.sendTextData (connection client) . Req.serialize

sendMessage :: TestClient -> Text -> Text -> IO ()
sendMessage client conv msg =
  sendRequest client $
    Req.SendMessage $ Msg.Message (Conv.ConversationName conv) msg

joinConversation :: TestClient -> Text -> IO ()
joinConversation client conv =
  sendRequest client $
    Req.JoinConversation $ Conv.ConversationName conv

leaveConversation :: TestClient -> Text -> IO ()
leaveConversation client conv =
  sendRequest client $
    Req.LeaveConversation $ Conv.ConversationName conv

-- Responses ------------------------------------------------------------------

getResponse :: HasCallStack => TestClient -> IO Res.Response
getResponse client = do
  let c = connection client
  response <- WS.receiveData c
  case Res.deserialize response of
    Left e -> assertFailure $ "failed parsing server response: " <> show e
    Right r -> return r

assertNoFurtherResponse :: HasCallStack => TestClient -> IO ()
assertNoFurtherResponse client = do
  -- we currently can't read responses non-blockingly
  res <-
    timeout 100_000 $ -- 100 milliseconds
      getResponse client
  assertEqual "unexpected server response" Nothing res

assertReceivedResponse :: HasCallStack => TestClient -> (Res.Response -> IO ()) -> IO ()
assertReceivedResponse client assertion = do
  response <- getResponse client
  assertion response

assertReceivedMessage ::
  HasCallStack =>
  TestClient ->
  (User.UserName -> Msg.Message -> IO ()) ->
  IO ()
assertReceivedMessage client assertion =
  assertReceivedResponse client $ \case
    Res.ReceivedMessage user msg -> assertion (User.userName user) msg
    other -> assertFailure $ "expected ReceivedMessage, but got: " <> show other

assertJoinedConversation ::
  HasCallStack =>
  TestClient ->
  (User.UserName -> Conv.ConversationName -> IO ()) ->
  IO ()
assertJoinedConversation client assertion =
  assertReceivedResponse client $ \case
    Res.JoinedConversation user conv -> assertion (User.userName user) conv
    other -> assertFailure $ "expected JoinedConversation, but got: " <> show other

assertLeftConversation ::
  HasCallStack =>
  TestClient ->
  (User.UserName -> Conv.ConversationName -> IO ()) ->
  IO ()
assertLeftConversation client assertion =
  assertReceivedResponse client $ \case
    Res.LeftConversation user conv -> assertion (User.userName user) conv
    other -> assertFailure $ "expected LeftConversation, but got: " <> show other

assertResponseIs :: HasCallStack => Res.Response -> Res.Response -> Expectation
assertResponseIs response1 response2 =
  assertEqual "" (normalizeResponse response1) (normalizeResponse response2)

joinedConversation :: Text -> Text -> Res.Response
joinedConversation userName conversationName =
  Res.JoinedConversation
    (dummyUser userName)
    (Conv.ConversationName conversationName)

dummyUser :: Text -> User.User
dummyUser userName =
  User.User {User.userID = User.UserID nil, User.userName = User.UserName userName}

normalizeResponse :: Res.Response -> Res.Response
normalizeResponse = \case
  Res.ReceivedMessage user msg -> Res.ReceivedMessage (normalizeUser user) msg
  Res.JoinedConversation user conv -> Res.JoinedConversation (normalizeUser user) conv
  Res.LeftConversation user conv -> Res.LeftConversation (normalizeUser user) conv

normalizeUser :: User.User -> User.User
normalizeUser user = user {User.userID = User.UserID nil}
