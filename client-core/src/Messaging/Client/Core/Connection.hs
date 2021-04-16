{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Messaging.Client.Core.Connection where

import Control.Concurrent (Chan, forkIO, killThread, newChan, readChan, writeChan)
import Control.Exception (bracket)
import Control.Monad (forever)
import Data.ByteString (ByteString)
import Data.Maybe (fromMaybe)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Messaging.Shared.Auth as Auth
import qualified Messaging.Shared.Request as Req
import qualified Messaging.Shared.Response as Res
import qualified Messaging.Shared.User as User
import Network.Socket (withSocketsDo)
import qualified Network.WebSockets as WS
import qualified URI.ByteString as URI
import qualified URI.ByteString.QQ as URI.QQ
import qualified Wuss -- secure websockets

-- | The URI subset we currently care about, more strongly typed than just
-- @String@s and @Int@s.
data URI = URI
  { -- | HTTP or HTTPS?
    uriSecure :: Bool,
    uriHost :: URI.Host,
    uriPort :: URI.Port,
    uriPath :: ByteString
  }
  deriving (Show)

-- | Default URI (localhost).
defaultURI :: URI
defaultURI =
  case uriFromAbsolute [URI.QQ.uri|ws://127.0.0.1:8080/|] of
    Right uri -> uri
    Left err -> error $ "Unexpected invalid default URI: " <> err

-- | The default URI (as defined by 'defaultURI') as a string.
-- This should match the 'defaultURI' function.
showDefaultURI :: String
showDefaultURI = "ws://127.0.0.1:8080/"

uriFromAbsolute :: URI.URIRef URI.Absolute -> Either String URI
uriFromAbsolute URI.URI {URI.uriScheme, URI.uriAuthority, URI.uriPath} = do
  secure <- case URI.schemeBS uriScheme of
    "ws" -> pure False
    "wss" -> pure True
    "http" -> pure False
    "https" -> pure True
    other -> Left $ "unsupported scheme: " <> show other
  authority <- note "contains no authority" uriAuthority
  let port = fromMaybe (defaultPort secure) (URI.authorityPort authority)
  pure $ URI secure (URI.authorityHost authority) port uriPath
  where
    note err = maybe (Left err) Right
    defaultPort secure = if secure then URI.Port 443 else URI.Port 80

-- | Runs the chat client.
runClientApp ::
  -- | The URI of the server.
  URI ->
  -- | The username used to identify this client.
  User.UserName ->
  -- | The main client action, which is passed the established connection.
  (WS.Connection -> IO a) ->
  IO a
runClientApp URI {uriSecure, uriHost, uriPort, uriPath} userName client = do
  let host = Text.unpack . Text.decodeUtf8 $ URI.hostBS uriHost
  let port = URI.portNumber uriPort
  let path = Text.unpack . Text.decodeUtf8 $ Auth.buildPath uriPath userName
  let options = WS.defaultConnectionOptions
  let headers = []

  withSocketsDo $
    if uriSecure
      then Wuss.runSecureClientWith host (fromIntegral port) path options headers client
      else WS.runClientWith host port path options headers client

-- IDEA: If we used the async package here, we could check whether any of the
-- spawned threads terminated and re-raise the exception in the main thread.
--
-- Also, maybe the Chans should also contain CloseRequests, so we can terminate
-- connections cleanly. E.g. when closing the client, instead of just closing
-- the connection forcefully, we should:
-- - when the user quits, send a CloseRequest to the server, continue normally
-- - when the server responds with CloseRequest, terminate the client
-- When we receive a CloseRequest from the server unexpectedly, we should put
-- one last CloseRequest into the outgoing Chan (and nothing more), drain it,
-- and then terminate the client (or try re-establishing a connection).
-- This will probably require keeping some extra state in the clients and
-- switching to a different Chan implementation (e.g. TChan) that supports
-- peeking for available messages without blocking.
--
-- See https://www.stackage.org/haddock/nightly-2021-03-19/websockets-0.12.7.2/Network-WebSockets.html#v:sendClose
withConnectionThreads ::
  WS.Connection ->
  (Chan Res.Response -> Chan Req.Request -> IO a) ->
  IO a
withConnectionThreads conn action =
  bracket setup cleanup $ \(_threads, (incoming, outgoing)) ->
    action incoming outgoing
  where
    setup = do
      incoming <- newChan
      outgoing <- newChan
      receiving <- forkIO $ recvThread conn (writeChan incoming)
      sending <- forkIO $ sendThread conn (readChan outgoing)
      pure ((receiving, sending), (incoming, outgoing))

    cleanup ((receiving, sending), _chans) = do
      killThread sending
      killThread receiving

-- | Creates a thread that sends requests over a connection.
sendThread ::
  -- | The connection over which to send the requests.
  WS.Connection ->
  -- | An IO action that retrieves the next request to be sent.
  IO Req.Request ->
  IO ()
sendThread conn getRequest = forever $ do
  req <- getRequest
  WS.sendTextData conn (Req.serialize req)

-- | Creates a thread through which responses from the server are received.
recvThread ::
  -- | The connection through which to listen for responses.
  WS.Connection ->
  -- | An IO action to handle an incoming response.
  (Res.Response -> IO ()) ->
  IO ()
recvThread conn putResponse = forever $ do
  received <- WS.receiveData conn
  case Res.deserialize received of
    Left err -> printError err
    Right res -> putResponse res
  where
    printError :: Res.DeserializeError -> IO ()
    printError err =
      putStrLn $ "failed to deserialize message: " <> Res.errorMessage err
