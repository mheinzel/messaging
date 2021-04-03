{-# LANGUAGE OverloadedStrings #-}

import Messaging.Shared.Conversation (ConversationName (..))
import qualified Messaging.Shared.Response as Res
import Server
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "server" $ do
    it "automatically has new clients join the general chat" $ do
      withServer $ \server -> do
        client <- mkUser "someone" server
        response <- getResponse client
        assertResponseIs
          (normalizeResponse response)
          (Res.JoinedConversation (dummyUser "someone") (ConversationName "general"))
