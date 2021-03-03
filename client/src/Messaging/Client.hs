{-# LANGUAGE OverloadedStrings #-}
module Messaging.Client where

import           Control.Monad       (unless)
import           Network.Socket      (withSocketsDo)
import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Data.Text.IO        as T
import qualified Network.WebSockets  as WS

runClient :: IO ()
runClient = do
  -- TODO: read from command line args
  withSocketsDo $ WS.runClient "127.0.0.1" 8080 "/" client

client :: WS.ClientApp ()
client conn = do
    putStrLn "Connected!"

    -- Read from stdin and write to WS
    let loop = do
            line <- T.getLine
            unless (T.null line) $ do
              WS.sendTextData conn line
              msg <- WS.receiveData conn
              T.putStrLn msg
              loop

    loop
    WS.sendClose conn ("Bye!" :: Text)
