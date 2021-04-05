{-# LANGUAGE QuasiQuotes #-}

module Messaging.Client.Core.Parser where

import Options.Applicative
import Data.Semigroup ((<>))
import Messaging.Shared.User as User
import Data.Text (pack)
import Options.Applicative.Types
import qualified Messaging.Client.Core.Connection as Con
import qualified URI.ByteString as URI
import qualified Data.ByteString.Char8 as Byte

data ConnectInput = ConnectInput 
  { username    :: User.UserName 
  , uri         :: Con.URI}

parseConnectInput :: Parser ConnectInput
parseConnectInput = ConnectInput
      <$> option userReadM
          ( long "username"
         <> metavar "NAME"
         <> help "Name for the username" )
      <*> option uriReadM
          ( long "uri"
         <> metavar "TARGET"
         <> help "Target for the uri" )

userReadM :: ReadM User.UserName 
userReadM = User.UserName . pack <$> readerAsk

uriReadM :: ReadM Con.URI 
uriReadM = stringToUri <$> readerAsk

stringToUri :: String -> Con.URI
stringToUri s = case Con.uriFromAbsolute uriLoc  of
            Right u -> u
            Left err -> error $ "Unexpected invalid default URI: " <> err
        where uriLoc = either (error . show) id (URI.parseURI URI.laxURIParserOptions (Byte.pack s))

