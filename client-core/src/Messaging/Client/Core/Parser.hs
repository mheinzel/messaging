module Messaging.Client.Core.Parser where

import Options.Applicative
import Options.Applicative.Types (readerAsk)
import Data.Text (pack)
import qualified Messaging.Client.Core.Connection as Con
import qualified URI.ByteString as URI
import qualified Data.ByteString.Char8 as Byte
import qualified Messaging.Shared.User as User

-- | Stores the inputs given at connection
data ConnectInput = ConnectInput 
  { _username    :: User.UserName
  , _uri         :: Con.URI}

parseConnectInput :: Parser ConnectInput
parseConnectInput = ConnectInput
      <$> option userReadM
          ( long "username"
         <> short 'u'
         <> metavar "NAME"
         <> help "Specifies the username to be used")
      <*> option uriReadM
          ( long "uri"
         <> metavar "URI"
         <> showDefault
         <> value Con.defaultURI 
         <> help "Specifies what the client should connect to")

userReadM :: ReadM User.UserName
userReadM = stringToUsername <$> readerAsk

stringToUsername :: String -> User.UserName 
stringToUsername s = case User.mkUserName (pack s) of
        Just userName -> userName
        Nothing -> error "error: invalid user name"

uriReadM :: ReadM Con.URI 
uriReadM = eitherReader stringToUri

stringToUri :: String -> Either String Con.URI
stringToUri s = Con.uriFromAbsolute uriLoc
        where uriLoc = either (error . show) id (URI.parseURI URI.laxURIParserOptions (Byte.pack s))

-- | Parses the command line arguments and returns a record with the connection info
runParse :: IO ConnectInput
runParse = execParser $ info (parseConnectInput <**> helper)
            ( fullDesc
     <> progDesc "Messaging client"
     <> header "Starts a local client." )