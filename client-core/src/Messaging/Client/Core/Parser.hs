module Messaging.Client.Core.Parser where

import Options.Applicative
import Data.Semigroup ((<>))
import Messaging.Shared.User as User
import Data.Text (Text, pack)
import Options.Applicative.Types
import qualified Messaging.Client.Core.Connection as Con
import qualified URI.ByteString as URI
import qualified Data.ByteString.Char8 as Byte

data ConnectInput = ConnectInput 
  { _username    :: Text -- Parsing to text and convert in client code
  , _uri         :: Con.URI}

parseConnectInput :: Parser ConnectInput
parseConnectInput = ConnectInput
      <$> option userReadM
          ( long "username"
         <> short 'u'
         <> metavar "NAME"
         <> help "Name for the username" )
      <*> option uriReadM
          ( long "uri"
         <> metavar "TARGET"
         <> help "Target is the uri" )

userReadM :: ReadM Text
userReadM = pack <$> readerAsk

uriReadM :: ReadM Con.URI 
uriReadM = stringToUri <$> readerAsk

stringToUri :: String -> Con.URI
stringToUri s = case Con.uriFromAbsolute uriLoc  of
            Right uri -> uri
            Left err -> error $ "Unexpected invalid default URI: " <> err
        where uriLoc = either (error . show) id (URI.parseURI URI.laxURIParserOptions (Byte.pack s))

runParse :: IO ConnectInput
runParse = execParser $ info (parseConnectInput <**> helper)
            ( fullDesc
     <> progDesc "Parses urename and URI"
     <> header "Takes a username -u \"username\" and URI -u \"URI\" and returns them." )