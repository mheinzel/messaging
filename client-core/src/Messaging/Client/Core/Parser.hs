module Messaging.Client.Core.Parser where

import qualified Data.ByteString.Char8 as Byte
import Data.Text (pack)
import qualified Messaging.Client.Core.Connection as Con
import qualified Messaging.Shared.User as User
import Options.Applicative
import qualified URI.ByteString as URI

-- | Stores the inputs given at connection
data ConnectInput = ConnectInput
  { _username :: User.UserName,
    _uri :: Con.URI
  }

parseConnectInput :: Parser ConnectInput
parseConnectInput =
  ConnectInput
    <$> option
      userReadM
      ( long "username"
          <> short 'u'
          <> metavar "NAME"
          <> help "Specifies the username to be used"
      )
    <*> option
      uriReadM
      ( long "uri"
          <> metavar "URI"
          <> showDefaultWith (const Con.showDefaultURI) -- hardcoded string
          <> value Con.defaultURI
          <> help "Specifies what the client should connect to"
      )

userReadM :: ReadM User.UserName
userReadM = eitherReader stringToUsername

stringToUsername :: String -> Either String User.UserName
stringToUsername s = case User.mkUserName (pack s) of
  Just userName -> Right userName
  Nothing -> Left "error: invalid user name"

uriReadM :: ReadM Con.URI
uriReadM = eitherReader stringToUri

stringToUri :: String -> Either String Con.URI
stringToUri s = case URI.parseURI URI.laxURIParserOptions (Byte.pack s) of
  Left err -> Left (show err)
  Right uriA -> case Con.uriFromAbsolute uriA of
    Left err -> Left err
    Right uri -> Right uri

-- | Parses the command line arguments and returns a record with the connection info
runParse :: IO ConnectInput
runParse =
  execParser $
    info
      (parseConnectInput <**> helper)
      ( fullDesc
          <> progDesc "Messaging client"
          <> header "Starts a local client."
      )
