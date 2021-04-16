-- | Functionality for parsing command line arguments.
module Messaging.Server.Parser where

import Options.Applicative

-- | Parser for the port to use for the server's websocket interface. Reads the port number (option
-- -p/--port).
parsePort :: Parser Int
parsePort =
  option
    auto
    ( long "port"
        <> short 'p'
        <> metavar "PORT"
        <> showDefault
        <> value 8080
        <> help "Port where the server offers its websocket interface"
    )

-- | Parses the command line arguments and returns the port number.
runParse :: IO Int
runParse =
  execParser $
    info
      (parsePort <**> helper)
      ( fullDesc
          <> progDesc "Messaging server"
          <> header "Sets up a local server."
      )
