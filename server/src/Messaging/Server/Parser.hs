module Messaging.Server.Parser where

import Options.Applicative

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

runParse :: IO Int
runParse =
  execParser $
    info
      (parsePort <**> helper)
      ( fullDesc
          <> progDesc "Messaging server"
          <> header "Sets up a local server."
      )
