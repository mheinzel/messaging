module Messaging.Server.Parser where

import Options.Applicative

parsePort :: Parser Int
parsePort = option auto
          ( long "port"
         <> short 'p'
         <> metavar "TARGET"
         <> value 8080
         <> help "Target is the port")

runParse :: IO Int
runParse = execParser $ info (parsePort <**> helper)
            ( fullDesc
     <> progDesc "Parses the port"
     <> header "Takes an int to use as a port after --port or -p" )