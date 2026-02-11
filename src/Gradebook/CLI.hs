{-# LANGUAGE OverloadedStrings #-}

module Gradebook.CLI
  ( Command(..)
  , parseCommand
  , run
  ) where

import Options.Applicative
import qualified Data.Text as T
import Gradebook.Commands (runLoadRoster, runSearchNetId)

data Command
  = LoadRoster
      { rosterFile :: FilePath
      }
  | SearchNetId
  deriving (Show, Eq)

-- | Parser for LoadRoster command
loadRosterParser :: Parser Command
loadRosterParser = LoadRoster
  <$> strOption
      ( long "roster"
     <> short 'r'
     <> metavar "FILE"
     <> value "data-files/roster.csv"
     <> help "Path to roster CSV file (default: data-files/roster.csv)"
      )

-- | Parser for SearchNetId command
searchNetIdParser :: Parser Command
searchNetIdParser = pure SearchNetId

-- | Command parsers with subcommands
commandParser :: Parser Command
commandParser = hsubparser
  ( command "load-roster"
    ( info loadRosterParser
      ( progDesc "Load roster CSV into the database" )
    )
  <> command "netid"
    ( info searchNetIdParser
      ( progDesc "Search for a student and output their netid" )
    )
  )

-- | Main command parser with program info
parseCommand :: IO Command
parseCommand = execParser $ info (commandParser <**> helper)
  ( fullDesc
  <> progDesc "CS421 Gradebook Manager"
  <> header "gb - a gradebook management tool"
  )

-- | Run the parsed command
run :: Command -> IO ()
run cmd = case cmd of
  LoadRoster{rosterFile = path} -> runLoadRoster path
  SearchNetId -> runSearchNetId
