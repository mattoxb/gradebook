{-# LANGUAGE OverloadedStrings #-}

module Gradebook.CLI
  ( Command(..)
  , parseCommand
  , run
  ) where

import Options.Applicative
import qualified Data.Text as T
import Gradebook.Commands (runLoadRoster, runSearchNetId, runLoadCategories, runLoadAssignments, runLoadScores, runGenerateReport, runMarkCollected)

data Command
  = LoadRoster
      { rosterFile :: FilePath
      }
  | LoadCategories
      { categoriesFile :: FilePath
      }
  | LoadAssignments
      { assignmentsFile :: FilePath
      }
  | LoadScores
      { scoresFile :: FilePath
      }
  | GenerateReport
      { reportNetId :: Maybe String
      , pushToGit   :: Bool
      , reportAll   :: Bool
      }
  | MarkCollected
      { assignmentSlugs :: [String]
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

-- | Parser for LoadCategories command
loadCategoriesParser :: Parser Command
loadCategoriesParser = LoadCategories
  <$> strOption
      ( long "categories"
     <> short 'c'
     <> metavar "FILE"
     <> value "data-files/categories.csv"
     <> help "Path to categories CSV file (default: data-files/categories.csv)"
      )

-- | Parser for LoadAssignments command
loadAssignmentsParser :: Parser Command
loadAssignmentsParser = LoadAssignments
  <$> strOption
      ( long "assignments"
     <> short 'a'
     <> metavar "FILE"
     <> value "data-files/assignments.csv"
     <> help "Path to assignments CSV file (default: data-files/assignments.csv)"
      )

-- | Parser for LoadScores command
loadScoresParser :: Parser Command
loadScoresParser = LoadScores
  <$> strArgument
      ( metavar "FILE"
     <> help "Path to scores CSV file"
      )

-- | Parser for GenerateReport command
generateReportParser :: Parser Command
generateReportParser = GenerateReport
  <$> optional (strOption
      ( long "netid"
     <> short 'n'
     <> metavar "NETID"
     <> help "Student netid (uses fzf if not provided)"
      ))
  <*> switch
      ( long "push"
     <> short 'p'
     <> help "Push report to student's GitHub repository"
      )
  <*> switch
      ( long "all"
     <> short 'a'
     <> help "Generate reports for all students (requires --push)"
      )

-- | Parser for MarkCollected command
markCollectedParser :: Parser Command
markCollectedParser = MarkCollected
  <$> some (strArgument
      ( metavar "SLUG..."
     <> help "Assignment slug(s) to mark as collected"
      ))

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
  <> command "load-categories"
    ( info loadCategoriesParser
      ( progDesc "Load categories CSV into the database" )
    )
  <> command "load-assignments"
    ( info loadAssignmentsParser
      ( progDesc "Load assignments CSV into the database" )
    )
  <> command "load-scores"
    ( info loadScoresParser
      ( progDesc "Load scores CSV into the database" )
    )
  <> command "generate-report"
    ( info generateReportParser
      ( progDesc "Generate grade report for a student" )
    )
  <> command "mark-collected"
    ( info markCollectedParser
      ( progDesc "Mark assignment(s) as collected (updates DB and CSV)" )
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
  LoadCategories{categoriesFile = path} -> runLoadCategories path
  LoadAssignments{assignmentsFile = path} -> runLoadAssignments path
  LoadScores{scoresFile = path} -> runLoadScores path
  GenerateReport{reportNetId = netid, pushToGit = push, reportAll = all'} -> runGenerateReport netid push all'
  MarkCollected{assignmentSlugs = slugs} -> runMarkCollected slugs
  SearchNetId -> runSearchNetId
