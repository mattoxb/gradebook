{-# LANGUAGE OverloadedStrings #-}

module Gradebook.CLI
  ( Command(..)
  , parseCommand
  , run
  ) where

import Options.Applicative
import qualified Data.Text as T
import Gradebook.Commands (runLoadRoster, runSearchNetId, runLoadCategories, runLoadAssignments, runLoadScores, runGenerateReport, runFinalGrades, runMarkCollected, runLoadExam, runLoadExamOverrides, runLoadExamZones, runGenExamZones)
import Gradebook.Version (versionString)

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
  | LoadExamZones
      { zonesExamSlug :: String
      , zonesCsvFile  :: FilePath
      }
  | GenExamZones
      { genZonesExamSlug   :: String
      , genInfoAssessment  :: FilePath
      , genZonesOutFile    :: Maybe FilePath
      , genZonesForce      :: Bool
      }
  | LoadExam
      { examSlug       :: String
      , examScoresFile :: FilePath
      }
  | LoadExamOverrides
      { overrideExamSlug :: String
      , overridesFile    :: FilePath
      }
  | GenerateReport
      { reportNetId :: Maybe String
      , pushToGit   :: Bool
      , reportAll   :: Bool
      }
  | MarkCollected
      { assignmentSlugs :: [String]
      }
  | FinalGrades
      { finalGradesOutPath :: FilePath
      }
  | SearchNetId
  | Version
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

-- | Parser for LoadExamZones command
loadExamZonesParser :: Parser Command
loadExamZonesParser = LoadExamZones
  <$> strOption
      ( long "exam"
     <> short 'e'
     <> metavar "SLUG"
     <> help "Exam assignment slug (e.g., exam-1)"
      )
  <*> strArgument
      ( metavar "FILE"
     <> help "Path to exam zones CSV (typically data-files/<slug>-zones.csv)"
      )

-- | Parser for GenExamZones command
genExamZonesParser :: Parser Command
genExamZonesParser = GenExamZones
  <$> strOption
      ( long "exam"
     <> short 'e'
     <> metavar "SLUG"
     <> help "Exam assignment slug (e.g., exam-1)"
      )
  <*> strArgument
      ( metavar "INFO_JSON"
     <> help "Path to PrairieLearn infoAssessment.json"
      )
  <*> optional (strOption
      ( long "output"
     <> short 'o'
     <> metavar "FILE"
     <> help "Output CSV path (default: data-files/<slug>-zones.csv)"
      ))
  <*> switch
      ( long "force"
     <> short 'f'
     <> help "Overwrite output file if it already exists"
      )

-- | Parser for LoadExam command
loadExamParser :: Parser Command
loadExamParser = LoadExam
  <$> strOption
      ( long "exam"
     <> short 'e'
     <> metavar "SLUG"
     <> help "Exam assignment slug (e.g., exam-1). If this is a retake slug defined in config, scores will be combined with the original."
      )
  <*> strArgument
      ( metavar "FILE"
     <> help "Path to PrairieLearn instance_questions CSV file"
      )

-- | Parser for LoadExamOverrides command
loadExamOverridesParser :: Parser Command
loadExamOverridesParser = LoadExamOverrides
  <$> strOption
      ( long "exam"
     <> short 'e'
     <> metavar "SLUG"
     <> help "Exam assignment slug (e.g., exam-1)"
      )
  <*> strArgument
      ( metavar "FILE"
     <> help "Path to overrides CSV file (netid,question_id,score,max_points,reason)"
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

-- | Parser for FinalGrades command
finalGradesParser :: Parser Command
finalGradesParser = FinalGrades
  <$> strOption
      ( long "output"
     <> short 'o'
     <> metavar "FILE"
     <> value "output-files/grades.xlsx"
     <> help "Output spreadsheet path (default: output-files/grades.xlsx)"
      )

-- | Parser for SearchNetId command
searchNetIdParser :: Parser Command
searchNetIdParser = pure SearchNetId

-- | Parser for Version command
versionParser :: Parser Command
versionParser = pure Version

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
  <> command "gen-exam-zones"
    ( info genExamZonesParser
      ( progDesc "Generate data-files/<slug>-zones.csv from a PrairieLearn infoAssessment.json (refuses to overwrite without --force)" )
    )
  <> command "load-exam-zones"
    ( info loadExamZonesParser
      ( progDesc "Load exam zone/question structure from a zones CSV (must run before load-exam)" )
    )
  <> command "load-exam"
    ( info loadExamParser
      ( progDesc "Load exam scores from PrairieLearn CSV (requires load-exam-zones to have run first)" )
    )
  <> command "load-exam-overrides"
    ( info loadExamOverridesParser
      ( progDesc "Apply score overrides to exam questions (takes max of existing and override)" )
    )
  <> command "report"
    ( info generateReportParser
      ( progDesc "Generate grade report for a student" )
    )
  <> command "collect"
    ( info markCollectedParser
      ( progDesc "Mark assignment(s) as collected (updates DB and CSV)" )
    )
  <> command "final-grades"
    ( info finalGradesParser
      ( progDesc "Write a .xlsx of final letter grades for upload (requires term-code and grade-thresholds in config)" )
    )
  <> command "netid"
    ( info searchNetIdParser
      ( progDesc "Search for a student and output their netid" )
    )
  <> command "version"
    ( info versionParser
      ( progDesc "Show version information" )
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
  LoadExamZones{zonesExamSlug = slug, zonesCsvFile = path} -> runLoadExamZones slug path
  GenExamZones{genZonesExamSlug = slug, genInfoAssessment = jsonPath, genZonesOutFile = out, genZonesForce = force} -> runGenExamZones slug jsonPath out force
  LoadExam{examSlug = slug, examScoresFile = path} -> runLoadExam slug path
  LoadExamOverrides{overrideExamSlug = slug, overridesFile = path} -> runLoadExamOverrides slug path
  GenerateReport{reportNetId = netid, pushToGit = push, reportAll = all'} -> runGenerateReport netid push all'
  MarkCollected{assignmentSlugs = slugs} -> runMarkCollected slugs
  FinalGrades{finalGradesOutPath = path} -> runFinalGrades path
  SearchNetId -> runSearchNetId
  Version -> putStrLn versionString
