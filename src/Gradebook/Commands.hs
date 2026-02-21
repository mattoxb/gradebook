{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Gradebook.Commands
  ( runLoadRoster
  , runLoadCategories
  , runLoadAssignments
  , runLoadScores
  , runGenerateReport
  , runMarkCollected
  , runSearchNetId
  , openConnection
  ) where

import Database.HDBC
import Database.HDBC.Sqlite3 (connectSqlite3, Connection)
-- import Database.HDBC.PostgreSQL (connectPostgreSQL)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Process (readProcess)
import System.Exit (exitFailure)
import qualified System.Directory
import System.Directory (findExecutable, doesFileExist)
import Control.Exception (catch, try, SomeException)
import Control.Monad (when)
import Data.Maybe (isJust)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Csv as Csv
import qualified Data.Vector as V
import Data.Csv (ToRecord(..), ToField(..))

import Gradebook.Config (Config(..), DbType(..), GradingConfig(..), loadConfig)
import Gradebook.Database (initDatabase, insertStudent, insertCategory, insertAssignment, insertScore, searchStudents, getScoresForStudent, Assignment(..))
import Gradebook.Roster (parseRosterCSV)
import Gradebook.Categories (parseCategoriesCSV)
import Gradebook.Assignments (parseAssignmentsCSV)
import Gradebook.Scores (parseScoresCSV)
import Gradebook.GradeCalculation (calculateGrades)
import Gradebook.Reports (generateReport)

-- | Open database connection based on config
openConnection :: Config -> IO Connection
openConnection config = case dbType config of
  SQLite -> do
    conn <- connectSqlite3 (T.unpack $ database config)
    -- Enable foreign key constraints (must be done per connection)
    _ <- quickQuery' conn "PRAGMA foreign_keys = ON" []
    return conn
  PostgreSQL -> error "PostgreSQL support not yet implemented. Please use SQLite."

-- | Load roster CSV into database
runLoadRoster :: FilePath -> IO ()
runLoadRoster rosterPath = do
  putStrLn $ "Loading roster from: " ++ rosterPath

  -- Load config
  config <- loadConfig "config.yaml"

  -- Parse roster CSV
  result <- parseRosterCSV rosterPath
  students <- case result of
    Left err -> do
      putStrLn $ "Error parsing CSV: " ++ err
      exitFailure
    Right s -> return s

  putStrLn $ "Parsed " ++ show (length students) ++ " students"

  -- Connect to database
  conn <- openConnection config

  -- Initialize database schema
  initDatabase conn

  -- Insert each student
  mapM_ (insertStudent conn) students

  -- Commit and close
  commit conn
  disconnect conn

  putStrLn $ "Successfully loaded " ++ show (length students) ++ " students into database"

-- | Load categories CSV into database
runLoadCategories :: FilePath -> IO ()
runLoadCategories categoriesPath = do
  putStrLn $ "Loading categories from: " ++ categoriesPath

  -- Load config
  config <- loadConfig "config.yaml"

  -- Parse categories CSV
  result <- parseCategoriesCSV categoriesPath
  categories <- case result of
    Left err -> do
      putStrLn $ "Error parsing CSV: " ++ err
      exitFailure
    Right c -> return c

  putStrLn $ "Parsed " ++ show (length categories) ++ " categories"

  -- Connect to database
  conn <- openConnection config

  -- Initialize database schema
  initDatabase conn

  -- Insert each category
  mapM_ (insertCategory conn) categories

  -- Commit and close
  commit conn
  disconnect conn

  putStrLn $ "Successfully loaded " ++ show (length categories) ++ " categories into database"

-- | Load assignments CSV into database
runLoadAssignments :: FilePath -> IO ()
runLoadAssignments assignmentsPath = do
  putStrLn $ "Loading assignments from: " ++ assignmentsPath

  -- Load config
  config <- loadConfig "config.yaml"

  -- Parse assignments CSV
  result <- parseAssignmentsCSV assignmentsPath
  assignments <- case result of
    Left err -> do
      putStrLn $ "Error parsing CSV: " ++ err
      exitFailure
    Right a -> return a

  putStrLn $ "Parsed " ++ show (length assignments) ++ " assignments"

  -- Connect to database
  conn <- openConnection config

  -- Initialize database schema
  initDatabase conn

  -- Insert each assignment
  mapM_ (insertAssignment conn) assignments

  -- Commit and close
  commit conn
  disconnect conn

  putStrLn $ "Successfully loaded " ++ show (length assignments) ++ " assignments into database"

-- | Load scores CSV into database
runLoadScores :: FilePath -> IO ()
runLoadScores scoresPath = do
  putStrLn $ "Loading scores from: " ++ scoresPath

  -- Load config
  config <- loadConfig "config.yaml"

  -- Parse scores CSV
  result <- parseScoresCSV scoresPath
  scores <- case result of
    Left err -> do
      putStrLn $ "Error parsing CSV: " ++ err
      exitFailure
    Right s -> return s

  putStrLn $ "Parsed " ++ show (length scores) ++ " score entries"

  -- Connect to database
  conn <- openConnection config

  -- Initialize database schema (ensures tables exist)
  initDatabase conn

  -- Insert each score
  mapM_ (insertScore conn) scores

  -- Commit and close
  commit conn
  disconnect conn

  putStrLn $ "Successfully loaded " ++ show (length scores) ++ " score entries into database"

-- | Search for a student using fzf and output their netid
runSearchNetId :: IO ()
runSearchNetId = do
  -- Check if fzf is available
  fzfPath <- findExecutable "fzf"
  case fzfPath of
    Nothing -> do
      putStrLn "Error: fzf not found in PATH"
      putStrLn "Please install fzf or ensure it's in your PATH"
      putStrLn "If running outside nix shell, try: nix develop --command stack exec gb -- netid"
      exitFailure
    Just fzfExe -> do
      -- Load config
      config <- loadConfig "config.yaml"

      -- Connect to database
      conn <- openConnection config

      -- Get all students (search with empty string matches all)
      students <- searchStudents conn ""

      disconnect conn

      if null students
        then do
          putStrLn "No students found in database. Run 'gb load-roster' first."
          exitFailure
        else do
          -- Format for fzf: "NetID | Name | Email | UIN"
          let fzfInput = unlines
                [ T.unpack $ T.intercalate " | " [netid, name, email, uin]
                | (netid, name, email, uin) <- students
                ]

          -- Run fzf
          result <- (readProcess fzfExe ["--height=40%", "--reverse"] fzfInput)
                    `catch` (\(e :: SomeException) -> do
                      putStrLn $ "Error running fzf: " ++ show e
                      putStrLn "Selection may have been cancelled"
                      exitFailure
                    )

          -- Extract netid (first field before |)
          let netid = takeWhile (/= '|') result
          putStrLn $ trim netid
  where
    trim = T.unpack . T.strip . T.pack

-- | Generate grade report for a student (or all students)
runGenerateReport :: Maybe String -> Bool -> Bool -> IO ()
runGenerateReport maybeNetid pushToGit generateAll = do
  -- Validate flags
  when (generateAll && not pushToGit) $ do
    putStrLn "Error: --all requires --push (cannot generate all reports to console)"
    exitFailure

  when (generateAll && isJust maybeNetid) $ do
    putStrLn "Warning: --netid is ignored when --all is specified"

  -- Handle --all case
  if generateAll
    then runGenerateReportForAll
    else runGenerateReportForOne maybeNetid pushToGit

-- | Generate report for a single student
runGenerateReportForOne :: Maybe String -> Bool -> IO ()
runGenerateReportForOne maybeNetid pushToGit = do
  -- Get netid (use fzf if not provided)
  netidStr <- case maybeNetid of
    Just n -> return n
    Nothing -> do
      putStrLn "No netid provided, using fzf to select student..."
      -- Reuse the netid search logic
      runSearchNetIdForReport

  let netid = T.pack netidStr

  -- Load config
  config <- loadConfig "config.yaml"

  -- Check if grading config exists
  case grading config of
    Nothing -> do
      putStrLn "Error: No grading configuration found in config.yaml"
      putStrLn "Please add a 'grading' section with category weights and policies"
      exitFailure
    Just gradingCfg -> do
      -- Connect to database
      conn <- openConnection config

      -- Get scores for student
      scores <- getScoresForStudent conn netid

      disconnect conn

      -- Calculate grades
      let categoryGrades = calculateGrades gradingCfg scores

      -- Generate report
      let report = generateReport netid categoryGrades

      -- Output or push report
      if pushToGit
        then do
          case repoPrefix config of
            Nothing -> do
              putStrLn "Error: No repo-prefix in config.yaml"
              exitFailure
            Just prefix -> do
              pushReportToGit netidStr (T.unpack prefix) report
        else do
          -- Print to console
          TIO.putStrLn report

-- | Generate and push reports for all students
runGenerateReportForAll :: IO ()
runGenerateReportForAll = do
  putStrLn "Generating reports for all students..."

  -- Load config
  config <- loadConfig "config.yaml"

  -- Check if grading config exists
  case grading config of
    Nothing -> do
      putStrLn "Error: No grading configuration found in config.yaml"
      exitFailure
    Just gradingCfg -> do
      -- Check repo prefix exists
      case repoPrefix config of
        Nothing -> do
          putStrLn "Error: No repo-prefix in config.yaml"
          exitFailure
        Just prefix -> do
          -- Connect to database
          conn <- openConnection config

          -- Get all students
          students <- searchStudents conn ""
          disconnect conn

          if null students
            then do
              putStrLn "No students found in database."
              exitFailure
            else do
              putStrLn $ "Found " ++ show (length students) ++ " students"
              putStrLn "Generating and pushing reports..."
              putStrLn ""

              -- Process each student
              mapM_ (generateAndPushForStudent config gradingCfg (T.unpack prefix)) students

              putStrLn ""
              putStrLn $ "Done! Processed " ++ show (length students) ++ " students"

-- | Generate and push report for a single student (helper for bulk processing)
generateAndPushForStudent :: Config -> GradingConfig -> String -> (T.Text, T.Text, T.Text, T.Text) -> IO ()
generateAndPushForStudent config gradingCfg repoPrefix (netid, name, _, _) = do
  putStrLn $ "Processing: " ++ T.unpack netid ++ " (" ++ T.unpack name ++ ")"

  -- Connect to database
  conn <- openConnection config

  -- Get scores for student
  scores <- getScoresForStudent conn netid
  disconnect conn

  -- Calculate grades
  let categoryGrades = calculateGrades gradingCfg scores

  -- Generate report
  let report = generateReport netid categoryGrades

  -- Push to git
  pushReportToGit (T.unpack netid) repoPrefix report
    `catch` (\(e :: SomeException) -> do
      putStrLn $ "  ERROR: " ++ show e
    )

  putStrLn ""

-- | Helper function to get netid using fzf (returns String)
runSearchNetIdForReport :: IO String
runSearchNetIdForReport = do
  -- Check if fzf is available
  fzfPath <- findExecutable "fzf"
  case fzfPath of
    Nothing -> do
      putStrLn "Error: fzf not found in PATH"
      exitFailure
    Just fzfExe -> do
      -- Load config
      config <- loadConfig "config.yaml"

      -- Connect to database
      conn <- openConnection config

      -- Get all students
      students <- searchStudents conn ""

      disconnect conn

      if null students
        then do
          putStrLn "No students found in database."
          exitFailure
        else do
          -- Format for fzf
          let fzfInput = unlines
                [ T.unpack $ T.intercalate " | " [netid, name, email, uin]
                | (netid, name, email, uin) <- students
                ]

          -- Run fzf
          result <- readProcess fzfExe ["--height=40%", "--reverse"] fzfInput
                    `catch` (\(e :: SomeException) -> do
                      putStrLn $ "Error: " ++ show e
                      exitFailure
                    )

          -- Extract netid
          let netid = takeWhile (/= '|') result
          return $ T.unpack $ T.strip $ T.pack netid

-- | Push report to student's git repository
pushReportToGit :: String -> String -> T.Text -> IO ()
pushReportToGit netid repoPrefix report = do
  let repoUrl = repoPrefix ++ netid
      repoDir = "repos/" ++ netid
      reportFile = repoDir ++ "/GRADE_REPORT.md"

  putStrLn $ "Pushing report to: " ++ repoUrl

  -- Check if repo directory exists
  repoExists <- System.Directory.doesDirectoryExist repoDir

  cloneOk <- if repoExists
    then do
      putStrLn $ "Updating existing repository: " ++ repoDir
      -- Pull latest changes
      _ <- readProcess "git" ["-C", repoDir, "pull"] ""
              `catch` (\(e :: SomeException) -> do
                putStrLn $ "Warning: git pull failed: " ++ show e
                return ""
              )
      return True
    else do
      putStrLn $ "Cloning repository: " ++ repoUrl
      -- Clone repository
      result <- (try (readProcess "git" ["clone", repoUrl, repoDir] "") :: IO (Either SomeException String))
      case result of
        Left _ -> do
          putStrLn $ "Skipping " ++ netid ++ ": repository not found upstream"
          return False
        Right _ -> return True

  when cloneOk $ do
    -- Write report
    TIO.writeFile reportFile report
    putStrLn $ "Wrote report to: " ++ reportFile

    -- Git add, commit, and push
    _ <- readProcess "git" ["-C", repoDir, "add", "GRADE_REPORT.md"] ""
    _ <- readProcess "git" ["-C", repoDir, "commit", "-m", "Update grade report"] ""
          `catch` (\(e :: SomeException) -> do
            putStrLn $ "Nothing to commit (report unchanged)"
            return ""
          )
    _ <- readProcess "git" ["-C", repoDir, "push"] ""
          `catch` (\(e :: SomeException) -> do
            putStrLn $ "Error pushing to repository: " ++ show e
            exitFailure
          )

    putStrLn "Successfully pushed grade report!"

-- | Mark assignments as collected (updates both DB and CSV)
runMarkCollected :: [String] -> IO ()
runMarkCollected slugStrs = do
  let slugs = map T.pack slugStrs

  putStrLn $ "Marking " ++ show (length slugs) ++ " assignment(s) as collected..."

  -- Load config
  config <- loadConfig "config.yaml"

  -- Connect to database
  conn <- openConnection config

  -- Update each assignment in the database
  mapM_ (markCollectedInDB conn) slugs

  commit conn
  disconnect conn

  putStrLn "Updated database"

  -- Update CSV file
  let csvPath = "data-files/assignments.csv"
  csvExists <- doesFileExist csvPath

  if not csvExists
    then do
      putStrLn $ "Warning: CSV file not found at " ++ csvPath
      putStrLn "Database updated but CSV file not modified"
    else do
      -- Read CSV
      csvData <- BL.readFile csvPath
      case Csv.decodeByName csvData :: Either String (Csv.Header, V.Vector Assignment) of
        Left err -> do
          putStrLn $ "Error reading CSV: " ++ err
          putStrLn "Database updated but CSV file not modified"
        Right (header, assignments) -> do
          -- Update collected field for matching assignments
          let updatedAssignments = V.map (updateCollectedField slugs) assignments

          -- Encode back to CSV
          let updatedCSV = Csv.encodeByName header (V.toList updatedAssignments)

          -- Write CSV
          BL.writeFile csvPath updatedCSV
          putStrLn $ "Updated " ++ csvPath

  putStrLn "Done!"

-- | Update assignment collected field in database
markCollectedInDB :: IConnection conn => conn -> T.Text -> IO ()
markCollectedInDB conn slug = do
  _ <- run conn "UPDATE assignments SET collected = 1 WHERE slug = ?" [toSql slug]
  return ()

-- | Update collected field for an assignment if slug matches
updateCollectedField :: [T.Text] -> Assignment -> Assignment
updateCollectedField slugs assignment =
  if assignmentSlug assignment `elem` slugs
    then assignment { assignmentCollected = True }
    else assignment
