{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Gradebook.Commands
  ( runLoadRoster
  , runLoadCategories
  , runLoadAssignments
  , runLoadScores
  , runLoadExam
  , runLoadExamOverrides
  , runGenerateReport
  , runFinalGrades
  , runMarkCollected
  , runSearchNetId
  , openConnection
  , buildStudentReportData
  ) where

import Database.HDBC
import Database.HDBC.Sqlite3 (connectSqlite3)
import Database.HDBC.PostgreSQL (connectPostgreSQL)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Process (readProcess)
import System.Exit (exitFailure)
import qualified System.Directory
import System.Directory (findExecutable, doesFileExist)
import Control.Exception (catch, try, SomeException)
import Control.Monad (when, foldM)
import Data.Maybe (isJust)
import qualified Data.Set as Set
import qualified Data.Map.Strict as M
import Data.List (find)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Csv as Csv
import qualified Data.Vector as V
import Data.Csv (ToRecord(..), ToField(..))

import Gradebook.Config (Config(..), DbType(..), GradingConfig(..), GradingMode(..), CategoryConfig(..), ExamConfig(..), RetakePolicy(..), loadConfig)
import Gradebook.Database (initDatabase, insertStudent, insertCategory, insertAssignment, insertScore, searchStudents, getScoresForStudent, getAllScores, Assignment(..), Score(..), getAllAssignmentSlugs, getAllStudentNetids, getAllStudentIdentifiers, ExamZone(..), ExamQuestionScore(..), insertExamZone, insertExamQuestionScore, getExamQuestionScoresForStudent, updateAssignmentScore, getExamZonesForExam, applyExamQuestionOverride, getAllExamSlugs, getStudentCreditHours, getStudentName)
import Gradebook.FinalGrades (FinalGradeRow(..), buildFinalGradeRow, writeFinalGradesXlsx, lastAttendedDate)
import System.FilePath (takeDirectory)
import Gradebook.ExamScores (PrairieLearnRow(..), parsePrairieLearnCSV, extractNetId, groupByStudent, buildExamZones, buildQuestionScores)
import Gradebook.ExamOverrides (ExamOverride(..), parseExamOverridesCSV)
import Gradebook.GradeCalculation (calculateExamScore, ExamGrade(..), ZoneGrade(..), QuestionGrade(..))
import Gradebook.Roster (parseRosterCSV)
import Gradebook.Categories (parseCategoriesCSV)
import Gradebook.Assignments (parseAssignmentsCSV)
import Gradebook.Scores (parseScoresCSV)
import Gradebook.GradeCalculation (calculateGrades, evaluateRequirements)
import Gradebook.Reports (ReportData(..), generateReport)
import qualified Data.HashMap.Strict as HM

-- | Open database connection based on config
-- Returns a ConnWrapper to allow different backend types
openConnection :: Config -> IO ConnWrapper
openConnection config = case dbType config of
  SQLite -> do
    conn <- connectSqlite3 (T.unpack $ database config)
    -- Enable foreign key constraints (must be done per connection)
    _ <- quickQuery' conn "PRAGMA foreign_keys = ON" []
    return (ConnWrapper conn)
  PostgreSQL -> do
    let connStr = "dbname=" ++ T.unpack (database config)
    conn <- connectPostgreSQL connStr
    -- Suppress NOTICE-level chatter (e.g. "relation already exists, skipping"
    -- from CREATE TABLE IF NOT EXISTS during initDatabase).
    _ <- run conn "SET client_min_messages = WARNING" []
    return (ConnWrapper conn)

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

  -- Get valid assignment slugs and student netids for pre-validation
  validAssignments <- getAllAssignmentSlugs conn
  validStudents <- getAllStudentNetids conn

  let assignmentSet = Set.fromList validAssignments
      studentSet = Set.fromList validStudents

  -- Validate scores and partition into valid/invalid
  let (validScores, warnings, errors) = validateScores assignmentSet studentSet scores

  -- Print warnings for missing students
  mapM_ putStrLn warnings

  -- Error out if any assignments are missing
  case errors of
    (err:_) -> do
      putStrLn err
      disconnect conn
      exitFailure
    [] -> return ()

  -- Diff against existing scores so we can show what actually changed
  -- and skip writes for unchanged rows.
  existing <- getAllScores conn
  let (newScores, updatedScores, unchangedCount) = diffScores existing validScores

  mapM_ (insertScore conn) (newScores ++ updatedScores)

  putStrLn $ "  " ++ show unchangedCount ++ " unchanged, "
                 ++ show (length updatedScores) ++ " updated, "
                 ++ show (length newScores) ++ " new"

  -- If the update list is short, list each change so the user can eyeball it.
  -- Above ~20 it just becomes noise.
  when (not (null updatedScores) && length updatedScores <= 20) $ do
    putStrLn "Updated:"
    mapM_ (printChange existing) updatedScores

  -- Commit and close
  commit conn
  disconnect conn

  putStrLn $ "Processed " ++ show (length validScores) ++ " score entries"
  when (not $ null warnings) $
    putStrLn $ "Skipped " ++ show (length warnings) ++ " scores for unknown students"
  where
    diffScores existing scores = go [] [] 0 scores
      where
        go new upd unch [] = (reverse new, reverse upd, unch)
        go new upd unch (s:rest) =
          let key = (scoreNetId s, scoreAssignment s)
              incoming = (scoreValue s, scoreExcused s)
          in case M.lookup key existing of
               Nothing -> go (s:new) upd unch rest
               Just current
                 | scoresEq current incoming -> go new upd (unch + 1) rest
                 | otherwise                 -> go new (s:upd) unch rest

    -- Compare incoming (Double, parsed from CSV) against the stored value
    -- after rounding through Float, since the scores column is `real`
    -- (single-precision) in PostgreSQL. Without this, a re-load of the
    -- same CSV reports every row as updated due to representation drift.
    scoresEq (Just a, ax) (Just b, bx) =
      ax == bx && (realToFrac a :: Float) == (realToFrac b :: Float)
    scoresEq (Nothing, ax) (Nothing, bx) = ax == bx
    scoresEq _ _ = False

    printChange existing s =
      let key = (scoreNetId s, scoreAssignment s)
          oldVal = case M.lookup key existing of
            Just (Just v, _) -> show v
            Just (Nothing, _) -> "(none)"
            Nothing -> "(none)"
          newVal = maybe "(none)" show (scoreValue s)
      in putStrLn $ "  " ++ T.unpack (scoreNetId s) ++ " "
                          ++ T.unpack (scoreAssignment s) ++ ": "
                          ++ oldVal ++ " -> " ++ newVal

-- | Validate scores against known assignments and students
-- Returns (valid scores, warnings for missing students, errors for missing assignments)
validateScores :: Set.Set T.Text -> Set.Set T.Text -> [Score] -> ([Score], [String], [String])
validateScores assignmentSet studentSet = foldr checkScore ([], [], [])
  where
    checkScore score (valid, warns, errs)
      -- Check assignment first - missing assignment is an error
      | not (Set.member (scoreAssignment score) assignmentSet) =
          let err = "Error: Assignment '" ++ T.unpack (scoreAssignment score) ++ "' not found in database"
          in (valid, warns, err : errs)
      -- Check student - missing student is a warning
      | not (Set.member (scoreNetId score) studentSet) =
          let warn = "Warning: Student '" ++ T.unpack (scoreNetId score) ++ "' not found in database, skipping score"
          in (valid, warn : warns, errs)
      -- Both valid
      | otherwise = (score : valid, warns, errs)

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

-- | Write a final-grade .xlsx for every student. Pulls letter grades from
-- the configured grade-thresholds and the same per-student total used by
-- the report. F students get a last-attended-date column filled from the
-- most recent assignment they have a real score for.
runFinalGrades :: FilePath -> IO ()
runFinalGrades outPath = do
  config <- loadConfig "config.yaml"

  let tc = termCode config
  case tc of
    Nothing -> do
      putStrLn "Error: term-code is not set in config.yaml"
      exitFailure
    Just term -> do
      case grading config of
        Nothing -> do
          putStrLn "Error: No grading configuration found in config.yaml"
          exitFailure
        Just gradingCfg -> do
          let thresholds = gradeThresholds gradingCfg
          when (null thresholds) $ do
            putStrLn "Error: grade-thresholds is empty in config.yaml; cannot assign letter grades"
            exitFailure

          conn <- openConnection config
          students <- getAllStudentIdentifiers conn
          rows <- mapM (buildRowForStudent conn gradingCfg thresholds) students
          let outRows = [r | Just r <- rows]

          when (null outRows) $ do
            putStrLn "No students to write."
            disconnect conn
            exitFailure

          -- Warn about empty CRNs.
          let missingCrn = [n | (n, _, c, _) <- students, T.null c]
          when (not (null missingCrn)) $ do
            putStrLn $ "Warning: " ++ show (length missingCrn) ++
                       " student(s) have no CRN: " ++
                       T.unpack (T.intercalate ", " (take 5 missingCrn)) ++
                       (if length missingCrn > 5 then ", ..." else "")

          -- Warn about F students with no last-attended date — registrar
          -- needs that column for failing grades, so flag them for manual
          -- follow-up.
          let fMissingDate = [fgrUin r | r <- outRows, fgrLetterGrade r == "F", fgrLastAttended r == Nothing]
          when (not (null fMissingDate)) $ do
            putStrLn $ "Warning: " ++ show (length fMissingDate) ++
                       " F student(s) have no last-attended date " ++
                       "(no scored assignments). UINs: " ++
                       T.unpack (T.intercalate ", " fMissingDate)

          -- Ensure output directory exists.
          let outDir = takeDirectory outPath
          when (not (null outDir)) $
            System.Directory.createDirectoryIfMissing True outDir

          writeFinalGradesXlsx outPath term outRows
          disconnect conn
          putStrLn $ "Wrote " ++ show (length outRows) ++ " rows to " ++ outPath
  where
    buildRowForStudent conn gradingCfg thresholds (netid, studentUin, studentCrn, _name) = do
      reportData <- buildStudentReportData conn gradingCfg netid
      lastDate <- lastAttendedDate conn netid
      return $ buildFinalGradeRow studentUin studentCrn reportData thresholds lastDate

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

      -- Generate the report
      report <- generateStudentReport conn gradingCfg netid

      disconnect conn

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

-- | Filter categories by student's credit hours and rescale weights
filterAndRescaleCategories :: Maybe Int -> GradingConfig -> GradingConfig
filterAndRescaleCategories Nothing cfg = cfg  -- No credit hours info, use all categories
filterAndRescaleCategories (Just creditHrs) cfg =
  let
    cats = categories cfg
    -- Filter out categories that don't apply to this student
    applicableCats = HM.filter (\c -> case categoryCreditHours c of
        Nothing -> True
        Just hours -> creditHrs `elem` hours
      ) cats
    -- Rescale weights to sum to 1.0
    totalWeight = sum $ map categoryWeight $ HM.elems applicableCats
    rescaledCats = if totalWeight > 0
      then HM.map (\c -> c { categoryWeight = categoryWeight c / totalWeight }) applicableCats
      else applicableCats
  in cfg { categories = rescaledCats }

-- | Generate a complete report for a student (shared logic for single and bulk reports)
generateStudentReport :: IConnection conn => conn -> GradingConfig -> T.Text -> IO T.Text
generateStudentReport conn gradingCfg netid = do
  reportData <- buildStudentReportData conn gradingCfg netid
  return $ generateReport reportData

-- | Build the ReportData for a student without rendering. Useful for callers
-- that want the numeric pieces (e.g. final-grade spreadsheet generation).
buildStudentReportData :: IConnection conn => conn -> GradingConfig -> T.Text -> IO ReportData
buildStudentReportData conn gradingCfg netid = do
  -- Look up student info
  studentCreditHours <- getStudentCreditHours conn netid
  studentName <- getStudentName conn netid

  -- Filter categories by credit hours and rescale weights
  let adjustedCfg = filterAndRescaleCategories studentCreditHours gradingCfg

  -- Get scores for student
  scores <- getScoresForStudent conn netid

  -- Filter out scores for categories that don't apply to this student
  let applicableCatSlugs = HM.keys (categories adjustedCfg)
      applicableScores = filter (\(_, _, _, _, cat, _, _) -> cat `elem` applicableCatSlugs) scores

  -- Get exam configs - use configured exams if available, otherwise auto-detect from DB
  examConfigs <- if null (exams adjustedCfg)
    then do
      -- Auto-detect exams from database
      examSlugs <- getAllExamSlugs conn
      return [ExamConfig slug slug Nothing MaxScore Nothing Nothing | slug <- examSlugs]
    else return (exams adjustedCfg)

  -- Get exam grades for all exams
  examGradeResults <- mapM (buildExamGradeForStudent conn adjustedCfg netid) examConfigs

  -- Pair exam grades with their per-exam weights
  let examCategoryWeight = maybe 0 categoryWeight $ HM.lookup "exams" (categories adjustedCfg)
      numExams = let n = length examConfigs in if n > 0 then n else 1
      perExamWeight = examCategoryWeight / fromIntegral numExams
      examGradesWithWeights =
        [ (grade, perExamWeight)
        | (_, maybeGrade) <- zip examConfigs examGradeResults
        , Just grade <- [maybeGrade]
        ]

  -- Calculate grades
  let categoryGrades = calculateGrades adjustedCfg applicableScores

  -- Calculate requirement results (for pass-fail modes)
  let reqResults = evaluateRequirements (passRequirements adjustedCfg) categoryGrades

  return ReportData
    { rdNetid = netid
    , rdStudentName = studentName
    , rdGradingConfig = adjustedCfg
    , rdCategoryGrades = categoryGrades
    , rdExamGrades = examGradesWithWeights
    , rdRequirements = reqResults
    , rdThresholds = gradeThresholds adjustedCfg
    }

-- | Build ExamGrade for a student from database
buildExamGradeForStudent :: IConnection conn => conn -> GradingConfig -> T.Text -> ExamConfig -> IO (Maybe ExamGrade)
buildExamGradeForStudent conn gradingCfg netid examCfg = do
  -- Get zones for this exam
  zones <- getExamZonesForExam conn (examSlug examCfg)

  if null zones
    then return Nothing
    else do
      -- Get question scores for original exam
      originalScores <- getExamQuestionScoresForStudent conn netid (examSlug examCfg)

      -- Get retake scores if configured
      retakeScores <- case examRetakeSlug examCfg of
        Nothing -> return []
        Just retakeSlug -> getExamQuestionScoresForStudent conn netid retakeSlug

      -- Get final scores if configured
      finalScores <- case examFinalSlug examCfg of
        Nothing -> return []
        Just finalSlug -> getExamQuestionScoresForStudent conn netid finalSlug

      if null originalScores && null retakeScores && null finalScores
        then return Nothing
        else do
          -- Build zone grades
          let zoneGrades = map (buildZoneGrade originalScores retakeScores finalScores (examRetakePolicy examCfg)) zones

          -- Calculate overall percentage (average of zone percentages)
          let overallPct = if null zoneGrades
                           then 0
                           else sum (map zgPercentage zoneGrades) / fromIntegral (length zoneGrades)

          return $ Just ExamGrade
            { egExamSlug = examSlug examCfg
            , egExamTitle = examTitle examCfg
            , egZones = zoneGrades
            , egPercentage = overallPct
            }

-- | Build a ZoneGrade from question scores
buildZoneGrade :: [ExamQuestionScore] -> [ExamQuestionScore] -> [ExamQuestionScore] -> RetakePolicy -> ExamZone -> ZoneGrade
buildZoneGrade originalScores retakeScores finalScores retakePolicy zone =
  let
    zoneNum = ezZoneNumber zone
    -- Filter scores for this zone
    origForZone = filter (\s -> eqsZoneNumber s == zoneNum) originalScores
    retakeForZone = filter (\s -> eqsZoneNumber s == zoneNum) retakeScores
    finalForZone = filter (\s -> eqsZoneNumber s == zoneNum) finalScores

    -- Build question grades
    questionGrades = map (buildQuestionGrade origForZone retakeForZone finalForZone retakePolicy) [1..ezQuestionCount zone]

    -- Calculate zone percentage (average of question combined scores)
    zonePct = if null questionGrades
              then 0
              else sum (map qgCombinedScore questionGrades) / fromIntegral (length questionGrades) / 100
  in ZoneGrade
       { zgZoneNumber = zoneNum
       , zgZoneTitle = ezZoneTitle zone
       , zgQuestions = questionGrades
       , zgPercentage = zonePct
       }

-- | Build a QuestionGrade from scores
buildQuestionGrade :: [ExamQuestionScore] -> [ExamQuestionScore] -> [ExamQuestionScore] -> RetakePolicy -> Int -> QuestionGrade
buildQuestionGrade origScores retakeScores finalScores retakePolicy qNum =
  let
    findScore scores = find (\s -> eqsQuestionNumber s == qNum) scores
    toPercent s = (eqsScore s / eqsMaxPoints s) * 100

    origScore = toPercent <$> findScore origScores
    retakeScore = toPercent <$> findScore retakeScores
    finalScore = toPercent <$> findScore finalScores

    -- Combine scores using policy
    combined = case (origScore, retakeScore) of
      (Nothing, Nothing) -> 0
      (Just o, Nothing) -> o
      (Nothing, Just r) -> r
      (Just o, Just r) -> case retakePolicy of
        MaxScore -> max o r
        MaxIfBetterAvgIfWorse -> if r >= o then r else (o + r) / 2
        WeightedRetake w -> (1 - w) * o + w * r

  in QuestionGrade
       { qgQuestionNumber = qNum
       , qgOriginalScore = origScore
       , qgRetakeScore = retakeScore
       , qgFinalScore = finalScore
       , qgCombinedScore = combined
       }

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
generateAndPushForStudent config gradingCfg prefix (netid, studentName, _, _) = do
  putStrLn $ "Processing: " ++ T.unpack netid ++ " (" ++ T.unpack studentName ++ ")"

  -- Connect to database
  conn <- openConnection config

  -- Generate the report
  report <- generateStudentReport conn gradingCfg netid

  disconnect conn

  -- Push to git
  pushReportToGit (T.unpack netid) prefix report
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

-- | Load exam scores from PrairieLearn CSV
runLoadExam :: String -> FilePath -> IO ()
runLoadExam examSlugStr scoresPath = do
  let examSlugT = T.pack examSlugStr
  putStrLn $ "Loading exam scores for: " ++ examSlugStr
  putStrLn $ "From file: " ++ scoresPath

  -- Load config
  config <- loadConfig "config.yaml"

  -- Check if grading config exists and find exam config
  case grading config of
    Nothing -> do
      putStrLn "Error: No grading configuration found in config.yaml"
      exitFailure
    Just gradingCfg -> do
      -- Find the exam configuration for this slug
      let examConfigs = exams gradingCfg
          -- Check if this is a primary exam or a retake
          (examCfg, isRetake, primarySlug) = findExamConfig examSlugT examConfigs

      case examCfg of
        Nothing -> do
          putStrLn $ "Warning: No exam configuration found for slug '" ++ examSlugStr ++ "'"
          putStrLn "Proceeding without retake policy (scores will be stored but not combined)"
          loadExamScoresSimple config examSlugT scoresPath
        Just cfg -> do
          if isRetake
            then do
              putStrLn $ "This is a retake exam for: " ++ T.unpack primarySlug
              putStrLn $ "Retake policy: " ++ show (examRetakePolicy cfg)
              loadExamScoresWithRetake config cfg examSlugT primarySlug scoresPath
            else do
              putStrLn "This is a primary exam"
              loadExamScoresSimple config examSlugT scoresPath

-- | Find exam config and determine if slug is primary or retake
-- Returns (Maybe ExamConfig, isRetake, primarySlug)
findExamConfig :: T.Text -> [ExamConfig] -> (Maybe ExamConfig, Bool, T.Text)
findExamConfig slug configs =
  -- First check if it's a primary exam slug
  case find (\c -> examSlug c == slug) configs of
    Just cfg -> (Just cfg, False, slug)
    Nothing ->
      -- Check if it's a retake slug
      case find (\c -> examRetakeSlug c == Just slug) configs of
        Just cfg -> (Just cfg, True, examSlug cfg)
        Nothing -> (Nothing, False, slug)

-- | Load exam scores without retake combination (simple case)
loadExamScoresSimple :: Config -> T.Text -> FilePath -> IO ()
loadExamScoresSimple config examSlugT scoresPath = do
  -- Parse PrairieLearn CSV
  result <- parsePrairieLearnCSV scoresPath
  rows <- case result of
    Left err -> do
      putStrLn $ "Error parsing CSV: " ++ err
      exitFailure
    Right r -> return r

  putStrLn $ "Parsed " ++ show (length rows) ++ " question score rows"

  -- Connect to database
  conn <- openConnection config

  -- Initialize database schema (ensures exam tables exist)
  initDatabase conn

  -- Get valid student netids
  validStudents <- getAllStudentNetids conn
  let studentSet = Set.fromList validStudents

  -- Build exam zones from the data
  let zones = buildExamZones examSlugT rows
  putStrLn $ "Found " ++ show (length zones) ++ " exam zones"

  -- Insert exam zones
  mapM_ (insertExamZone conn) zones

  -- Group rows by student and process each
  let byStudent = groupByStudent rows
      studentCount = M.size byStudent

  putStrLn $ "Processing scores for " ++ show studentCount ++ " students..."

  -- Process each student
  (processedCount, totals) <- foldM
    (processStudentExamScoresSimple conn studentSet examSlugT)
    (0, emptyExamDiffTotals)
    (M.toList byStudent)

  reportExamDiffTotals totals

  -- Commit and close
  commit conn
  disconnect conn

  putStrLn $ "Successfully loaded exam scores for " ++ show processedCount ++ " students"

-- | Process exam scores for a single student (simple case, no retake)
processStudentExamScoresSimple :: IConnection conn
                               => conn -> Set.Set T.Text -> T.Text
                               -> (Int, ExamDiffTotals)
                               -> (T.Text, [PrairieLearnRow])
                               -> IO (Int, ExamDiffTotals)
processStudentExamScoresSimple conn studentSet examSlugT (count, totals) (netid, studentRows) = do
  -- Skip if student not in database
  if not (Set.member netid studentSet)
    then do
      putStrLn $ "Warning: Student '" ++ T.unpack netid ++ "' not found, skipping"
      return (count, totals)
    else do
      -- Build incoming question scores from CSV
      let questionScores = buildQuestionScores examSlugT netid studentRows

      -- Diff against existing DB rows: skip overridden questions, only write
      -- rows that are new or actually changed.
      existing <- getExamQuestionScoresForStudent conn netid examSlugT
      let diff = diffExamQuestionScores existing questionScores
      mapM_ (insertExamQuestionScore conn) (edNew diff ++ edUpdated diff)

      -- Calculate overall exam score using ALL incoming questions (including
      -- ones we skipped writing because they were overridden — the override
      -- value is still present in `existing`, but we want the merged effective
      -- score for this student). Use existing override values where present.
      let effective = mergeWithOverrides existing questionScores
          scoreTuples = [(eqsZoneNumber s, eqsQuestionNumber s, eqsScore s, eqsMaxPoints s) | s <- effective]
          examScore = calculateExamScore MaxScore Nothing scoreTuples [] []

      -- Update the exam assignment score
      updateAssignmentScore conn netid examSlugT examScore

      return (count + 1, totals `addExamDiff` diff)

-- | Load exam scores with retake combination
loadExamScoresWithRetake :: Config -> ExamConfig -> T.Text -> T.Text -> FilePath -> IO ()
loadExamScoresWithRetake config examCfg retakeSlug primarySlug scoresPath = do
  -- Parse PrairieLearn CSV
  result <- parsePrairieLearnCSV scoresPath
  rows <- case result of
    Left err -> do
      putStrLn $ "Error parsing CSV: " ++ err
      exitFailure
    Right r -> return r

  putStrLn $ "Parsed " ++ show (length rows) ++ " question score rows"

  -- Connect to database
  conn <- openConnection config

  -- Initialize database schema
  initDatabase conn

  -- Get valid student netids
  validStudents <- getAllStudentNetids conn
  let studentSet = Set.fromList validStudents

  -- Build exam zones (retake should have same structure as primary)
  let zones = buildExamZones retakeSlug rows
  putStrLn $ "Found " ++ show (length zones) ++ " exam zones"

  -- Insert exam zones for retake
  mapM_ (insertExamZone conn) zones

  -- Group rows by student
  let byStudent = groupByStudent rows
      studentCount = M.size byStudent

  putStrLn $ "Processing retake scores for " ++ show studentCount ++ " students..."

  -- Process each student with retake logic
  (processedCount, totals) <- foldM
    (processStudentRetakeScores conn studentSet examCfg retakeSlug primarySlug)
    (0, emptyExamDiffTotals)
    (M.toList byStudent)

  reportExamDiffTotals totals

  -- Commit and close
  commit conn
  disconnect conn

  putStrLn $ "Successfully processed retake scores for " ++ show processedCount ++ " students"

-- | Process retake scores for a single student and combine with original
processStudentRetakeScores :: IConnection conn
                           => conn -> Set.Set T.Text -> ExamConfig -> T.Text -> T.Text
                           -> (Int, ExamDiffTotals)
                           -> (T.Text, [PrairieLearnRow])
                           -> IO (Int, ExamDiffTotals)
processStudentRetakeScores conn studentSet examCfg retakeSlug primarySlug (count, totals) (netid, studentRows) = do
  -- Skip if student not in database
  if not (Set.member netid studentSet)
    then do
      putStrLn $ "Warning: Student '" ++ T.unpack netid ++ "' not found, skipping"
      return (count, totals)
    else do
      -- Build incoming retake scores
      let retakeScores = buildQuestionScores retakeSlug netid studentRows

      -- Diff against existing retake DB rows: skip overrides, write only changes
      existingRetake <- getExamQuestionScoresForStudent conn netid retakeSlug
      let diff = diffExamQuestionScores existingRetake retakeScores
      mapM_ (insertExamQuestionScore conn) (edNew diff ++ edUpdated diff)

      -- Get original exam scores (already in DB; diff doesn't apply here)
      originalScores <- getExamQuestionScoresForStudent conn netid primarySlug

      -- Use override-aware merge so a previously-applied override on the
      -- retake side survives a re-load of the same CSV.
      let effectiveRetake = mergeWithOverrides existingRetake retakeScores
          origTuples    = [(eqsZoneNumber s, eqsQuestionNumber s, eqsScore s, eqsMaxPoints s) | s <- originalScores]
          retakeTuples  = [(eqsZoneNumber s, eqsQuestionNumber s, eqsScore s, eqsMaxPoints s) | s <- effectiveRetake]

      -- Calculate combined score using retake policy
      let combinedScore = calculateExamScore
            (examRetakePolicy examCfg)
            Nothing  -- No final policy for now
            origTuples
            retakeTuples
            []  -- No final scores

      -- Update the primary exam assignment score
      updateAssignmentScore conn netid primarySlug combinedScore

      putStrLn $ "  " ++ T.unpack netid ++ ": combined score = " ++ show (combinedScore * 100) ++ "%"

      return (count + 1, totals `addExamDiff` diff)

-- | Result of diffing an incoming batch of question scores against the DB
-- for a single (netid, exam_slug). Override rows are removed from the
-- incoming batch entirely (counted in edSkippedOverride) and not written.
data ExamDiff = ExamDiff
  { edNew              :: [ExamQuestionScore]   -- ^ rows not in DB
  , edUpdated          :: [ExamQuestionScore]   -- ^ rows whose values changed
  , edUnchangedCount   :: !Int                  -- ^ rows that match the DB
  , edSkippedOverride  :: !Int                  -- ^ rows skipped due to override
  , edChanges          :: [(ExamQuestionScore, Maybe ExamQuestionScore)]
    -- ^ for reporting: (incoming, prior) — prior is Nothing for new rows
  }

-- | Aggregate counters across all students in a load.
data ExamDiffTotals = ExamDiffTotals
  { tNew              :: !Int
  , tUpdated          :: !Int
  , tUnchanged        :: !Int
  , tSkippedOverride  :: !Int
  , tChanges          :: [(ExamQuestionScore, Maybe ExamQuestionScore)]
  }

emptyExamDiffTotals :: ExamDiffTotals
emptyExamDiffTotals = ExamDiffTotals 0 0 0 0 []

addExamDiff :: ExamDiffTotals -> ExamDiff -> ExamDiffTotals
addExamDiff t d = ExamDiffTotals
  { tNew             = tNew t + length (edNew d)
  , tUpdated         = tUpdated t + length (edUpdated d)
  , tUnchanged       = tUnchanged t + edUnchangedCount d
  , tSkippedOverride = tSkippedOverride t + edSkippedOverride d
  , tChanges         = tChanges t ++ edChanges d
  }

-- | Diff incoming question scores against existing DB rows.
diffExamQuestionScores :: [ExamQuestionScore] -> [ExamQuestionScore] -> ExamDiff
diffExamQuestionScores existing incoming =
  let existingMap = M.fromList [((eqsZoneNumber s, eqsQuestionNumber s), s) | s <- existing]
      go new upd unch skp chg [] =
        ExamDiff (reverse new) (reverse upd) unch skp (reverse chg)
      go new upd unch skp chg (s:rest) =
        let key = (eqsZoneNumber s, eqsQuestionNumber s)
        in case M.lookup key existingMap of
             Just current
               | isJust (eqsOverrideReason current) ->
                   -- Question has a manual override; leave it alone.
                   go new upd unch (skp + 1) chg rest
             Just current
               | examQuestionEq current s ->
                   go new upd (unch + 1) skp chg rest
               | otherwise ->
                   go new (s:upd) unch skp ((s, Just current):chg) rest
             Nothing ->
                   go (s:new) upd unch skp ((s, Nothing):chg) rest
  in go [] [] 0 0 [] incoming

-- | Equality comparison for an exam question score. Score and max_points
-- round through Float because the columns are REAL (single-precision) in
-- PostgreSQL — without this, a re-load reports every row as updated.
examQuestionEq :: ExamQuestionScore -> ExamQuestionScore -> Bool
examQuestionEq a b =
     eqsQuestionId a == eqsQuestionId b
  && (realToFrac (eqsScore a) :: Float)     == (realToFrac (eqsScore b) :: Float)
  && (realToFrac (eqsMaxPoints a) :: Float) == (realToFrac (eqsMaxPoints b) :: Float)

-- | When recomputing the overall exam score, substitute existing overridden
-- DB rows for any incoming questions that have an override applied. This
-- prevents a re-load of the CSV from silently reverting an override's
-- contribution to the assignment score.
mergeWithOverrides :: [ExamQuestionScore] -> [ExamQuestionScore] -> [ExamQuestionScore]
mergeWithOverrides existing incoming =
  let existingMap = M.fromList [((eqsZoneNumber s, eqsQuestionNumber s), s) | s <- existing]
  in [ case M.lookup (eqsZoneNumber s, eqsQuestionNumber s) existingMap of
         Just cur | isJust (eqsOverrideReason cur) -> cur
         _                                         -> s
     | s <- incoming
     ]

-- | Print aggregate diff totals after processing all students. Lists each
-- change individually if there are 20 or fewer (so a one-off regrade is
-- easy to eyeball).
reportExamDiffTotals :: ExamDiffTotals -> IO ()
reportExamDiffTotals t = do
  putStrLn $ "  " ++ show (tUnchanged t) ++ " unchanged, "
                  ++ show (tUpdated t)   ++ " updated, "
                  ++ show (tNew t)       ++ " new"
  when (tSkippedOverride t > 0) $
    putStrLn $ "  " ++ show (tSkippedOverride t)
                    ++ " skipped (manual override in place)"
  let changeCount = tNew t + tUpdated t
  when (changeCount > 0 && changeCount <= 20) $ do
    putStrLn "Changes:"
    mapM_ printExamChange (tChanges t)
  where
    printExamChange (s, mPrior) =
      let oldVal = case mPrior of
            Just p  -> show (eqsScore p) ++ "/" ++ show (eqsMaxPoints p)
            Nothing -> "(none)"
          newVal = show (eqsScore s) ++ "/" ++ show (eqsMaxPoints s)
      in putStrLn $ "  " ++ T.unpack (eqsNetId s)
                  ++ " z" ++ show (eqsZoneNumber s)
                  ++ " q" ++ show (eqsQuestionNumber s)
                  ++ ": " ++ oldVal ++ " -> " ++ newVal

-- | Load exam question score overrides from CSV
runLoadExamOverrides :: String -> FilePath -> IO ()
runLoadExamOverrides examSlugStr overridesPath = do
  let examSlugT = T.pack examSlugStr
  putStrLn $ "Loading exam overrides for: " ++ examSlugStr
  putStrLn $ "From file: " ++ overridesPath

  -- Load config
  config <- loadConfig "config.yaml"

  -- Parse overrides CSV
  result <- parseExamOverridesCSV overridesPath
  overrides <- case result of
    Left err -> do
      putStrLn $ "Error parsing CSV: " ++ err
      exitFailure
    Right o -> return o

  putStrLn $ "Parsed " ++ show (length overrides) ++ " override entries"

  -- Connect to database
  conn <- openConnection config

  -- Initialize database schema (ensures tables exist with new column)
  initDatabase conn

  -- Get valid student netids for validation
  validStudents <- getAllStudentNetids conn
  let studentSet = Set.fromList validStudents

  -- Apply each override
  appliedCount <- foldM (applyOverride conn studentSet examSlugT) 0 overrides

  -- Now recalculate exam scores for affected students
  let affectedNetids = Set.fromList [eoNetId o | o <- overrides, Set.member (eoNetId o) studentSet]
  putStrLn $ "Recalculating exam scores for " ++ show (Set.size affectedNetids) ++ " affected students..."

  -- Get exam config for retake policy
  case grading config of
    Nothing -> putStrLn "Warning: No grading config, using simple score calculation"
    Just gradingCfg -> do
      let examCfg = find (\c -> examSlug c == examSlugT) (exams gradingCfg)
      mapM_ (recalculateStudentExamScore conn examSlugT examCfg) (Set.toList affectedNetids)

  -- Commit and close
  commit conn
  disconnect conn

  putStrLn $ "Successfully applied " ++ show appliedCount ++ " overrides"

-- | Apply a single override (helper)
applyOverride :: IConnection conn => conn -> Set.Set T.Text -> T.Text -> Int -> ExamOverride -> IO Int
applyOverride conn studentSet examSlugT count override = do
  let netid = eoNetId override
  if not (Set.member netid studentSet)
    then do
      putStrLn $ "Warning: Student '" ++ T.unpack netid ++ "' not found, skipping"
      return count
    else do
      applyExamQuestionOverride conn
        netid
        examSlugT
        (eoZoneNumber override)
        (eoQuestionNumber override)
        (eoScore override)
        (eoMaxPoints override)
        (eoReason override)
      putStrLn $ "  Applied override for " ++ T.unpack netid ++
                 " zone " ++ show (eoZoneNumber override) ++
                 " question " ++ show (eoQuestionNumber override) ++
                 ": " ++ T.unpack (eoReason override)
      return (count + 1)

-- | Recalculate a student's exam score after overrides
recalculateStudentExamScore :: IConnection conn => conn -> T.Text -> Maybe ExamConfig -> T.Text -> IO ()
recalculateStudentExamScore conn examSlugT maybeExamCfg netid = do
  -- Get all question scores for this student on this exam
  questionScores <- getExamQuestionScoresForStudent conn netid examSlugT

  if null questionScores
    then putStrLn $ "  Warning: No scores found for " ++ T.unpack netid
    else do
      -- Get retake scores if configured
      retakeScores <- case maybeExamCfg >>= examRetakeSlug of
        Nothing -> return []
        Just retakeSlug -> getExamQuestionScoresForStudent conn netid retakeSlug

      -- Calculate combined score
      let scoreTuples = [(eqsZoneNumber s, eqsQuestionNumber s, eqsScore s, eqsMaxPoints s) | s <- questionScores]
          retakeTuples = [(eqsZoneNumber s, eqsQuestionNumber s, eqsScore s, eqsMaxPoints s) | s <- retakeScores]
          retakePolicy = maybe MaxScore examRetakePolicy maybeExamCfg
          newScore = calculateExamScore retakePolicy Nothing scoreTuples retakeTuples []

      -- Update the exam score
      updateAssignmentScore conn netid examSlugT newScore
      putStrLn $ "  " ++ T.unpack netid ++ ": new score = " ++ show (newScore * 100) ++ "%"

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
  _ <- run conn "UPDATE assignments SET collected = TRUE WHERE slug = ?" [toSql slug]
  return ()

-- | Update collected field for an assignment if slug matches
updateCollectedField :: [T.Text] -> Assignment -> Assignment
updateCollectedField slugs assignment =
  if assignmentSlug assignment `elem` slugs
    then assignment { assignmentCollected = True }
    else assignment
