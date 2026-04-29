{-# LANGUAGE OverloadedStrings #-}

module Gradebook.Reports.CS421
  ( formatReport
  ) where

import qualified Data.Text as T
import Text.Printf (printf)
import Data.List (sortOn)
import Data.Maybe (isNothing)
import Data.Char (toUpper)
import Gradebook.GradeCalculation
  ( CategoryGrade(..), AssignmentGrade(..), ExamGrade(..), ZoneGrade(..)
  , QuestionGrade(..), calculateLetterGrade
  )
import Gradebook.Config (GradeThreshold(..), CategoryConfig(..), GradingConfig(..))
import qualified Data.HashMap.Strict as HM

-- | Generate a CS 421 formatted grade report
-- Matches the format from previous semesters with markdown headers,
-- drop-lowest display, exam zone tables, and letter grade summary.
formatReport :: T.Text              -- ^ Student netid
             -> Maybe T.Text        -- ^ Student name (if available)
             -> GradingConfig       -- ^ Grading configuration (for weights)
             -> [CategoryGrade]     -- ^ Category grades
             -> [(ExamGrade, Double)] -- ^ Exam grades with weights
             -> [GradeThreshold]    -- ^ Letter grade thresholds
             -> T.Text
formatReport netid maybeName gradingCfg categoryGrades examGrades thresholds =
  let
    -- Header
    header = case maybeName of
      Just studentName -> "# Grade Report for " <> studentName <> " (" <> netid <> ")"
      Nothing -> "# Grade Report for " <> netid

    -- Non-exam category sections
    nonExamCategories = filter (not . isExamCategory) categoryGrades
    categorySections = concatMap (formatCategorySection gradingCfg) nonExamCategories

    -- Exam sections
    examSections = concatMap formatExamGradeSection examGrades

    -- Total score and letter grade
    weightedScores = map cgWeightedScore categoryGrades
    totalWeighted = sum $ map (maybe 0 id) weightedScores
    overallPct = totalWeighted * 100
    letterGrade = if null thresholds
                  then Nothing
                  else Just (calculateLetterGrade thresholds overallPct)

    totalLine = case letterGrade of
      Just lg -> T.pack $ printf "## Total Score: %.2f, Letter Grade: %s" overallPct (T.unpack lg)
      Nothing -> T.pack $ printf "## Total Score: %.2f" overallPct

  in T.unlines $ [header, ""] ++ categorySections ++ examSections ++ ["", "", totalLine, ""]

-- | Check if a category is an exam category (handled separately)
isExamCategory :: CategoryGrade -> Bool
isExamCategory cg = cgCategory cg == "exams"

-- | Format a category section with markdown header and drop-lowest info
formatCategorySection :: GradingConfig -> CategoryGrade -> [T.Text]
formatCategorySection gradingCfg cg =
  let
    catName = cgCategory cg
    catConfig = HM.lookup catName (categories gradingCfg)
    weight = maybe 0 categoryWeight catConfig
    dropCount = maybe 0 categoryDropLowest catConfig
    weightPct = weight * 100

    -- Section header: ## Category Title - Worth X%
    catTitle = cgCategory cg
    sectionHeader = T.pack $ printf "## %s - Worth %s" (T.unpack $ capitalizeFirst catTitle) (formatWeight weightPct)

    assignments = cgAssignments cg

    -- Calculate max title width for alignment
    maxTitleWidth = if null assignments then 0
                    else maximum $ map (T.length . agTitle) assignments

    -- Column headers
    scoreColHeader = "Score"
    colHeader = T.replicate (maxTitleWidth - T.length scoreColHeader) " " <> scoreColHeader <> "  Score"

    -- Format each assignment line (right-aligned title, right-aligned score)
    formattedAssignments = map (formatAssignmentLine maxTitleWidth) assignments

    -- Drop lowest section
    dropSection = if dropCount > 0
                  then formatDropSection cg dropCount
                  else []

    -- Category average
    avgLine = case cgPercentage cg of
      Nothing -> "- " <> capitalizeFirst catTitle <> " Average: N/A"
      Just pct -> T.pack $ printf "- %s Average: %.2f%%" (T.unpack $ capitalizeFirst catTitle) (pct * 100)

  in ["", sectionHeader, ""]
     ++ [colHeader]
     ++ formattedAssignments
     ++ [""]
     ++ dropSection
     ++ [avgLine]
     ++ [""]

-- | Format a single assignment line with right-aligned title and score
formatAssignmentLine :: Int -> AssignmentGrade -> T.Text
formatAssignmentLine titleWidth ag =
  let
    title = agTitle ag
    paddedTitle = T.replicate (titleWidth - T.length title) " " <> title
    score = formatScoreValue ag
  in paddedTitle <> " " <> score

-- | Format score value for display
-- When a bonus is applied, append a "(+N% bonus)" annotation so students
-- can see the extra credit in their report.
formatScoreValue :: AssignmentGrade -> T.Text
formatScoreValue ag
  | agPending ag = "(pending)"
  | agExcused ag = "(excused)"
  | Just s <- agScore ag = T.pack (printf "%6.2f" s) <> bonusAnnotation
  | otherwise = "(missing)"
  where
    bonusAnnotation = case agBonus ag of
      Just b | b /= 0 -> T.pack $ printf " (+%g%% bonus)" (b * 100)
      _               -> ""

-- | Format the "Dropping N lowest scores" section
formatDropSection :: CategoryGrade -> Int -> [T.Text]
formatDropSection cg dropCount =
  let
    assignments = cgAssignments cg
    -- Only consider scored, non-excused assignments for dropping
    scoredAssignments = filter (\ag -> not (agExcused ag) && not (isNothing (agScore ag))) assignments
    -- Sort by percentage (ascending) to find lowest
    sorted = sortOn (\ag -> maybe 0 id (agScore ag) / fromIntegral (agMaxPoints ag)) scoredAssignments
    dropped = take dropCount sorted
    droppedScores = map (\ag -> T.pack $ printf "%.2f" (maybe 0 id (agScore ag))) dropped

    dropLabel = if dropCount == 1
                then "Dropping lowest score:"
                else T.pack $ printf "Dropping %d lowest scores:" dropCount
    droppedLine = "  " <> T.intercalate " " droppedScores <> " "
  in [dropLabel, droppedLine, ""]

-- | Format an exam grade section with zone breakdown table
formatExamGradeSection :: (ExamGrade, Double) -> [T.Text]
formatExamGradeSection (examGrade, weight) =
  let
    weightPct = weight * 100
    sectionHeader = T.pack $ printf "## %s - Worth %s" (T.unpack $ egExamTitle examGrade) (formatWeight weightPct)

    zones = egZones examGrade

    -- Determine which columns to show based on data
    hasRetake = any (any (not . isNothing . qgRetakeScore) . zgQuestions) zones
    hasFinal = any (any (not . isNothing . qgFinalScore) . zgQuestions) zones

    -- Build column headers
    colHeaders = buildExamColHeaders (egExamTitle examGrade) hasRetake hasFinal

    -- Format each zone row
    zoneRows = map (formatZoneRow hasRetake hasFinal) zones

    -- Exam total line
    totalLine = T.pack $ printf " - %s Score %%: %.2f%%" (T.unpack $ egExamTitle examGrade) (egPercentage examGrade * 100)

  in ["", sectionHeader, ""]
     ++ [colHeaders]
     ++ zoneRows
     ++ ["", totalLine]
     ++ [""]

-- | Build exam column headers dynamically based on available data
buildExamColHeaders :: T.Text -> Bool -> Bool -> T.Text
buildExamColHeaders _examTitle hasRetake hasFinal =
  let
    baseCols = ["Zone", "Score %"]
    retakeCols = if hasRetake then ["Retake", "New Score %"] else []
    -- Note: if there's a final, the "New Score %" column is after that
    finalCols = if hasFinal then ["Final", "New Score %"] else []
    allCols = baseCols ++ retakeCols ++ finalCols
  in T.intercalate "  " $ map (T.justifyRight 16 ' ') allCols

-- | Format a zone row in the exam table
formatZoneRow :: Bool -> Bool -> ZoneGrade -> T.Text
formatZoneRow hasRetake hasFinal zg =
  let
    title = zgZoneTitle zg
    pct = zgPercentage zg * 100
    questions = zgQuestions zg

    -- Average original score across questions
    origScores = [s | q <- questions, Just s <- [qgOriginalScore q]]
    avgOrig = if null origScores then 0 else sum origScores / fromIntegral (length origScores)

    -- Average retake score
    retakeScores = [s | q <- questions, Just s <- [qgRetakeScore q]]
    avgRetake = if null retakeScores then 0 else sum retakeScores / fromIntegral (length retakeScores)

    -- Average final score
    finalScores = [s | q <- questions, Just s <- [qgFinalScore q]]
    avgFinal = if null finalScores then 0 else sum finalScores / fromIntegral (length finalScores)

    -- Combined score is the zone percentage
    combined = pct

    -- Format fields
    titleCol = T.justifyRight 24 ' ' title
    origCol = T.pack $ printf "%6.2f" avgOrig
    retakeCol = if null retakeScores then T.pack $ printf "%6.2f" (0 :: Double) else T.pack $ printf "%6.2f" avgRetake
    finalCol = if null finalScores then T.pack $ printf "%6.2f" (0 :: Double) else T.pack $ printf "%6.2f" avgFinal
    combinedCol = T.pack $ printf "%6.2f" combined

    -- Build row
    baseParts = [titleCol, "  ", origCol]
    retakeParts = if hasRetake then ["  ", retakeCol, "  ", combinedCol] else []
    finalParts = if hasFinal then ["  ", finalCol, "  ", combinedCol] else []

  in T.concat $ baseParts ++ retakeParts ++ finalParts

-- | Format a weight value, showing as integer if effectively whole.
-- Uses a tolerance because renormalized weights (e.g. 0.28 / 0.80 * 100)
-- land on values like 34.99999999999999 in IEEE-754 and would otherwise
-- print as 35.00 instead of 35%.
formatWeight :: Double -> String
formatWeight w
  | abs (w - fromIntegral r) < 0.01 = printf "%d%%" r
  | otherwise                       = printf "%.2f%%" w
  where
    r = round w :: Int

-- | Capitalize the first letter of a text
capitalizeFirst :: T.Text -> T.Text
capitalizeFirst t = case T.uncons t of
  Nothing -> t
  Just (c, rest) -> T.cons (toUpper c) rest
