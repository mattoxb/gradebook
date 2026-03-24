{-# LANGUAGE OverloadedStrings #-}

module Gradebook.Reports
  ( generateReport
  , generatePassFailReport
  , generateLetterGradeReport
  , formatReport
  , formatExamSection
  ) where

import qualified Data.Text as T
import qualified Data.List as L
import Text.Printf (printf)
import Gradebook.GradeCalculation (CategoryGrade(..), AssignmentGrade(..), RequirementResult(..), ExamGrade(..), ZoneGrade(..), QuestionGrade(..), formatScore, calculateLetterGrade)
import Gradebook.Config (GradeThreshold(..))

-- | Generate a formatted grade report for a student
generateReport :: T.Text -> [CategoryGrade] -> T.Text
generateReport netid categoryGrades =
  T.unlines $
    [ "Grade report for " <> netid
    , ""
    ] ++ formatAllCategories categoryGrades

-- | Format all categories with their assignments
formatAllCategories :: [CategoryGrade] -> [T.Text]
formatAllCategories categoryGrades =
  let formatted = map formatCategorySection categoryGrades
      overallSection = formatOverallGrade categoryGrades
  in L.intercalate [""] formatted ++ ["", ""] ++ overallSection

-- | Format a single category section
formatCategorySection :: CategoryGrade -> [T.Text]
formatCategorySection cg =
  let
    categoryHeader = cgCategory cg <> ":"
    assignments = cgAssignments cg

    -- Calculate max widths for alignment
    maxTitleWidth = maximum $ map (T.length . agTitle) assignments
    maxScoreWidth = maximum $ map (T.length . formatScore) assignments

    -- Format each assignment
    formattedAssignments = map (formatAssignment maxTitleWidth maxScoreWidth) assignments

    -- Category average
    avgLine = case cgPercentage cg of
      Nothing -> "  Category average: N/A"
      Just pct -> T.pack $ printf "  Category average: %.2f%%" (pct * 100)

  in [categoryHeader] ++ formattedAssignments ++ [avgLine]

-- | Format a single assignment line with alignment
formatAssignment :: Int -> Int -> AssignmentGrade -> T.Text
formatAssignment titleWidth scoreWidth ag =
  let
    title = agTitle ag
    score = formatScore ag
    maxPoints = T.pack $ show $ agMaxPoints ag

    -- Pad title to align columns
    paddedTitle = title <> T.replicate (titleWidth - T.length title) " "
    paddedScore = T.replicate (scoreWidth - T.length score) " " <> score

  in " - " <> paddedTitle <> "  " <> paddedScore <> " / " <> maxPoints

-- | Format overall grade summary
formatOverallGrade :: [CategoryGrade] -> [T.Text]
formatOverallGrade categoryGrades =
  let
    -- Calculate overall grade
    weightedScores = map cgWeightedScore categoryGrades
    totalWeighted = sum $ map (maybe 0 id) weightedScores

    overallPct = totalWeighted

    overallLine = T.pack $ printf "Overall grade: %.2f%%" (overallPct * 100)

    -- Category breakdown
    breakdown = map formatCategoryBreakdown categoryGrades

  in ["Category breakdown:"] ++ breakdown ++ ["", overallLine]

-- | Format a category's contribution to overall grade
formatCategoryBreakdown :: CategoryGrade -> T.Text
formatCategoryBreakdown cg =
  case cgWeightedScore cg of
    Nothing -> "  " <> cgCategory cg <> ": N/A"
    Just weighted -> T.pack $ printf "  %s: %.2f%%" (T.unpack $ cgCategory cg) (weighted * 100)

-- | Public alias for generateReport (for backward compatibility)
formatReport :: T.Text -> [CategoryGrade] -> T.Text
formatReport = generateReport

-- | Generate a pass/fail grade report
generatePassFailReport :: T.Text -> [CategoryGrade] -> [RequirementResult] -> T.Text
generatePassFailReport netid categoryGrades requirements =
  T.unlines $
    [ "Grade report for " <> netid
    , ""
    ] ++ formatAllCategories categoryGrades
      ++ ["", ""]
      ++ formatRequirements requirements
      ++ [""]
      ++ formatPassFailOutcome requirements

-- | Format requirement results
formatRequirements :: [RequirementResult] -> [T.Text]
formatRequirements reqs =
  ["Requirements:"] ++ map formatRequirement reqs

-- | Format a single requirement result
formatRequirement :: RequirementResult -> T.Text
formatRequirement rr =
  let
    status = if rrPassed rr then "[PASS]" else "[FAIL]"
    excusedNote = if rrExcused rr > 0
                  then T.pack $ printf " (%d excused)" (rrExcused rr)
                  else ""
  in T.pack $ printf "  %s %s: %d/%d required, %d/%d achieved%s"
       (status :: String)
       (T.unpack $ rrName rr)
       (rrRequired rr)
       (rrTotal rr)
       (rrActual rr)
       (rrTotal rr)
       (T.unpack excusedNote)

-- | Format pass/fail outcome
formatPassFailOutcome :: [RequirementResult] -> [T.Text]
formatPassFailOutcome reqs =
  let
    allPassed = all rrPassed reqs
    outcome = if allPassed then "PASSING" else "NOT PASSING"
  in [T.pack $ printf "Current status: %s" (outcome :: String)]

-- | Generate a letter grade report
generateLetterGradeReport :: T.Text -> [CategoryGrade] -> [GradeThreshold] -> T.Text
generateLetterGradeReport netid categoryGrades thresholds =
  T.unlines $
    [ "Grade report for " <> netid
    , ""
    ] ++ formatAllCategories categoryGrades
      ++ ["", ""]
      ++ formatLetterGradeOutcome categoryGrades thresholds

-- | Format letter grade outcome
formatLetterGradeOutcome :: [CategoryGrade] -> [GradeThreshold] -> [T.Text]
formatLetterGradeOutcome categoryGrades thresholds =
  let
    -- Calculate overall percentage (same as weighted mode)
    weightedScores = map cgWeightedScore categoryGrades
    totalWeighted = sum $ map (maybe 0 id) weightedScores
    overallPct = totalWeighted * 100  -- convert to percentage

    -- Determine letter grade
    letterGrade = calculateLetterGrade thresholds overallPct

    breakdown = map formatCategoryBreakdown categoryGrades
    overallLine = T.pack $ printf "Overall grade: %.2f%% (%s)" overallPct (T.unpack letterGrade)

  in ["Category breakdown:"] ++ breakdown ++ ["", overallLine]

-- | Format an exam section for the grade report
-- Shows each zone with its questions and scores
formatExamSection :: ExamGrade -> Double -> [T.Text]
formatExamSection examGrade weight =
  let
    header = T.pack $ printf "## %s - Worth %.2f" (T.unpack $ egExamTitle examGrade) (weight * 100)
    headerLine = ""

    -- Column headers
    colHeaders = T.pack $ printf "%22s %6s %6s %11s"
                   ("" :: String) ("Score" :: String) ("Final" :: String) ("New Score" :: String)
    colUnderline = T.pack $ printf "%22s %6s %6s %11s"
                   ("" :: String) ("%" :: String) ("%" :: String) ("%" :: String)

    -- Format each zone
    zoneLines = concatMap formatZoneSection (egZones examGrade)

    -- Overall exam score
    overallLine = T.pack $ printf "\n%22s %6.2f" ("Exam Total:" :: String) (egPercentage examGrade * 100)

  in [header, headerLine, colHeaders, colUnderline] ++ zoneLines ++ [overallLine]

-- | Format a zone section within an exam
formatZoneSection :: ZoneGrade -> [T.Text]
formatZoneSection zoneGrade =
  let
    -- Zone header with overall zone percentage
    zoneHeader = T.pack $ printf "%20s: %6.2f"
                   (T.unpack $ zgZoneTitle zoneGrade)
                   (zgPercentage zoneGrade * 100)

    -- If zone has multiple questions, show individual question lines
    questionLines = if length (zgQuestions zoneGrade) > 1
      then map formatQuestionLine (zgQuestions zoneGrade)
      else []

  in [zoneHeader] ++ questionLines

-- | Format a single question line
formatQuestionLine :: QuestionGrade -> T.Text
formatQuestionLine qg =
  let
    qLabel = T.pack $ printf "    Question %d" (qgQuestionNumber qg)
    origScore :: String
    origScore = maybe "--" (printf "%6.2f") (qgOriginalScore qg)
    finalScore' :: String
    finalScore' = maybe "--" (printf "%6.2f") (qgFinalScore qg)
    combined :: String
    combined = printf "%6.2f" (qgCombinedScore qg)
  in T.pack $ printf "%20s %6s %6s %11s" (T.unpack qLabel) origScore finalScore' combined
