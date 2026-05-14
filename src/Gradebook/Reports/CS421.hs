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
    -- Mirrors calculateCategoryGrade: missings count as zero and are
    -- eligible to be dropped; pending and excused are not.
    eligible = filter (\ag -> not (agExcused ag) && not (agPending ag)) assignments
    -- Sort by percentage (ascending) to find lowest
    sorted = sortOn (\ag -> maybe 0 id (agScore ag) / fromIntegral (agMaxPoints ag)) eligible
    dropped = take dropCount sorted
    droppedScores = map (\ag ->
        if isNothing (agScore ag)
          then T.pack "(missing)"
          else T.pack $ printf "%.2f" (maybe 0 id (agScore ag))
      ) dropped

    dropLabel = if dropCount == 1
                then "Dropping lowest score:"
                else T.pack $ printf "Dropping %d lowest scores:" dropCount
    droppedLine = "  " <> T.intercalate " " droppedScores <> " "
  in [dropLabel, droppedLine, ""]

-- | Format an exam grade section with zone (and per-question) breakdown.
-- When the exam has retake or final scores anywhere, each zone is followed
-- by per-question rows so a student can see why combined != avg(orig, retake)
-- — the combined column is per-question max, not zone-average max.
formatExamGradeSection :: (ExamGrade, Double) -> [T.Text]
formatExamGradeSection (examGrade, weight) =
  let
    weightPct = weight * 100
    sectionHeader = T.pack $ printf "## %s - Worth %s" (T.unpack $ egExamTitle examGrade) (formatWeight weightPct)

    zones = egZones examGrade

    hasRetake = any (any (not . isNothing . qgRetakeScore) . zgQuestions) zones
    hasFinal  = any (any (not . isNothing . qgFinalScore)  . zgQuestions) zones
    -- Always render per-question rows. Students use these to decide which
    -- questions to redo on a retake, and hand-graded scores live at the
    -- question level. The retake/final *columns* still gate on the booleans
    -- above; we only fan questions out when needed.
    showQuestions = True

    -- Title column width adapts to the longest zone title in this exam.
    -- Question rows render as "  Qn", so we need at least 4 + width of the
    -- largest question number; for typical exams with ≤ 9 questions/zone
    -- that is 4 chars.
    maxQNum = maximum (1 : [qgQuestionNumber q | z <- zones, q <- zgQuestions z])
    qLabelWidth = T.length (T.pack ("  Q" ++ show maxQNum))
    titleWidth = maximum
      [ T.length "Zone"
      , qLabelWidth
      , maximum (0 : [T.length (zgZoneTitle z) | z <- zones])
      ]

    colHeaders = buildExamColHeaders titleWidth hasRetake hasFinal
    bodyRows   = concatMap (formatZoneSection titleWidth showQuestions hasRetake hasFinal) zones

    totalLine = T.pack $ printf " - %s Score %%: %.2f%%" (T.unpack $ egExamTitle examGrade) (egPercentage examGrade * 100)

  in ["", sectionHeader, ""]
     ++ [colHeaders]
     ++ bodyRows
     ++ ["", totalLine]
     ++ [""]

-- Numeric column widths chosen so the headers fit without overflowing.
zoneScoreWidth, zoneRetakeWidth, zoneCombinedWidth, zoneFinalWidth :: Int
zoneScoreWidth    = 7   -- "Score %"
zoneRetakeWidth   = 6   -- "Retake"
zoneCombinedWidth = 11  -- "New Score %"
zoneFinalWidth    = 6   -- "Final"

-- | Build exam column headers dynamically based on available data.
buildExamColHeaders :: Int -> Bool -> Bool -> T.Text
buildExamColHeaders titleWidth hasRetake hasFinal =
  let
    baseCols    = [("Zone", titleWidth), ("Score %", zoneScoreWidth)]
    retakeCols  = if hasRetake then [("Retake", zoneRetakeWidth), ("New Score %", zoneCombinedWidth)] else []
    finalCols   = if hasFinal  then [("Final", zoneFinalWidth),   ("New Score %", zoneCombinedWidth)] else []
    allCols = baseCols ++ retakeCols ++ finalCols
    fmt (label, w) = T.justifyRight w ' ' label
  in T.intercalate "  " $ map fmt allCols

-- | Format a zone: a single summary row when there's no retake/final,
-- otherwise a zone-summary row followed by one row per question.
formatZoneSection :: Int -> Bool -> Bool -> Bool -> ZoneGrade -> [T.Text]
formatZoneSection titleWidth showQuestions hasRetake hasFinal zg =
  let summary = formatZoneSummaryRow titleWidth hasRetake hasFinal zg
      qs = if showQuestions
             then map (formatQuestionRow titleWidth hasRetake hasFinal) (zgQuestions zg)
             else []
  in summary : qs

-- | Zone summary row: zone-level averages across the questions in the zone.
formatZoneSummaryRow :: Int -> Bool -> Bool -> ZoneGrade -> T.Text
formatZoneSummaryRow titleWidth hasRetake hasFinal zg =
  let
    questions = zgQuestions zg
    origs   = [s | q <- questions, Just s <- [qgOriginalScore q]]
    retakes = [s | q <- questions, Just s <- [qgRetakeScore q]]
    finals  = [s | q <- questions, Just s <- [qgFinalScore q]]
    avgMaybe xs = if null xs then Nothing else Just (sum xs / fromIntegral (length xs))

    titleCol    = T.justifyRight titleWidth ' ' (zgZoneTitle zg)
    origCol     = formatMaybeScore zoneScoreWidth   (avgMaybe origs)
    retakeCol   = formatMaybeScore zoneRetakeWidth  (avgMaybe retakes)
    finalCol    = formatMaybeScore zoneFinalWidth   (avgMaybe finals)
    combinedCol = T.pack $ printf "%*.2f" zoneCombinedWidth (zgPercentage zg * 100)

    baseParts   = [titleCol, "  ", origCol]
    retakeParts = if hasRetake then ["  ", retakeCol, "  ", combinedCol] else []
    finalParts  = if hasFinal  then ["  ", finalCol,  "  ", combinedCol] else []
  in T.concat $ baseParts ++ retakeParts ++ finalParts

-- | One row per question, indented under its zone. A question that wasn't
-- attempted on a particular pass shows blanks, not 0.00, in that column.
formatQuestionRow :: Int -> Bool -> Bool -> QuestionGrade -> T.Text
formatQuestionRow titleWidth hasRetake hasFinal qg =
  let
    label = T.pack ("  Q" ++ show (qgQuestionNumber qg))
    titleCol    = T.justifyRight titleWidth ' ' label
    origCol     = formatMaybeScore zoneScoreWidth   (qgOriginalScore qg)
    retakeCol   = formatMaybeScore zoneRetakeWidth  (qgRetakeScore qg)
    finalCol    = formatMaybeScore zoneFinalWidth   (qgFinalScore qg)
    combinedCol = T.pack $ printf "%*.2f" zoneCombinedWidth (qgCombinedScore qg)

    baseParts   = [titleCol, "  ", origCol]
    retakeParts = if hasRetake then ["  ", retakeCol, "  ", combinedCol] else []
    finalParts  = if hasFinal  then ["  ", finalCol,  "  ", combinedCol] else []
  in T.concat $ baseParts ++ retakeParts ++ finalParts

formatMaybeScore :: Int -> Maybe Double -> T.Text
formatMaybeScore w Nothing  = T.justifyRight w ' ' "-"
formatMaybeScore w (Just s) = T.pack $ printf "%*.2f" w s

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
