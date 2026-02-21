{-# LANGUAGE OverloadedStrings #-}

module Gradebook.Reports
  ( generateReport
  , formatReport
  ) where

import qualified Data.Text as T
import qualified Data.List as L
import Text.Printf (printf)
import Gradebook.GradeCalculation (CategoryGrade(..), AssignmentGrade(..), formatScore)

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
