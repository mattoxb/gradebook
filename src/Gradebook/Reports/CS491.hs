{-# LANGUAGE OverloadedStrings #-}

module Gradebook.Reports.CS491
  ( formatReport
  ) where

import qualified Data.Text as T
import Text.Printf (printf)
import Gradebook.GradeCalculation
  ( CategoryGrade(..), AssignmentGrade(..), RequirementResult(..)
  )

-- | Generate a CS 491 CAP formatted grade report
-- Pass/fail focused: shows assignment completion status and requirement
-- progress without misleading percentages.
formatReport :: T.Text                -- ^ Student netid
             -> Maybe T.Text          -- ^ Student name (if available)
             -> [CategoryGrade]       -- ^ Category grades
             -> [RequirementResult]   -- ^ Requirement results
             -> T.Text
formatReport netid maybeName categoryGrades requirements =
  let
    header = case maybeName of
      Just studentName -> "# Grade Report for " <> studentName <> " (" <> netid <> ")"
      Nothing -> "# Grade Report for " <> netid

    categorySections = concatMap formatCategorySection categoryGrades

    reqSection = formatRequirements requirements

    outcome = formatOutcome requirements

  in T.unlines $ [header, ""] ++ categorySections ++ reqSection ++ [""] ++ outcome ++ [""]

-- | Format a category section showing completion status (not percentages)
formatCategorySection :: CategoryGrade -> [T.Text]
formatCategorySection cg =
  let
    catTitle = cgCategory cg
    sectionHeader = "## " <> catTitle

    assignments = cgAssignments cg

    -- Calculate max title width for alignment
    maxTitleWidth = if null assignments then 0
                    else maximum $ map (T.length . agTitle) assignments

    formattedAssignments = map (formatAssignmentStatus maxTitleWidth) assignments

    -- Summary: completed/total
    completed = length $ filter isCompleted assignments
    excused = length $ filter agExcused assignments
    total = length assignments - excused
    summaryLine = T.pack $ printf "  %d/%d completed" completed total
    excusedLine = if excused > 0
                  then [T.pack $ printf "  (%d excused)" excused]
                  else []

  in ["", sectionHeader, ""]
     ++ formattedAssignments
     ++ [""]
     ++ [summaryLine]
     ++ excusedLine
     ++ [""]

-- | Format an assignment showing its completion status
formatAssignmentStatus :: Int -> AssignmentGrade -> T.Text
formatAssignmentStatus titleWidth ag =
  let
    title = agTitle ag
    paddedTitle = title <> T.replicate (titleWidth - T.length title) " "
    status = assignmentStatus ag
  in "  " <> paddedTitle <> "  " <> status

-- | Determine the status indicator for an assignment.
-- Completed → green check; absent/incomplete → blank so the paragraph view
-- (markdown collapses lines) reads cleanly. Pending/excused keep bracketed
-- text so they remain visually distinct from a routine miss.
assignmentStatus :: AssignmentGrade -> T.Text
assignmentStatus ag
  | agExcused ag = "[excused]"
  | agPending ag = "[pending]"
  | isCompleted ag = "✅"
  | otherwise = ""

-- | Check if an assignment is completed (has a score > 0)
isCompleted :: AssignmentGrade -> Bool
isCompleted ag = not (agExcused ag) && not (agPending ag) && maybe False (> 0) (agScore ag)

-- | Format the requirements section
formatRequirements :: [RequirementResult] -> [T.Text]
formatRequirements reqs =
  ["## Requirements", ""] ++ map formatRequirement reqs

-- | Format a single requirement
formatRequirement :: RequirementResult -> T.Text
formatRequirement rr =
  let
    status = if rrPassed rr then "[PASS]" else "[FAIL]"
    excusedNote = if rrExcused rr > 0
                  then T.pack $ printf " (%d excused)" (rrExcused rr)
                  else ""
  in T.pack $ printf "  %s %s: %d/%d achieved, %d required%s"
       (status :: String)
       (T.unpack $ rrName rr)
       (rrActual rr)
       (rrTotal rr)
       (rrRequired rr)
       (T.unpack excusedNote)

-- | Format the overall outcome
formatOutcome :: [RequirementResult] -> [T.Text]
formatOutcome reqs =
  let
    allPassed = all rrPassed reqs
    outcome = if allPassed then "PASSING" else "NOT PASSING"
  in [T.pack $ printf "## Current Status: %s" (outcome :: String)]
