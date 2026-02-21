{-# LANGUAGE OverloadedStrings #-}

module Gradebook.GradeCalculation
  ( CategoryGrade(..)
  , AssignmentGrade(..)
  , calculateGrades
  , formatScore
  ) where

import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import Data.List (sortOn, maximumBy)
import Data.Function (on)
import Data.Maybe (catMaybes, isNothing)
import Gradebook.Config (GradingConfig(..), CategoryConfig(..))

-- | Individual assignment grade information
data AssignmentGrade = AssignmentGrade
  { agSlug      :: T.Text
  , agTitle     :: T.Text
  , agScore     :: Maybe Double
  , agMaxPoints :: Int
  , agExcused   :: Bool
  , agPending   :: Bool
  } deriving (Show, Eq)

-- | Category grade summary
data CategoryGrade = CategoryGrade
  { cgCategory      :: T.Text
  , cgAssignments   :: [AssignmentGrade]
  , cgEarnedPoints  :: Double
  , cgPossiblePoints :: Double
  , cgPercentage    :: Maybe Double
  , cgWeightedScore :: Maybe Double
  } deriving (Show, Eq)

-- | Calculate grades for all categories
-- Takes: grading config, list of (slug, score, excused, maxPoints, category, title, collected)
-- Returns: list of CategoryGrade
calculateGrades :: GradingConfig
                -> [(T.Text, Maybe Double, Bool, Int, T.Text, T.Text, Bool)]
                -> [CategoryGrade]
calculateGrades gradingCfg scores =
  let categoriesMap = categories gradingCfg
      -- Group scores by category
      scoresByCategory = groupByCategory scores
      -- Calculate grade for each category
      categoryGrades = map (calculateCategoryGrade categoriesMap) (HM.toList scoresByCategory)
  in categoryGrades

-- | Group assignment scores by category
groupByCategory :: [(T.Text, Maybe Double, Bool, Int, T.Text, T.Text, Bool)]
                -> HM.HashMap T.Text [(T.Text, Maybe Double, Bool, Int, T.Text, Bool)]
groupByCategory scores =
  foldr insertScore HM.empty scores
  where
    insertScore (slug, score, excused, maxPts, cat, title, collected) acc =
      let entry = (slug, score, excused, maxPts, title, collected)
          existing = HM.lookupDefault [] cat acc
      in HM.insert cat (entry : existing) acc

-- | Calculate grade for a single category
calculateCategoryGrade :: HM.HashMap T.Text CategoryConfig
                       -> (T.Text, [(T.Text, Maybe Double, Bool, Int, T.Text, Bool)])
                       -> CategoryGrade
calculateCategoryGrade categoriesMap (categoryName, assignments) =
  let
    catConfig = HM.lookup categoryName categoriesMap
    weight = maybe 0.0 categoryWeight catConfig
    dropCount = maybe 0 categoryDropLowest catConfig

    -- Convert to AssignmentGrade list, correctly setting pending flag
    assignmentGrades = map toAssignmentGrade assignments

    -- Separate excused and non-excused
    excusedAssignments = filter agExcused assignmentGrades
    nonExcusedAssignments = filter (not . agExcused) assignmentGrades

    -- Only consider assignments with scores for dropping
    scoredAssignments = filter (\ag -> not (isNothing (agScore ag))) nonExcusedAssignments

    -- Calculate earned/possible points based on drop logic
    -- If drops >= number of scores, use the highest score as the average
    (earnedPoints, possiblePoints) =
      if dropCount >= length scoredAssignments && not (null scoredAssignments)
        then
          -- Use the highest scoring assignment (by percentage)
          let highest = maximumBy (compare `on` (\ag -> maybe 0 id (agScore ag) / fromIntegral (agMaxPoints ag))) scoredAssignments
          in (maybe 0 id (agScore highest), fromIntegral (agMaxPoints highest))
        else
          -- Normal dropping logic: sort by percentage, drop N lowest
          let sortedByPercentage = sortOn (\ag -> maybe 0 id (agScore ag) / fromIntegral (agMaxPoints ag)) scoredAssignments
              kept = drop dropCount sortedByPercentage
          in (sum $ map (\ag -> maybe 0 id (agScore ag)) kept, sum $ map (fromIntegral . agMaxPoints) kept)

    -- Calculate percentage and weighted score
    percentage = if possiblePoints > 0
                 then Just (earnedPoints / possiblePoints)
                 else Nothing

    weightedScore = fmap (* weight) percentage

  in CategoryGrade
       { cgCategory = categoryName
       , cgAssignments = assignmentGrades
       , cgEarnedPoints = earnedPoints
       , cgPossiblePoints = possiblePoints
       , cgPercentage = percentage
       , cgWeightedScore = weightedScore
       }

  where
    -- Create AssignmentGrade with correct pending flag:
    -- pending = True when: not collected AND no score AND not excused
    -- pending = False otherwise (either collected, has score, or is excused)
    toAssignmentGrade (slug, score, excused, maxPts, title, collected) =
      let isPending = not collected && isNothing score && not excused
      in AssignmentGrade slug title score maxPts excused isPending

-- | Format a score for display
formatScore :: AssignmentGrade -> T.Text
formatScore ag
  | agPending ag = "(pending)"
  | agExcused ag = "(excused)"
  | Just s <- agScore ag = T.pack $ show s
  | otherwise = "(missing)"
