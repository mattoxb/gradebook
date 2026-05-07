{-# LANGUAGE OverloadedStrings #-}

module Gradebook.GradeCalculation
  ( CategoryGrade(..)
  , AssignmentGrade(..)
  , RequirementResult(..)
  , GradeResult(..)
  , ExamGrade(..)
  , ZoneGrade(..)
  , QuestionGrade(..)
  , calculateGrades
  , evaluateRequirements
  , calculateLetterGrade
  , formatScore
  , calculateExamScore
  , combineWithRetake
  , combineWithFinal
  ) where

import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as M
import Data.List (sortOn, sortBy, maximumBy, find)
import Data.Function (on)
import Data.Ord (Down(..))
import Data.Maybe (isNothing)
import Gradebook.Config (GradingConfig(..), CategoryConfig(..), PassRequirement(..), RequirementRule(..), GradeThreshold(..), RetakePolicy(..))

-- | Individual assignment grade information
data AssignmentGrade = AssignmentGrade
  { agSlug      :: T.Text
  , agTitle     :: T.Text
  , agScore     :: Maybe Double  -- ^ Effective score, including any bonus
  , agMaxPoints :: Int
  , agExcused   :: Bool
  , agPending   :: Bool
  , agBonus     :: Maybe Double  -- ^ Bonus multiplier applied (e.g. 0.10 = +10%), if any
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
    bonusMap = maybe HM.empty categoryBonuses catConfig

    -- Convert to AssignmentGrade list, correctly setting pending flag
    -- and applying any per-assignment bonus before drop-lowest
    assignmentGrades = map (toAssignmentGrade bonusMap) assignments

    -- Separate excused and non-excused
    excusedAssignments = filter agExcused assignmentGrades
    nonExcusedAssignments = filter (not . agExcused) assignmentGrades

    -- Eligible for drop/sum: anything that has actually come due. A
    -- (pending) assignment (not yet collected, no score, not excused)
    -- shouldn't count against the student. A (missing) assignment
    -- (collected, no score, not excused) is treated as a zero out of full
    -- max_points — drop-lowest will naturally pick these first since
    -- they are the lowest possible. Excused was already filtered above.
    scoredAssignments = filter (not . agPending) nonExcusedAssignments

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
    -- If a bonus is configured for this slug, multiply the score by (1 + bonus)
    -- so that the bonused score participates in drop-lowest selection.
    toAssignmentGrade bonusMap (slug, score, excused, maxPts, title, collected) =
      let isPending = not collected && isNothing score && not excused
          mBonus = HM.lookup slug bonusMap
          bonusedScore = case (score, mBonus) of
            (Just s, Just b) -> Just (s * (1 + b))
            _                -> score
      in AssignmentGrade slug title bonusedScore maxPts excused isPending mBonus

-- | Format a score for display
formatScore :: AssignmentGrade -> T.Text
formatScore ag
  | agPending ag = "(pending)"
  | agExcused ag = "(excused)"
  | Just s <- agScore ag = T.pack (show s) <> bonusAnnotation
  | otherwise = "(missing)"
  where
    bonusAnnotation = case agBonus ag of
      Just b | b /= 0 -> T.pack $ " (+" ++ show (b * 100) ++ "% bonus)"
      _               -> ""

-- | Result of evaluating a single pass requirement
data RequirementResult = RequirementResult
  { rrName       :: T.Text       -- requirement name
  , rrCategory   :: T.Text       -- category name
  , rrPassed     :: Bool         -- whether requirement is met
  , rrActual     :: Int          -- actual count achieved
  , rrRequired   :: Int          -- required count
  , rrTotal      :: Int          -- total available (after excused)
  , rrExcused    :: Int          -- number excused
  } deriving (Show, Eq)

-- | Overall grade result
data GradeResult
  = WeightedResult
      { wrPercentage :: Double
      }
  | PassFailResult
      { pfrPassed       :: Bool
      , pfrRequirements :: [RequirementResult]
      }
  | LetterGradeResult
      { lgrGrade      :: T.Text
      , lgrPercentage :: Double
      }
  deriving (Show, Eq)

-- | Evaluate all pass requirements against category grades
evaluateRequirements :: [PassRequirement] -> [CategoryGrade] -> [RequirementResult]
evaluateRequirements reqs categoryGrades =
  map (evaluateRequirement categoryGrades) reqs

-- | Evaluate a single pass requirement
evaluateRequirement :: [CategoryGrade] -> PassRequirement -> RequirementResult
evaluateRequirement categoryGrades req =
  let
    categoryName = reqCategory req
    -- Find the matching category grade
    maybeCatGrade = find (\cg -> cgCategory cg == categoryName) categoryGrades

    -- Count scores: we consider an assignment "achieved" if it has a score > 0
    -- For pass-fail courses like cs491cap, any score means they solved it
    (achieved, excused, total) = case maybeCatGrade of
      Nothing -> (0, 0, 0)
      Just cg ->
        let assignments = cgAssignments cg
            achievedCount = length $ filter hasScore assignments
            excusedCount = length $ filter agExcused assignments
            totalCount = length assignments
        in (achievedCount, excusedCount, totalCount)

    -- Calculate effective total (total minus excused)
    effectiveTotal = total - excused

    -- Calculate required count based on rule
    required = case reqRule req of
      PercentageRule pct ->
        floor (pct / 100.0 * fromIntegral effectiveTotal)
      PercentageWithCapRule pct cap ->
        min cap (ceiling (pct / 100.0 * fromIntegral effectiveTotal))
      MinimumCountRule cnt ->
        cnt

    passed = achieved >= required

  in RequirementResult
       { rrName = reqName req
       , rrCategory = categoryName
       , rrPassed = passed
       , rrActual = achieved
       , rrRequired = required
       , rrTotal = effectiveTotal
       , rrExcused = excused
       }
  where
    -- An assignment has a score if it's not pending, not excused, and has Some score
    hasScore ag = not (agExcused ag) && not (agPending ag) && maybe False (> 0) (agScore ag)

-- | Calculate letter grade based on percentage and thresholds
-- Thresholds should be sorted by min-percent descending (highest first)
calculateLetterGrade :: [GradeThreshold] -> Double -> T.Text
calculateLetterGrade thresholds percentage =
  let
    -- Sort thresholds by min-percent descending
    sortedThresholds = sortBy (compare `on` (Down . gradeMinPercent)) thresholds
    -- Find first threshold where percentage >= min-percent
    matchingGrade = find (\gt -> percentage >= gradeMinPercent gt) sortedThresholds
  in case matchingGrade of
       Just gt -> gradeLabel gt
       Nothing -> "F"  -- default if no threshold matches

-- | Individual question grade within an exam zone
data QuestionGrade = QuestionGrade
  { qgQuestionNumber :: Int        -- ^ Question number (1-indexed)
  , qgOriginalScore  :: Maybe Double  -- ^ Score from original attempt (percentage 0-100)
  , qgRetakeScore    :: Maybe Double  -- ^ Score from retake attempt (percentage 0-100)
  , qgFinalScore     :: Maybe Double  -- ^ Score from final exam (percentage 0-100)
  , qgCombinedScore  :: Double     -- ^ Combined score after applying policies (percentage 0-100)
  } deriving (Show, Eq)

-- | Zone grade within an exam
data ZoneGrade = ZoneGrade
  { zgZoneNumber    :: Int         -- ^ Zone number
  , zgZoneTitle     :: T.Text      -- ^ Zone title (e.g., "Direct Recursion")
  , zgQuestions     :: [QuestionGrade]  -- ^ Individual question grades
  , zgPercentage    :: Double      -- ^ Zone percentage (average of question percentages)
  } deriving (Show, Eq)

-- | Overall exam grade
data ExamGrade = ExamGrade
  { egExamSlug      :: T.Text      -- ^ Exam assignment slug
  , egExamTitle     :: T.Text      -- ^ Exam display title
  , egZones         :: [ZoneGrade] -- ^ Grades for each zone
  , egPercentage    :: Double      -- ^ Overall exam percentage (average of zone percentages)
  } deriving (Show, Eq)

-- | Calculate exam score from question scores
-- Takes original scores, optional retake scores, optional final scores
-- Returns the combined score as a percentage (0-1 scale for storage in scores table)
calculateExamScore
  :: RetakePolicy                      -- ^ Policy for combining original and retake
  -> Maybe RetakePolicy                -- ^ Policy for combining with final (if applicable)
  -> [(Int, Int, Double, Double)]      -- ^ Original: (zone, question, score, maxPoints)
  -> [(Int, Int, Double, Double)]      -- ^ Retake: (zone, question, score, maxPoints)
  -> [(Int, Int, Double, Double)]      -- ^ Final: (zone, question, score, maxPoints)
  -> Double                            -- ^ Combined score (0-1 scale)
calculateExamScore retakePolicy finalPolicy originalScores retakeScores finalScores =
  let
    -- Build a map of (zone, question) -> percentage for each attempt
    toPercentMap scores = M.fromList
      [((z, q), if maxPts > 0 then (s / maxPts) * 100 else 0)
      | (z, q, s, maxPts) <- scores]

    origMap = toPercentMap originalScores
    retakeMap = toPercentMap retakeScores
    finalMap = toPercentMap finalScores

    -- Get all unique (zone, question) keys from original
    allKeys = M.keys origMap

    -- Combine original and retake for each question
    afterRetake = M.fromList
      [ (k, combineWithRetake retakePolicy (M.lookup k origMap) (M.lookup k retakeMap))
      | k <- allKeys
      ]

    -- Combine with final if policy exists
    afterFinal = case finalPolicy of
      Nothing -> afterRetake
      Just fPolicy -> M.fromList
        [ (k, combineWithFinal fPolicy (M.lookup k afterRetake) (M.lookup k finalMap))
        | k <- allKeys
        ]

    -- Group by zone and calculate zone averages
    byZone = M.fromListWith (++)
      [((fst k), [v]) | (k, v) <- M.toList afterFinal]

    zoneAverages = [avg vs | (_, vs) <- M.toList byZone]

    -- Overall score is average of zone averages
    overallPercent = if null zoneAverages then 0 else avg zoneAverages

  in overallPercent / 100  -- Convert to 0-1 scale
  where
    avg xs = if null xs then 0 else sum xs / fromIntegral (length xs)

-- | Combine original and retake scores using the specified policy
combineWithRetake :: RetakePolicy -> Maybe Double -> Maybe Double -> Double
combineWithRetake _ Nothing Nothing = 0
combineWithRetake _ (Just orig) Nothing = orig
combineWithRetake _ Nothing (Just retake) = retake
combineWithRetake policy (Just orig) (Just retake) =
  case policy of
    MaxScore -> max orig retake
    MaxIfBetterAvgIfWorse ->
      if retake >= orig
        then retake
        else (orig + retake) / 2
    WeightedRetake w -> (1 - w) * orig + w * retake

-- | Combine score with final exam component using the specified policy
combineWithFinal :: RetakePolicy -> Maybe Double -> Maybe Double -> Double
combineWithFinal _ Nothing Nothing = 0
combineWithFinal _ (Just combined) Nothing = combined
combineWithFinal _ Nothing (Just final') = final'
combineWithFinal policy (Just combined) (Just final') =
  case policy of
    MaxScore -> max combined final'
    MaxIfBetterAvgIfWorse ->
      if final' >= combined
        then final'
        else (combined + final') / 2
    WeightedRetake w -> (1 - w) * combined + w * final'
