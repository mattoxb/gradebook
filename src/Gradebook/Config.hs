{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Gradebook.Config
  ( Config(..)
  , DbType(..)
  , GradingConfig(..)
  , GradingMode(..)
  , CategoryConfig(..)
  , Policy(..)
  , PassRequirement(..)
  , RequirementRule(..)
  , GradeThreshold(..)
  , ExamConfig(..)
  , RetakePolicy(..)
  , loadConfig
  ) where

import Data.Yaml (FromJSON(..), (.:), (.:?), (.!=), decodeFileThrow)
import Data.Aeson (withObject, withText)
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import GHC.Generics (Generic)

data DbType = SQLite | PostgreSQL
  deriving (Show, Eq, Generic)

instance FromJSON DbType where
  parseJSON = withText "DbType" $ \dbTypeStr ->
    case dbTypeStr of
      "sqlite3"    -> return SQLite
      "postgresql" -> return PostgreSQL
      _            -> fail "db-type must be 'sqlite3' or 'postgresql'"

-- | Grading mode
data GradingMode = Weighted | PassFail | LetterGrade
  deriving (Show, Eq, Generic)

instance FromJSON GradingMode where
  parseJSON = withText "GradingMode" $ \modeStr ->
    case modeStr of
      "weighted"     -> return Weighted
      "pass-fail"    -> return PassFail
      "letter-grade" -> return LetterGrade
      _              -> fail "mode must be 'weighted', 'pass-fail', or 'letter-grade'"

-- | Category grading configuration (for weighted/letter-grade modes)
data CategoryConfig = CategoryConfig
  { categoryWeight       :: Double
  , categoryDropLowest   :: Int
  , categoryCreditHours  :: Maybe [Int]  -- ^ If set, category only applies to students with these credit hours
  , categoryBonuses      :: HM.HashMap T.Text Double  -- ^ Per-assignment bonus multipliers (e.g. 0.10 = +10%) applied before drop-lowest
  } deriving (Show, Eq, Generic)

instance FromJSON CategoryConfig where
  parseJSON = withObject "CategoryConfig" $ \v -> do
    weight <- v .: "weight"
    dropLowest <- v .:? "drop-lowest" .!= 0
    creditHours <- v .:? "credit-hours"
    bonuses <- v .:? "bonuses" .!= HM.empty
    return $ CategoryConfig weight dropLowest creditHours bonuses

-- | Grading policy (e.g., minimum attendance) - legacy, kept for compatibility
data Policy = Policy
  { policyType     :: T.Text
  , policyCategory :: Maybe T.Text
  , policyThreshold :: Maybe Double
  , policyMessage  :: T.Text
  } deriving (Show, Eq, Generic)

instance FromJSON Policy where
  parseJSON = withObject "Policy" $ \v -> do
    pType <- v .: "type"
    pCategory <- v .:? "category"
    pThreshold <- v .:? "threshold"
    pMessage <- v .: "message"
    return $ Policy pType pCategory pThreshold pMessage

-- | Requirement rule types for pass-fail grading
data RequirementRule
  = PercentageRule
      { rulePercentage :: Double  -- required percentage (0-100)
      }
  | PercentageWithCapRule
      { rulePercentage :: Double  -- required percentage (0-100)
      , ruleCap        :: Int     -- max count required regardless of total
      }
  | MinimumCountRule
      { ruleCount :: Int          -- minimum count required
      }
  deriving (Show, Eq, Generic)

instance FromJSON RequirementRule where
  parseJSON = withObject "RequirementRule" $ \v -> do
    ruleType <- v .: "rule"
    case (ruleType :: T.Text) of
      "percentage" -> do
        pct <- v .: "percentage"
        return $ PercentageRule pct
      "percentage-with-cap" -> do
        pct <- v .: "percentage"
        cap <- v .: "cap"
        return $ PercentageWithCapRule pct cap
      "minimum-count" -> do
        cnt <- v .: "count"
        return $ MinimumCountRule cnt
      _ -> fail "rule must be 'percentage', 'percentage-with-cap', or 'minimum-count'"

-- | A single pass requirement
data PassRequirement = PassRequirement
  { reqName     :: T.Text           -- display name for the requirement
  , reqCategory :: T.Text           -- which category this applies to
  , reqRule     :: RequirementRule  -- the rule to evaluate
  } deriving (Show, Eq, Generic)

instance FromJSON PassRequirement where
  parseJSON = withObject "PassRequirement" $ \v -> do
    name <- v .: "name"
    cat <- v .: "category"
    -- Parse the rule fields from the same object
    ruleType <- v .: "rule"
    rule <- case (ruleType :: T.Text) of
      "percentage" -> do
        pct <- v .: "percentage"
        return $ PercentageRule pct
      "percentage-with-cap" -> do
        pct <- v .: "percentage"
        cap <- v .: "cap"
        return $ PercentageWithCapRule pct cap
      "minimum-count" -> do
        cnt <- v .: "count"
        return $ MinimumCountRule cnt
      _ -> fail "rule must be 'percentage', 'percentage-with-cap', or 'minimum-count'"
    return $ PassRequirement name cat rule

-- | Grade threshold for letter grades
data GradeThreshold = GradeThreshold
  { gradeLabel      :: T.Text   -- e.g., "A+", "A", "A-", "B+", etc.
  , gradeMinPercent :: Double   -- minimum percentage for this grade (0-100)
  } deriving (Show, Eq, Generic)

instance FromJSON GradeThreshold where
  parseJSON = withObject "GradeThreshold" $ \v -> do
    label <- v .: "grade"
    minPct <- v .: "min-percent"
    return $ GradeThreshold label minPct

-- | Retake policy for combining original and retake scores
data RetakePolicy
  = MaxScore                    -- ^ Take the maximum of original and retake for each question
  | MaxIfBetterAvgIfWorse       -- ^ Max if retake improves, average if retake is worse
  | WeightedRetake Double       -- ^ Weighted: (1-w)*original + w*retake
  deriving (Show, Eq, Generic)

instance FromJSON RetakePolicy where
  parseJSON = withText "RetakePolicy" $ \policyStr ->
    case policyStr of
      "max"                      -> return MaxScore
      "max-if-better-avg-if-worse" -> return MaxIfBetterAvgIfWorse
      _                          -> fail "retake-policy must be 'max' or 'max-if-better-avg-if-worse'"

-- | Configuration for an exam
data ExamConfig = ExamConfig
  { examSlug         :: T.Text           -- ^ Exam assignment slug (e.g., "exam-1")
  , examTitle        :: T.Text           -- ^ Display title (e.g., "Midterm 1")
  , examRetakeSlug   :: Maybe T.Text     -- ^ Optional retake exam slug (e.g., "exam-1r")
  , examRetakePolicy :: RetakePolicy     -- ^ How to combine original and retake scores
  , examFinalSlug    :: Maybe T.Text     -- ^ Optional final exam component for this material
  , examFinalPolicy  :: Maybe RetakePolicy -- ^ Policy for combining with final
  } deriving (Show, Eq, Generic)

instance FromJSON ExamConfig where
  parseJSON = withObject "ExamConfig" $ \v -> do
    slug <- v .: "slug"
    title <- v .: "title"
    retakeSlug <- v .:? "retake-slug"
    retakePolicy <- v .:? "retake-policy" .!= MaxScore
    finalSlug <- v .:? "final-slug"
    finalPolicy <- v .:? "final-policy"
    return $ ExamConfig slug title retakeSlug retakePolicy finalSlug finalPolicy

-- | Overall grading configuration
data GradingConfig = GradingConfig
  { gradingMode       :: GradingMode
  , categories        :: HM.HashMap T.Text CategoryConfig
  , policies          :: [Policy]                        -- legacy policies
  , passRequirements  :: [PassRequirement]               -- for pass-fail mode
  , gradeThresholds   :: [GradeThreshold]                -- for letter-grade mode
  , exams             :: [ExamConfig]                    -- exam configurations
  , reportFormat      :: T.Text                          -- report format identifier (e.g., "default", "cs421-v1", "cs491-v1")
  } deriving (Show, Eq, Generic)

instance FromJSON GradingConfig where
  parseJSON = withObject "GradingConfig" $ \v -> do
    mode <- v .:? "mode" .!= Weighted  -- default to weighted for backward compat
    cats <- v .:? "categories" .!= HM.empty
    pols <- v .:? "policies" .!= []
    reqs <- v .:? "pass-requirements" .!= []
    thresholds <- v .:? "grade-thresholds" .!= []
    examConfigs <- v .:? "exams" .!= []
    fmt <- v .:? "report-format" .!= "default"
    return $ GradingConfig mode cats pols reqs thresholds examConfigs fmt

data Config = Config
  { database    :: T.Text
  , dbType      :: DbType
  , repoPrefix  :: Maybe T.Text
  , grading     :: Maybe GradingConfig
  } deriving (Show, Eq, Generic)

instance FromJSON Config where
  parseJSON = withObject "Config" $ \v -> do
    db <- v .: "database"
    dt <- v .: "db-type"
    rp <- v .:? "repo-prefix"
    gr <- v .:? "grading"
    return $ Config db dt rp gr

-- | Load configuration from config.yaml
loadConfig :: FilePath -> IO Config
loadConfig = decodeFileThrow
