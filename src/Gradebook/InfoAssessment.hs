{-# LANGUAGE OverloadedStrings #-}

-- | Parser for PrairieLearn @infoAssessment.json@ files.
--
-- The JSON describes an exam's zone and question structure. We use it as the
-- single source of truth for assigning each question a (zone_number, question_number)
-- in the gradebook database, rather than relying on alphabetical sort of the
-- question IDs that show up in PrairieLearn's CSV export.
module Gradebook.InfoAssessment
  ( InfoAssessment(..)
  , Zone(..)
  , QuestionSlot(..)
  , PointsSpec(..)
  , parseInfoAssessment
  , graderZones
  , flattenQuestions
  , buildZonesFromInfo
  ) where

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import Data.Aeson ((.:), (.:?), (.!=))

import Gradebook.Database (ExamZone(..))

data InfoAssessment = InfoAssessment
  { iaTitle :: T.Text
  , iaZones :: [Zone]
  } deriving (Show, Eq)

data Zone = Zone
  { zTitle     :: T.Text
  , zQuestions :: [QuestionSlot]
  } deriving (Show, Eq)

data QuestionSlot = QuestionSlot
  { qsPoints       :: Maybe PointsSpec
  , qsAlternatives :: [T.Text]
  } deriving (Show, Eq)

-- | PrairieLearn writes points as either a scalar (single attempt) or an
-- array (per-attempt scores). We only care about whether it's zero (Workspace).
data PointsSpec
  = PointsScalar Double
  | PointsArray [Double]
  deriving (Show, Eq)

instance A.FromJSON InfoAssessment where
  parseJSON = A.withObject "InfoAssessment" $ \o ->
    InfoAssessment <$> o .: "title" <*> o .: "zones"

instance A.FromJSON Zone where
  parseJSON = A.withObject "Zone" $ \o ->
    Zone <$> o .: "title" <*> o .: "questions"

instance A.FromJSON QuestionSlot where
  parseJSON = A.withObject "QuestionSlot" $ \o -> do
    pts  <- o .:? "points"
    alts <- o .: "alternatives" >>= traverse alternativeId
    return QuestionSlot { qsPoints = pts, qsAlternatives = alts }
    where
      alternativeId = A.withObject "Alternative" $ \alt -> alt .: "id"

instance A.FromJSON PointsSpec where
  parseJSON v = case v of
    A.Number _ -> PointsScalar <$> A.parseJSON v
    A.Array _  -> PointsArray  <$> A.parseJSON v
    _          -> fail "points must be a number or an array of numbers"

-- | Parse an @infoAssessment.json@ file.
parseInfoAssessment :: FilePath -> IO (Either String InfoAssessment)
parseInfoAssessment path = do
  bytes <- BL.readFile path
  return $ A.eitherDecode bytes

-- | Zones the gradebook should track, paired with their PrairieLearn-natural
-- zone_number (1-indexed position in the original JSON, *not* repacked).
-- Workspace-style zones (every question has @"points": 0@) are filtered out.
graderZones :: InfoAssessment -> [(Int, Zone)]
graderZones ia =
  [ (n, z) | (n, z) <- zip [1..] (iaZones ia), isGraded z ]
  where
    isGraded z = not (null (zQuestions z)) && not (all isZeroPoint (zQuestions z))
    isZeroPoint q = case qsPoints q of
      Just (PointsScalar 0) -> True
      Just (PointsArray xs) -> not (null xs) && all (== 0) xs
      _                     -> False

-- | One row per (zone_number, question_number, question_id) — multiple
-- alternatives in the same slot share a question_number.
flattenQuestions :: InfoAssessment -> [(Int, Int, T.Text)]
flattenQuestions ia =
  [ (zoneNum, slotNum, qid)
  | (zoneNum, zone)   <- graderZones ia
  , (slotNum, slot)   <- zip [1..] (zQuestions zone)
  , qid               <- qsAlternatives slot
  ]

-- | ExamZone records for inserting into the @exam_zones@ table.
buildZonesFromInfo :: T.Text -> InfoAssessment -> [ExamZone]
buildZonesFromInfo examSlug ia =
  [ ExamZone
      { ezExamSlug      = examSlug
      , ezZoneNumber    = zoneNum
      , ezZoneTitle     = zTitle zone
      , ezQuestionCount = length (zQuestions zone)
      }
  | (zoneNum, zone) <- graderZones ia
  ]
