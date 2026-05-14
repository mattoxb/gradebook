{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

-- | CSV representation of an exam's zone/question structure.
--
-- One row per @(zone_number, question_id)@. Multiple alternatives for the
-- same slot share a @question_number@. The CSV is authoritative for what
-- @gb load-exam-zones@ writes into the database; it's a hand-editable
-- artifact derived initially from @infoAssessment.json@ by
-- @gb gen-exam-zones@ but free to diverge afterwards (e.g., to keep a
-- removed-mid-exam alternative around for historical score data).
module Gradebook.ExamZonesCSV
  ( ExamZoneRow(..)
  , parseExamZonesCSV
  , writeExamZonesCSV
  ) where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Csv as Csv
import qualified Data.Text as T
import qualified Data.Vector as V
import GHC.Generics (Generic)

-- | One row of an exam zones CSV.
data ExamZoneRow = ExamZoneRow
  { ezrExamSlug       :: T.Text
  , ezrZoneNumber     :: Int
  , ezrZoneTitle      :: T.Text
  , ezrQuestionNumber :: Int
  , ezrQuestionId     :: T.Text
  , ezrComment        :: T.Text
  } deriving (Show, Eq, Generic)

instance Csv.FromNamedRecord ExamZoneRow where
  parseNamedRecord r = ExamZoneRow
    <$> r Csv..: "exam_slug"
    <*> r Csv..: "zone_number"
    <*> r Csv..: "zone_title"
    <*> r Csv..: "question_number"
    <*> r Csv..: "question_id"
    <*> r Csv..: "comment"

instance Csv.ToNamedRecord ExamZoneRow where
  toNamedRecord r = Csv.namedRecord
    [ "exam_slug"       Csv..= ezrExamSlug r
    , "zone_number"     Csv..= ezrZoneNumber r
    , "zone_title"      Csv..= ezrZoneTitle r
    , "question_number" Csv..= ezrQuestionNumber r
    , "question_id"     Csv..= ezrQuestionId r
    , "comment"         Csv..= ezrComment r
    ]

instance Csv.DefaultOrdered ExamZoneRow where
  headerOrder _ = V.fromList
    [ "exam_slug", "zone_number", "zone_title"
    , "question_number", "question_id", "comment"
    ]

parseExamZonesCSV :: FilePath -> IO (Either String [ExamZoneRow])
parseExamZonesCSV path = do
  contents <- BL.readFile path
  case Csv.decodeByName contents of
    Left err -> return $ Left err
    Right (_, rows) -> return $ Right (V.toList rows)

writeExamZonesCSV :: FilePath -> [ExamZoneRow] -> IO ()
writeExamZonesCSV path rows =
  BL.writeFile path (Csv.encodeDefaultOrderedByName rows)
