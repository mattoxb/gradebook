{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Gradebook.ExamOverrides
  ( ExamOverride(..)
  , parseExamOverridesCSV
  ) where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Csv as Csv
import qualified Data.Text as T
import qualified Data.Vector as V
import GHC.Generics (Generic)

-- | An override record for an exam question score
data ExamOverride = ExamOverride
  { eoNetId          :: T.Text   -- ^ Student netid
  , eoZoneNumber     :: Int      -- ^ Zone number (1-indexed)
  , eoQuestionNumber :: Int      -- ^ Question number within zone (1-indexed)
  , eoScore          :: Double   -- ^ Override score (will take max with existing)
  , eoMaxPoints      :: Double   -- ^ Max points for this question
  , eoReason         :: T.Text   -- ^ Reason for the override
  } deriving (Show, Eq, Generic)

instance Csv.FromNamedRecord ExamOverride where
  parseNamedRecord r = ExamOverride
    <$> r Csv..: "netid"
    <*> r Csv..: "zone_number"
    <*> r Csv..: "question_number"
    <*> r Csv..: "score"
    <*> r Csv..: "max_points"
    <*> r Csv..: "reason"

-- | Parse an exam overrides CSV file
parseExamOverridesCSV :: FilePath -> IO (Either String [ExamOverride])
parseExamOverridesCSV path = do
  contents <- BL.readFile path
  case Csv.decodeByName contents of
    Left err -> return $ Left err
    Right (_, rows) -> return $ Right (V.toList rows)
