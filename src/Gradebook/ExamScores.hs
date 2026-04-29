{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Gradebook.ExamScores
  ( PrairieLearnRow(..)
  , parsePrairieLearnCSV
  , extractNetId
  , groupByStudent
  , buildExamZones
  , buildQuestionScores
  ) where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Csv as Csv
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Map.Strict as M
import Data.List (sortOn, nub)
import GHC.Generics (Generic)

import Gradebook.Database (ExamZone(..), ExamQuestionScore(..))

-- | A row from PrairieLearn's instance_questions CSV export
data PrairieLearnRow = PrairieLearnRow
  { plrUID            :: T.Text   -- ^ Email (e.g., "netid@illinois.edu")
  , plrUIN            :: T.Text   -- ^ University ID number
  , plrName           :: T.Text   -- ^ Student name
  , plrRole           :: T.Text   -- ^ "Student" or other role
  , plrAssessment     :: T.Text   -- ^ Assessment name (e.g., "Exam 1")
  , plrZoneNumber     :: Int      -- ^ Zone number (1-indexed)
  , plrZoneTitle      :: T.Text   -- ^ Zone title (e.g., "Direct Recursion")
  , plrQuestion       :: T.Text   -- ^ Question ID (e.g., "code-haskell/list-recursion/mulList")
  , plrQuestionPoints :: Double   -- ^ Points earned
  , plrMaxPoints      :: Double   -- ^ Maximum points possible
  } deriving (Show, Eq, Generic)

instance Csv.FromNamedRecord PrairieLearnRow where
  parseNamedRecord r = PrairieLearnRow
    <$> r Csv..: "UID"
    <*> r Csv..: "UIN"
    <*> r Csv..: "Name"
    <*> r Csv..: "Role"
    <*> r Csv..: "Assessment"
    <*> r Csv..: "Zone number"
    <*> r Csv..: "Zone title"
    <*> r Csv..: "Question"
    <*> r Csv..: "Question points"
    <*> r Csv..: "Max points"

-- | Parse a PrairieLearn instance_questions CSV file
parsePrairieLearnCSV :: FilePath -> IO (Either String [PrairieLearnRow])
parsePrairieLearnCSV path = do
  contents <- BL.readFile path
  case Csv.decodeByName contents of
    Left err -> return $ Left err
    Right (_, rows) ->
      -- Filter to only include students (not staff/instructors)
      -- and exclude zero-point questions (e.g., PrairieLearn workspace questions)
      let studentRows = V.toList $ V.filter (\r -> plrRole r == "Student" && plrMaxPoints r > 0) rows
      in return $ Right studentRows

-- | Extract netid from email (e.g., "netid@illinois.edu" -> "netid")
extractNetId :: T.Text -> T.Text
extractNetId email = T.takeWhile (/= '@') email

-- | Group PrairieLearn rows by student netid
groupByStudent :: [PrairieLearnRow] -> M.Map T.Text [PrairieLearnRow]
groupByStudent rows = M.fromListWith (++)
  [(extractNetId (plrUID r), [r]) | r <- rows]

-- | Build ExamZone records from PrairieLearn data
-- This extracts the unique zones and counts questions per zone
buildExamZones :: T.Text -> [PrairieLearnRow] -> [ExamZone]
buildExamZones examSlug rows =
  let -- Get unique (zone_number, zone_title) pairs
      zonePairs = nub [(plrZoneNumber r, plrZoneTitle r) | r <- rows]
      -- Sort by zone number
      sortedZones = sortOn fst zonePairs
      -- Count questions per zone (assuming each zone has same question count across students)
      -- We'll count from the first student's data
      questionCounts = M.fromListWith max
        [(plrZoneNumber r, countQuestionsInZone (plrZoneNumber r) rows) | r <- rows]
  in [ ExamZone
         { ezExamSlug = examSlug
         , ezZoneNumber = zoneNum
         , ezZoneTitle = zoneTitle
         , ezQuestionCount = M.findWithDefault 1 zoneNum questionCounts
         }
     | (zoneNum, zoneTitle) <- sortedZones
     ]
  where
    -- Count questions in a zone for any single student
    countQuestionsInZone :: Int -> [PrairieLearnRow] -> Int
    countQuestionsInZone zoneNum allRows =
      let -- Group by student
          byStudent = groupByStudent allRows
          -- Get counts for each student
          counts = [length $ filter (\r -> plrZoneNumber r == zoneNum) studentRows
                   | studentRows <- M.elems byStudent]
      in if null counts then 1 else maximum counts

-- | Build ExamQuestionScore records for a single student
buildQuestionScores :: T.Text -> T.Text -> [PrairieLearnRow] -> [ExamQuestionScore]
buildQuestionScores examSlug netid rows =
  let -- Sort by zone number, then by question within zone
      sortedRows = sortOn (\r -> (plrZoneNumber r, plrQuestion r)) rows
      -- Group by zone and number questions within each zone
      byZone = M.fromListWith (++) [(plrZoneNumber r, [r]) | r <- sortedRows]
      -- Build question scores with proper numbering
      questionScores = concatMap (buildZoneQuestions examSlug netid) (M.toList byZone)
  in questionScores

-- | Build question scores for a single zone
buildZoneQuestions :: T.Text -> T.Text -> (Int, [PrairieLearnRow]) -> [ExamQuestionScore]
buildZoneQuestions examSlug netid (zoneNum, zoneRows) =
  let -- Sort rows within zone by question ID for consistent ordering
      sortedRows = sortOn plrQuestion zoneRows
  in [ ExamQuestionScore
         { eqsNetId = netid
         , eqsExamSlug = examSlug
         , eqsZoneNumber = zoneNum
         , eqsQuestionNumber = qNum
         , eqsQuestionId = plrQuestion row
         , eqsScore = plrQuestionPoints row
         , eqsMaxPoints = plrMaxPoints row
         , eqsOverrideReason = Nothing  -- No override when loading from PrairieLearn
         }
     | (qNum, row) <- zip [1..] sortedRows
     ]
