{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Gradebook.ExamScores
  ( PrairieLearnRow(..)
  , parsePrairieLearnCSV
  , extractNetId
  , groupByStudent
  , buildQuestionScores
  , missingQuestionMsg
  ) where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Csv as Csv
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Map.Strict as M
import GHC.Generics (Generic)

import Gradebook.Database (ExamQuestionScore(..))

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

-- | Build ExamQuestionScore records for a single student. The
-- @(zone_number, question_id) -> question_number@ map comes from
-- @exam_questions@ and is the single source of truth for numbering.
-- If a row's (zone, question_id) is not in the map we hard-fail the
-- whole load (the JSON on disk has drifted from the CSV).
buildQuestionScores :: M.Map (Int, T.Text) Int
                    -> T.Text
                    -> T.Text
                    -> [PrairieLearnRow]
                    -> Either T.Text [ExamQuestionScore]
buildQuestionScores qmap examSlug netid =
  traverse one
  where
    one r = case M.lookup (plrZoneNumber r, plrQuestion r) qmap of
      Nothing -> Left (missingQuestionMsg examSlug netid r)
      Just qn -> Right ExamQuestionScore
        { eqsNetId          = netid
        , eqsExamSlug       = examSlug
        , eqsZoneNumber     = plrZoneNumber r
        , eqsQuestionNumber = qn
        , eqsQuestionId     = plrQuestion r
        , eqsScore          = plrQuestionPoints r
        , eqsMaxPoints      = plrMaxPoints r
        , eqsOverrideReason = Nothing
        }

-- | Human-readable error explaining that a CSV question is unknown
-- to exam_questions.
missingQuestionMsg :: T.Text -> T.Text -> PrairieLearnRow -> T.Text
missingQuestionMsg examSlug netid r = T.unlines
  [ "Error: PrairieLearn CSV contains a question that is not in exam_questions."
  , "  exam_slug   = " <> examSlug
  , "  netid       = " <> netid
  , "  zone_number = " <> T.pack (show (plrZoneNumber r))
  , "  zone_title  = " <> plrZoneTitle r
  , "  question_id = " <> plrQuestion r
  , ""
  , "The infoAssessment.json on disk has changed since the last load-exam-zones."
  , "Re-run:  gb load-exam-zones -e " <> examSlug <> " <path-to-infoAssessment.json>"
  , "then re-run this load-exam command."
  ]
