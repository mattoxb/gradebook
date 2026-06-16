{-# LANGUAGE OverloadedStrings #-}

module Gradebook.Penalties
  ( parsePenaltiesCSV
  , applyGradeReduction
  ) where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Csv as Csv
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Csv (FromNamedRecord(..), (.:))
import Data.List (sortBy, elemIndex)
import Data.Function (on)
import Data.Ord (Down(..))
import Gradebook.Config (GradeThreshold(..))
import Gradebook.Database (Penalty(..))

-- | Parse a penalties CSV (columns: netid,steps,reason). The 'Penalty' record
-- itself lives in "Gradebook.Database" (alongside 'Score' etc.); penalties are
-- a hand-editable CSV so the database can be regenerated from source in a
-- future semester.
instance FromNamedRecord Penalty where
  parseNamedRecord r = do
    netid  <- r .: "netid"
    steps  <- r .: "steps"
    reason <- r .: "reason"
    return $ Penalty netid steps reason

-- | Parse a penalties CSV file (columns: netid,steps,reason).
parsePenaltiesCSV :: FilePath -> IO (Either String [Penalty])
parsePenaltiesCSV filepath = do
  csvData <- BL.readFile filepath
  case Csv.decodeByName csvData of
    Left err -> return $ Left err
    Right (_, records) -> return $ Right $ V.toList records

-- | Drop a letter grade @steps@ notches down the ordered threshold list.
-- The list is sorted by min-percent descending (highest grade first, matching
-- how @calculateLetterGrade@ orders it), so moving toward higher indices means
-- a lower grade. Clamps at the lowest grade so a large penalty can't run off
-- the end. @steps <= 0@ returns the grade unchanged.
--
-- A grade not found in the threshold list (shouldn't happen, since it was
-- produced from the same list) is returned unchanged.
applyGradeReduction :: [GradeThreshold] -> Int -> T.Text -> T.Text
applyGradeReduction thresholds steps grade
  | steps <= 0 = grade
  | otherwise  =
      let ordered = map gradeLabel (sortBy (compare `on` (Down . gradeMinPercent)) thresholds)
      in case elemIndex grade ordered of
           Nothing  -> grade
           Just idx -> ordered !! min (length ordered - 1) (idx + steps)
