{-# LANGUAGE OverloadedStrings #-}

module Gradebook.FinalGrades
  ( FinalGradeRow(..)
  , buildFinalGradeRow
  , writeFinalGradesXlsx
  , lastAttendedDate
  ) where

import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import Database.HDBC (IConnection, quickQuery', toSql, fromSql, SqlValue(SqlNull))
import Codec.Xlsx
  ( Xlsx, Worksheet, CellValue(..), Cell
  , def, fromXlsx
  , xlSheets, wsCells, cellValue
  )
import Control.Lens ((&), (.~), (?~))
import Data.Time.Clock.POSIX (getPOSIXTime)

import Gradebook.GradeCalculation
  ( CategoryGrade(..), calculateLetterGrade )
import Gradebook.Config (GradeThreshold)
import Gradebook.Reports (ReportData(..))

-- | One row of the final-grade spreadsheet.
data FinalGradeRow = FinalGradeRow
  { fgrUin           :: T.Text
  , fgrCrn           :: T.Text
  , fgrLetterGrade   :: T.Text
  , fgrLastAttended  :: Maybe T.Text   -- ISO date string; only relevant for F
  } deriving (Show, Eq)

-- | Compute the row data for one student. Takes the student's uin, crn,
-- and the already-built report data and threshold list. Returns Nothing
-- when thresholds are empty (can't assign a letter without them).
buildFinalGradeRow :: T.Text -> T.Text -> ReportData -> [GradeThreshold] -> Maybe T.Text -> Maybe FinalGradeRow
buildFinalGradeRow studentUin studentCrn reportData thresholds lastDate =
  if null thresholds
    then Nothing
    else
      let weightedScores = map cgWeightedScore (rdCategoryGrades reportData)
          totalPct = sum (map (maybe 0 id) weightedScores) * 100
          letter = calculateLetterGrade thresholds totalPct
          dateField = if letter == "F" then lastDate else Nothing
      in Just FinalGradeRow
           { fgrUin          = studentUin
           , fgrCrn          = studentCrn
           , fgrLetterGrade  = letter
           , fgrLastAttended = dateField
           }

-- | The most recent assignment start_date for which the student has a
-- non-null, non-excused score. Returns the date as Text (cs421 stores
-- ISO yyyy-mm-dd in the start_date text column).
lastAttendedDate :: IConnection conn => conn -> T.Text -> IO (Maybe T.Text)
lastAttendedDate conn netid = do
  rows <- quickQuery' conn
    (unlines
      [ "SELECT a.start_date FROM scores s"
      , "JOIN assignments a ON s.assignment = a.slug"
      , "WHERE s.netid = ?"
      , "  AND s.score IS NOT NULL"
      , "  AND NOT s.excused"
      , "ORDER BY a.start_date DESC NULLS LAST"
      , "LIMIT 1"
      ])
    [toSql netid]
  case rows of
    [[SqlNull]] -> return Nothing
    [[v]]       -> return (Just (T.pack (fromSql v)))
    _           -> return Nothing

-- | Write the spreadsheet to the given path. One worksheet, "Grades".
writeFinalGradesXlsx :: FilePath -> T.Text -> [FinalGradeRow] -> IO ()
writeFinalGradesXlsx path termCodeText rows = do
  ct <- getPOSIXTime
  let cells = Map.fromList (headerCells ++ concat (zipWith bodyCells [2..] rows))
      ws   = (def :: Worksheet) & wsCells .~ cells
      xlsx = (def :: Xlsx) & xlSheets .~ [("Grades", ws)]
  BL.writeFile path (fromXlsx ct xlsx)
  where
    textCell :: T.Text -> Cell
    textCell t = def & cellValue ?~ CellText t

    headerLabels = ["Term Code", "CRN", "Student ID", "Final Grade", "Last Date of Attendance"]
    headerCells  = [((1, c), textCell label) | (c, label) <- zip [1..] headerLabels]

    bodyCells rowNum r =
      let base =
            [ ((rowNum, 1), textCell termCodeText)
            , ((rowNum, 2), textCell (fgrCrn r))
            , ((rowNum, 3), textCell (fgrUin r))
            , ((rowNum, 4), textCell (fgrLetterGrade r))
            ]
          dateCell = case fgrLastAttended r of
            Just d  -> [((rowNum, 5), textCell d)]
            Nothing -> []
      in base ++ dateCell
