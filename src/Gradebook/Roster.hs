{-# LANGUAGE OverloadedStrings #-}

module Gradebook.Roster
  ( parseRosterCSV
  ) where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Csv as Csv
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import Data.Csv (FromRecord(..), (.!))
import Gradebook.Database (Student(..))

-- | Make Student an instance of FromRecord for CSV parsing
instance FromRecord Student where
  parseRecord v
    | V.length v == 22 = Student
        <$> v .! 0  -- netId
        <*> v .! 1  -- uin
        <*> v .! 2  -- admitTerm
        <*> v .! 3  -- gender
        <*> v .! 4  -- name
        <*> v .! 5  -- email
        <*> v .! 6  -- credit
        <*> v .! 7  -- level
        <*> v .! 8  -- year
        <*> v .! 9  -- subject
        <*> v .! 10 -- number
        <*> v .! 11 -- section
        <*> v .! 12 -- crn
        <*> v .! 13 -- degreeName
        <*> v .! 14 -- major1Name
        <*> v .! 15 -- college
        <*> v .! 16 -- programCode
        <*> v .! 17 -- programName
        <*> v .! 18 -- ferpa
        <*> v .! 19 -- honorsCredit
        <*> v .! 20 -- advisors
        -- Skip field 21 (On Pending Degree List)
    | otherwise = fail $ "Expected 22 fields, got " ++ show (V.length v)

-- | Parse a roster CSV file into a list of Students
parseRosterCSV :: FilePath -> IO (Either String [Student])
parseRosterCSV filepath = do
  csvData <- BL.readFile filepath
  case Csv.decode Csv.HasHeader csvData of
    Left err -> return $ Left err
    Right records -> return $ Right $ V.toList records
