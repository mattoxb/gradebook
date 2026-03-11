{-# LANGUAGE OverloadedStrings #-}

module Gradebook.Roster
  ( parseRosterCSV
  ) where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Csv as Csv
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import Data.Csv (FromNamedRecord(..), (.:))
import Gradebook.Database (Student(..))

-- | Make Student an instance of FromNamedRecord for CSV parsing
instance FromNamedRecord Student where
  parseNamedRecord r = Student
    <$> r .: "Net ID"
    <*> r .: "UIN"
    <*> r .: "Admit Term"
    <*> r .: "Gender"
    <*> r .: "Name"
    <*> r .: "Email Address"
    <*> r .: "Credit"
    <*> r .: "Level"
    <*> r .: "Year"
    <*> r .: "Subject"
    <*> r .: "Number"
    <*> r .: "Section"
    <*> r .: "CRN"
    <*> r .: "Degree Name"
    <*> r .: "Major 1 Name"
    <*> r .: "College"
    <*> r .: "Program Code"
    <*> r .: "Program Name"
    <*> r .: "FERPA"
    <*> r .: "Honors Credit"
    <*> r .: "Advisors"
    -- Skip field "On Pending Degree List"

-- | Parse a roster CSV file into a list of Students
parseRosterCSV :: FilePath -> IO (Either String [Student])
parseRosterCSV filepath = do
  csvData <- BL.readFile filepath
  case Csv.decodeByName csvData of
    Left err -> return $ Left err
    Right (_, records) -> return $ Right $ V.toList records
