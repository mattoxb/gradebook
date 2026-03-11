{-# LANGUAGE OverloadedStrings #-}

module Gradebook.Categories
  ( parseCategoriesCSV
  ) where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Csv as Csv
import qualified Data.Vector as V
import Data.Csv (FromNamedRecord(..), (.:))
import Gradebook.Database (Category(..))

-- | Make Category an instance of FromNamedRecord for CSV parsing
instance FromNamedRecord Category where
  parseNamedRecord r = Category
    <$> r .: "slug"
    <*> r .: "title"

-- | Parse a categories CSV file into a list of Categories
parseCategoriesCSV :: FilePath -> IO (Either String [Category])
parseCategoriesCSV filepath = do
  csvData <- BL.readFile filepath
  case Csv.decodeByName csvData of
    Left err -> return $ Left err
    Right (_, records) -> return $ Right $ V.toList records
