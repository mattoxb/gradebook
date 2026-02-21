{-# LANGUAGE OverloadedStrings #-}

module Gradebook.Categories
  ( parseCategoriesCSV
  ) where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Csv as Csv
import qualified Data.Vector as V
import Data.Csv (FromRecord(..), (.!))
import Gradebook.Database (Category(..))

-- | Make Category an instance of FromRecord for CSV parsing
instance FromRecord Category where
  parseRecord v
    | V.length v == 2 = Category
        <$> v .! 0  -- categorySlug
        <*> v .! 1  -- categoryTitle
    | otherwise = fail $ "Expected 2 fields, got " ++ show (V.length v)

-- | Parse a categories CSV file into a list of Categories
parseCategoriesCSV :: FilePath -> IO (Either String [Category])
parseCategoriesCSV filepath = do
  csvData <- BL.readFile filepath
  case Csv.decode Csv.HasHeader csvData of
    Left err -> return $ Left err
    Right records -> return $ Right $ V.toList records
