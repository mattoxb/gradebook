{-# LANGUAGE OverloadedStrings #-}

module Gradebook.Assignments
  ( parseAssignmentsCSV
  ) where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Csv as Csv
import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Csv (ToRecord(..), ToField(..), FromField(..), ToNamedRecord(..), FromNamedRecord(..), namedRecord, (.:), (.=))
import Gradebook.Database (Assignment(..))

-- | Make Assignment an instance of ToRecord for CSV encoding
instance ToRecord Assignment where
  toRecord (Assignment order' startDate' category' slug' maxPoints' title' collected') =
    Csv.record
      [ toField order'
      , toField startDate'
      , toField category'
      , toField slug'
      , toField maxPoints'
      , toField title'
      , toField (if collected' then (1 :: Int) else 0)
      ]

-- | Make Assignment an instance of ToNamedRecord for CSV encoding with headers
instance ToNamedRecord Assignment where
  toNamedRecord (Assignment order' startDate' category' slug' maxPoints' title' collected') =
    namedRecord
      [ "order" .= order'
      , "start_date" .= startDate'
      , "category" .= category'
      , "slug" .= slug'
      , "max_points" .= maxPoints'
      , "title" .= title'
      , "collected" .= (if collected' then (1 :: Int) else 0)
      ]

-- | Make Assignment an instance of FromNamedRecord for CSV decoding with headers
instance FromNamedRecord Assignment where
  parseNamedRecord r = do
    order' <- r .: "order"
    startDate' <- r .: "start_date"
    category' <- r .: "category"
    slug' <- r .: "slug"
    maxPoints' <- r .: "max_points"
    title' <- r .: "title"
    collectedInt <- r .: "collected" :: Csv.Parser Int
    let collected' = collectedInt /= 0
    return $ Assignment order' startDate' category' slug' maxPoints' title' collected'

-- | Parse an assignments CSV file into a list of Assignments
parseAssignmentsCSV :: FilePath -> IO (Either String [Assignment])
parseAssignmentsCSV filepath = do
  csvData <- BL.readFile filepath
  case Csv.decodeByName csvData of
    Left err -> return $ Left err
    Right (_, records) -> return $ Right $ V.toList records
