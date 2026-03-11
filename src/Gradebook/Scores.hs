{-# LANGUAGE OverloadedStrings #-}

module Gradebook.Scores
  ( parseScoresCSV
  ) where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Csv as Csv
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import Data.Csv (FromNamedRecord(..), (.:))
import Text.Read (readMaybe)
import Gradebook.Database (Score(..))

-- | Make Score an instance of FromNamedRecord for CSV parsing
instance FromNamedRecord Score where
  parseNamedRecord r = do
    netid <- r .: "netid"
    assignment <- r .: "assignment"
    scoreText <- r .: "score" :: Csv.Parser T.Text

    -- Parse score field: either a number or 'x' for excused
    let (scoreVal, excused) = parseScoreField scoreText

    return $ Score netid assignment scoreVal excused

-- | Parse score field: returns (Maybe Double, Bool)
-- 'x' or 'X' → (Nothing, True)
-- number → (Just n, False)
-- empty or invalid → (Nothing, False)
parseScoreField :: T.Text -> (Maybe Double, Bool)
parseScoreField txt
  | T.toLower (T.strip txt) == "x" = (Nothing, True)
  | T.null (T.strip txt) = (Nothing, False)
  | otherwise = case readMaybe (T.unpack $ T.strip txt) of
      Just n  -> (Just n, False)
      Nothing -> (Nothing, False)

-- | Parse a scores CSV file into a list of Scores
parseScoresCSV :: FilePath -> IO (Either String [Score])
parseScoresCSV filepath = do
  csvData <- BL.readFile filepath
  case Csv.decodeByName csvData of
    Left err -> return $ Left err
    Right (_, records) -> return $ Right $ V.toList records
