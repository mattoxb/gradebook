{-# LANGUAGE OverloadedStrings #-}

module Gradebook.Scores
  ( parseScoresCSV
  ) where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Csv as Csv
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import Data.Csv (FromRecord(..), (.!))
import Text.Read (readMaybe)
import Gradebook.Database (Score(..))

-- | Make Score an instance of FromRecord for CSV parsing
instance FromRecord Score where
  parseRecord v
    | V.length v == 3 = do
        netid <- v .! 0
        assignment <- v .! 1
        scoreText <- v .! 2 :: Csv.Parser T.Text

        -- Parse score field: either a number or 'x' for excused
        let (scoreVal, excused) = parseScoreField scoreText

        return $ Score netid assignment scoreVal excused
    | otherwise = fail $ "Expected 3 fields, got " ++ show (V.length v)

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
  case Csv.decode Csv.HasHeader csvData of
    Left err -> return $ Left err
    Right records -> return $ Right $ V.toList records
