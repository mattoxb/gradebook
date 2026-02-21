{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Gradebook.Config
  ( Config(..)
  , DbType(..)
  , GradingConfig(..)
  , CategoryConfig(..)
  , Policy(..)
  , loadConfig
  ) where

import Data.Yaml (FromJSON(..), (.:), (.:?), (.!=), decodeFileThrow)
import Data.Aeson (withObject, withText)
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import GHC.Generics (Generic)

data DbType = SQLite | PostgreSQL
  deriving (Show, Eq, Generic)

instance FromJSON DbType where
  parseJSON = withText "DbType" $ \dbTypeStr ->
    case dbTypeStr of
      "sqlite3"    -> return SQLite
      "postgresql" -> return PostgreSQL
      _            -> fail "db-type must be 'sqlite3' or 'postgresql'"

-- | Category grading configuration
data CategoryConfig = CategoryConfig
  { categoryWeight    :: Double
  , categoryDropLowest :: Int
  } deriving (Show, Eq, Generic)

instance FromJSON CategoryConfig where
  parseJSON = withObject "CategoryConfig" $ \v -> do
    weight <- v .: "weight"
    dropLowest <- v .:? "drop-lowest" .!= 0
    return $ CategoryConfig weight dropLowest

-- | Grading policy (e.g., minimum attendance)
data Policy = Policy
  { policyType     :: T.Text
  , policyCategory :: Maybe T.Text
  , policyThreshold :: Maybe Double
  , policyMessage  :: T.Text
  } deriving (Show, Eq, Generic)

instance FromJSON Policy where
  parseJSON = withObject "Policy" $ \v -> do
    pType <- v .: "type"
    pCategory <- v .:? "category"
    pThreshold <- v .:? "threshold"
    pMessage <- v .: "message"
    return $ Policy pType pCategory pThreshold pMessage

-- | Overall grading configuration
data GradingConfig = GradingConfig
  { categories :: HM.HashMap T.Text CategoryConfig
  , policies   :: [Policy]
  } deriving (Show, Eq, Generic)

instance FromJSON GradingConfig where
  parseJSON = withObject "GradingConfig" $ \v -> do
    cats <- v .: "categories"
    pols <- v .:? "policies" .!= []
    return $ GradingConfig cats pols

data Config = Config
  { database    :: T.Text
  , dbType      :: DbType
  , repoPrefix  :: Maybe T.Text
  , grading     :: Maybe GradingConfig
  } deriving (Show, Eq, Generic)

instance FromJSON Config where
  parseJSON = withObject "Config" $ \v -> do
    db <- v .: "database"
    dt <- v .: "db-type"
    rp <- v .:? "repo-prefix"
    gr <- v .:? "grading"
    return $ Config db dt rp gr

-- | Load configuration from config.yaml
loadConfig :: FilePath -> IO Config
loadConfig = decodeFileThrow
