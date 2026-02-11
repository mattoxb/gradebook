{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Gradebook.Config
  ( Config(..)
  , DbType(..)
  , loadConfig
  ) where

import Data.Yaml (FromJSON(..), (.:), decodeFileThrow)
import Data.Aeson (withObject, withText)
import qualified Data.Text as T
import GHC.Generics (Generic)

data DbType = SQLite | PostgreSQL
  deriving (Show, Eq, Generic)

instance FromJSON DbType where
  parseJSON = withText "DbType" $ \dbTypeStr ->
    case dbTypeStr of
      "sqlite3"    -> return SQLite
      "postgresql" -> return PostgreSQL
      _            -> fail "db-type must be 'sqlite3' or 'postgresql'"

data Config = Config
  { database :: T.Text
  , dbType   :: DbType
  } deriving (Show, Eq, Generic)

instance FromJSON Config where
  parseJSON = withObject "Config" $ \v -> do
    db <- v .: "database"
    dt <- v .: "db-type"
    return $ Config db dt

-- | Load configuration from config.yaml
loadConfig :: FilePath -> IO Config
loadConfig = decodeFileThrow
