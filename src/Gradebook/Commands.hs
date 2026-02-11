{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Gradebook.Commands
  ( runLoadRoster
  , runSearchNetId
  , openConnection
  ) where

import Database.HDBC
import Database.HDBC.Sqlite3 (connectSqlite3, Connection)
-- import Database.HDBC.PostgreSQL (connectPostgreSQL)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Process (readProcess)
import System.Exit (exitFailure)
import System.Directory (findExecutable)
import Control.Exception (catch, SomeException)

import Gradebook.Config (Config(..), DbType(..), loadConfig)
import Gradebook.Database (initDatabase, insertStudent, searchStudents)
import Gradebook.Roster (parseRosterCSV)

-- | Open database connection based on config
openConnection :: Config -> IO Connection
openConnection config = case dbType config of
  SQLite -> connectSqlite3 (T.unpack $ database config)
  PostgreSQL -> error "PostgreSQL support not yet implemented. Please use SQLite."

-- | Load roster CSV into database
runLoadRoster :: FilePath -> IO ()
runLoadRoster rosterPath = do
  putStrLn $ "Loading roster from: " ++ rosterPath

  -- Load config
  config <- loadConfig "config.yaml"

  -- Parse roster CSV
  result <- parseRosterCSV rosterPath
  students <- case result of
    Left err -> do
      putStrLn $ "Error parsing CSV: " ++ err
      exitFailure
    Right s -> return s

  putStrLn $ "Parsed " ++ show (length students) ++ " students"

  -- Connect to database
  conn <- openConnection config

  -- Initialize database schema
  initDatabase conn

  -- Insert each student
  mapM_ (insertStudent conn) students

  -- Commit and close
  commit conn
  disconnect conn

  putStrLn $ "Successfully loaded " ++ show (length students) ++ " students into database"

-- | Search for a student using fzf and output their netid
runSearchNetId :: IO ()
runSearchNetId = do
  -- Check if fzf is available
  fzfPath <- findExecutable "fzf"
  case fzfPath of
    Nothing -> do
      putStrLn "Error: fzf not found in PATH"
      putStrLn "Please install fzf or ensure it's in your PATH"
      putStrLn "If running outside nix shell, try: nix develop --command stack exec gb -- netid"
      exitFailure
    Just fzfExe -> do
      -- Load config
      config <- loadConfig "config.yaml"

      -- Connect to database
      conn <- openConnection config

      -- Get all students (search with empty string matches all)
      students <- searchStudents conn ""

      disconnect conn

      if null students
        then do
          putStrLn "No students found in database. Run 'gb load-roster' first."
          exitFailure
        else do
          -- Format for fzf: "NetID | Name | Email | UIN"
          let fzfInput = unlines
                [ T.unpack $ T.intercalate " | " [netid, name, email, uin]
                | (netid, name, email, uin) <- students
                ]

          -- Run fzf
          result <- (readProcess fzfExe ["--height=40%", "--reverse"] fzfInput)
                    `catch` (\(e :: SomeException) -> do
                      putStrLn $ "Error running fzf: " ++ show e
                      putStrLn "Selection may have been cancelled"
                      exitFailure
                    )

          -- Extract netid (first field before |)
          let netid = takeWhile (/= '|') result
          putStrLn $ trim netid
  where
    trim = T.unpack . T.strip . T.pack
