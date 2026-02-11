{-# LANGUAGE OverloadedStrings #-}

module Gradebook.Database
  ( initDatabase
  , insertStudent
  , searchStudents
  , Student(..)
  ) where

import Database.HDBC
import qualified Data.Text as T

-- | Student record from roster
data Student = Student
  { netId         :: T.Text
  , uin           :: T.Text
  , admitTerm     :: T.Text
  , gender        :: T.Text
  , name          :: T.Text
  , email         :: T.Text
  , credit        :: T.Text
  , level         :: T.Text
  , year          :: T.Text
  , subject       :: T.Text
  , number        :: T.Text
  , section       :: T.Text
  , crn           :: T.Text
  , degreeName    :: T.Text
  , major1Name    :: T.Text
  , college       :: T.Text
  , programCode   :: T.Text
  , programName   :: T.Text
  , ferpa         :: T.Text
  , honorsCredit  :: T.Text
  , advisors      :: T.Text
  } deriving (Show, Eq)

-- | Initialize the database schema
initDatabase :: IConnection conn => conn -> IO ()
initDatabase conn = do
  _ <- run conn createTableSQL []
  commit conn
  where
    createTableSQL = unlines
      [ "CREATE TABLE IF NOT EXISTS students ("
      , "  netid TEXT PRIMARY KEY,"
      , "  uin TEXT NOT NULL,"
      , "  admit_term TEXT,"
      , "  gender TEXT,"
      , "  name TEXT NOT NULL,"
      , "  email TEXT NOT NULL,"
      , "  credit TEXT,"
      , "  level TEXT,"
      , "  year TEXT,"
      , "  subject TEXT,"
      , "  number TEXT,"
      , "  section TEXT,"
      , "  crn TEXT,"
      , "  degree_name TEXT,"
      , "  major_1_name TEXT,"
      , "  college TEXT,"
      , "  program_code TEXT,"
      , "  program_name TEXT,"
      , "  ferpa TEXT,"
      , "  honors_credit TEXT,"
      , "  advisors TEXT"
      , ")"
      ]

-- | Insert a student into the database
insertStudent :: IConnection conn => conn -> Student -> IO ()
insertStudent conn student = do
  _ <- run conn insertSQL
    [ toSql $ netId student
    , toSql $ uin student
    , toSql $ admitTerm student
    , toSql $ gender student
    , toSql $ name student
    , toSql $ email student
    , toSql $ credit student
    , toSql $ level student
    , toSql $ year student
    , toSql $ subject student
    , toSql $ number student
    , toSql $ section student
    , toSql $ crn student
    , toSql $ degreeName student
    , toSql $ major1Name student
    , toSql $ college student
    , toSql $ programCode student
    , toSql $ programName student
    , toSql $ ferpa student
    , toSql $ honorsCredit student
    , toSql $ advisors student
    ]
  return ()
  where
    insertSQL = unlines
      [ "INSERT OR REPLACE INTO students"
      , "(netid, uin, admit_term, gender, name, email, credit, level, year,"
      , " subject, number, section, crn, degree_name, major_1_name, college,"
      , " program_code, program_name, ferpa, honors_credit, advisors)"
      , "VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
      ]

-- | Search for students by netid, name, email, or UIN
-- Returns list of (netid, name, email, uin) tuples
searchStudents :: IConnection conn => conn -> T.Text -> IO [(T.Text, T.Text, T.Text, T.Text)]
searchStudents conn searchTerm = do
  let pattern = "%" <> searchTerm <> "%"
  results <- quickQuery' conn searchSQL
    [ toSql pattern
    , toSql pattern
    , toSql pattern
    , toSql pattern
    ]
  return $ map rowToTuple results
  where
    searchSQL = unlines
      [ "SELECT netid, name, email, uin FROM students"
      , "WHERE netid LIKE ? OR name LIKE ? OR email LIKE ? OR uin LIKE ?"
      , "ORDER BY name"
      ]

    rowToTuple :: [SqlValue] -> (T.Text, T.Text, T.Text, T.Text)
    rowToTuple [netid', name', email', uin'] =
      ( fromSql netid'
      , fromSql name'
      , fromSql email'
      , fromSql uin'
      )
    rowToTuple _ = error "Unexpected row format from database"
