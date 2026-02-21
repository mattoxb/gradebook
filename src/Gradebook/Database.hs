{-# LANGUAGE OverloadedStrings #-}

module Gradebook.Database
  ( initDatabase
  , insertStudent
  , searchStudents
  , Student(..)
  , insertCategory
  , Category(..)
  , insertAssignment
  , Assignment(..)
  , insertScore
  , Score(..)
  , getAssignmentsByCategory
  , getScoresForStudent
  , getAllCategories
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

-- | Category record for grade categories
data Category = Category
  { categorySlug  :: T.Text
  , categoryTitle :: T.Text
  } deriving (Show, Eq)

-- | Assignment record
data Assignment = Assignment
  { assignmentOrder     :: Int
  , assignmentStartDate :: T.Text
  , assignmentCategory  :: T.Text
  , assignmentSlug      :: T.Text
  , assignmentMaxPoints :: Int
  , assignmentTitle     :: T.Text
  , assignmentCollected :: Bool
  } deriving (Show, Eq)

-- | Score record
data Score = Score
  { scoreNetId     :: T.Text
  , scoreAssignment :: T.Text
  , scoreValue     :: Maybe Double
  , scoreExcused   :: Bool
  } deriving (Show, Eq)

-- | Initialize the database schema
initDatabase :: IConnection conn => conn -> IO ()
initDatabase conn = do
  _ <- run conn createStudentsTableSQL []
  _ <- run conn createCategoriesTableSQL []
  _ <- run conn createAssignmentsTableSQL []
  _ <- run conn createScoresTableSQL []
  commit conn
  where
    createStudentsTableSQL = unlines
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

    createCategoriesTableSQL = unlines
      [ "CREATE TABLE IF NOT EXISTS categories ("
      , "  slug TEXT PRIMARY KEY,"
      , "  title TEXT NOT NULL"
      , ")"
      ]

    createAssignmentsTableSQL = unlines
      [ "CREATE TABLE IF NOT EXISTS assignments ("
      , "  slug TEXT PRIMARY KEY,"
      , "  order_num INTEGER NOT NULL,"
      , "  start_date TEXT NOT NULL,"
      , "  category TEXT NOT NULL,"
      , "  max_points INTEGER NOT NULL,"
      , "  title TEXT NOT NULL,"
      , "  collected INTEGER NOT NULL DEFAULT 0,"
      , "  FOREIGN KEY (category) REFERENCES categories(slug)"
      , ")"
      ]

    createScoresTableSQL = unlines
      [ "CREATE TABLE IF NOT EXISTS scores ("
      , "  netid TEXT NOT NULL,"
      , "  assignment TEXT NOT NULL,"
      , "  score REAL,"
      , "  excused INTEGER NOT NULL DEFAULT 0,"
      , "  PRIMARY KEY (netid, assignment),"
      , "  FOREIGN KEY (netid) REFERENCES students(netid),"
      , "  FOREIGN KEY (assignment) REFERENCES assignments(slug)"
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

-- | Insert a category into the database
insertCategory :: IConnection conn => conn -> Category -> IO ()
insertCategory conn category = do
  _ <- run conn insertSQL
    [ toSql $ categorySlug category
    , toSql $ categoryTitle category
    ]
  return ()
  where
    insertSQL = "INSERT OR REPLACE INTO categories (slug, title) VALUES (?, ?)"

-- | Insert an assignment into the database
insertAssignment :: IConnection conn => conn -> Assignment -> IO ()
insertAssignment conn assignment = do
  _ <- run conn insertSQL
    [ toSql $ assignmentSlug assignment
    , toSql $ assignmentOrder assignment
    , toSql $ assignmentStartDate assignment
    , toSql $ assignmentCategory assignment
    , toSql $ assignmentMaxPoints assignment
    , toSql $ assignmentTitle assignment
    , toSql $ assignmentCollected assignment
    ]
  return ()
  where
    insertSQL = unlines
      [ "INSERT OR REPLACE INTO assignments"
      , "(slug, order_num, start_date, category, max_points, title, collected)"
      , "VALUES (?, ?, ?, ?, ?, ?, ?)"
      ]

-- | Insert a score into the database
insertScore :: IConnection conn => conn -> Score -> IO ()
insertScore conn score = do
  _ <- run conn insertSQL
    [ toSql $ scoreNetId score
    , toSql $ scoreAssignment score
    , toSql $ scoreValue score
    , toSql $ scoreExcused score
    ]
  return ()
  where
    insertSQL = unlines
      [ "INSERT OR REPLACE INTO scores"
      , "(netid, assignment, score, excused)"
      , "VALUES (?, ?, ?, ?)"
      ]

-- | Get all assignments for a specific category
getAssignmentsByCategory :: IConnection conn => conn -> T.Text -> IO [Assignment]
getAssignmentsByCategory conn categorySlug = do
  results <- quickQuery' conn querySQL [toSql categorySlug]
  return $ map rowToAssignment results
  where
    querySQL = unlines
      [ "SELECT order_num, start_date, category, slug, max_points, title, collected"
      , "FROM assignments"
      , "WHERE category = ?"
      , "ORDER BY order_num"
      ]

    rowToAssignment :: [SqlValue] -> Assignment
    rowToAssignment [order', startDate', category', slug', maxPoints', title', collected'] =
      Assignment
        (fromSql order')
        (fromSql startDate')
        (fromSql category')
        (fromSql slug')
        (fromSql maxPoints')
        (fromSql title')
        (fromSql collected')
    rowToAssignment _ = error "Unexpected row format from assignments query"

-- | Get all scores for a specific student
-- Returns (assignment_slug, score, excused, max_points, category, title, collected)
getScoresForStudent :: IConnection conn => conn -> T.Text -> IO [(T.Text, Maybe Double, Bool, Int, T.Text, T.Text, Bool)]
getScoresForStudent conn netid = do
  results <- quickQuery' conn querySQL [toSql netid]
  return $ map rowToScoreTuple results
  where
    querySQL = unlines
      [ "SELECT a.slug, s.score, s.excused, a.max_points, a.category, a.title, a.collected"
      , "FROM assignments a"
      , "LEFT JOIN scores s ON a.slug = s.assignment AND s.netid = ?"
      , "ORDER BY a.order_num"
      ]

    rowToScoreTuple :: [SqlValue] -> (T.Text, Maybe Double, Bool, Int, T.Text, T.Text, Bool)
    rowToScoreTuple [slug', score', excused', maxPoints', category', title', collected'] =
      ( fromSql slug'
      , case score' of
          SqlNull -> Nothing
          _ -> Just (fromSql score')
      , case excused' of
          SqlNull -> False
          _ -> fromSql excused'
      , fromSql maxPoints'
      , fromSql category'
      , fromSql title'
      , fromSql collected'
      )
    rowToScoreTuple _ = error "Unexpected row format from scores query"

-- | Get all category slugs from the database
getAllCategories :: IConnection conn => conn -> IO [T.Text]
getAllCategories conn = do
  results <- quickQuery' conn "SELECT slug FROM categories ORDER BY slug" []
  return $ map (\[slug'] -> fromSql slug') results
