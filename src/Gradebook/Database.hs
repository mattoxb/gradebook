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
  , getAllAssignmentSlugs
  , getAllStudentNetids
  -- Exam-related exports
  , ExamZone(..)
  , ExamQuestionScore(..)
  , insertExamZone
  , insertExamQuestionScore
  , getExamZonesForExam
  , getExamQuestionScoresForStudent
  , updateAssignmentScore
  , applyExamQuestionOverride
  , getAllExamSlugs
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

-- | Exam zone record (defines structure of an exam)
data ExamZone = ExamZone
  { ezExamSlug      :: T.Text   -- ^ Which exam this zone belongs to (e.g., "exam-1")
  , ezZoneNumber    :: Int      -- ^ Zone ordering number
  , ezZoneTitle     :: T.Text   -- ^ Zone display title (e.g., "Direct Recursion")
  , ezQuestionCount :: Int      -- ^ Number of questions in this zone
  } deriving (Show, Eq)

-- | Exam question score record (individual question scores)
data ExamQuestionScore = ExamQuestionScore
  { eqsNetId          :: T.Text        -- ^ Student netid
  , eqsExamSlug       :: T.Text        -- ^ Exam slug (e.g., "exam-1" or "exam-1r" for retake)
  , eqsZoneNumber     :: Int           -- ^ Zone number
  , eqsQuestionNumber :: Int           -- ^ Question number within zone (1-indexed)
  , eqsQuestionId     :: T.Text        -- ^ PrairieLearn question ID (e.g., "code-haskell/list-recursion/mulList")
  , eqsScore          :: Double        -- ^ Points earned
  , eqsMaxPoints      :: Double        -- ^ Maximum possible points
  , eqsOverrideReason :: Maybe T.Text  -- ^ Reason for manual override (if any)
  } deriving (Show, Eq)

-- | Initialize the database schema
initDatabase :: IConnection conn => conn -> IO ()
initDatabase conn = do
  _ <- run conn createStudentsTableSQL []
  _ <- run conn createCategoriesTableSQL []
  _ <- run conn createAssignmentsTableSQL []
  _ <- run conn createScoresTableSQL []
  _ <- run conn createExamZonesTableSQL []
  _ <- run conn createExamQuestionScoresTableSQL []
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
      , "  collected BOOLEAN NOT NULL DEFAULT FALSE,"
      , "  FOREIGN KEY (category) REFERENCES categories(slug) ON DELETE CASCADE"
      , ")"
      ]

    createScoresTableSQL = unlines
      [ "CREATE TABLE IF NOT EXISTS scores ("
      , "  netid TEXT NOT NULL,"
      , "  assignment TEXT NOT NULL,"
      , "  score REAL,"
      , "  excused BOOLEAN NOT NULL DEFAULT FALSE,"
      , "  PRIMARY KEY (netid, assignment),"
      , "  FOREIGN KEY (netid) REFERENCES students(netid) ON DELETE CASCADE,"
      , "  FOREIGN KEY (assignment) REFERENCES assignments(slug) ON DELETE CASCADE"
      , ")"
      ]

    -- Exam zones define the structure of an exam (zone titles and question counts)
    createExamZonesTableSQL = unlines
      [ "CREATE TABLE IF NOT EXISTS exam_zones ("
      , "  exam_slug TEXT NOT NULL,"
      , "  zone_number INTEGER NOT NULL,"
      , "  zone_title TEXT NOT NULL,"
      , "  question_count INTEGER NOT NULL DEFAULT 1,"
      , "  PRIMARY KEY (exam_slug, zone_number),"
      , "  FOREIGN KEY (exam_slug) REFERENCES assignments(slug) ON DELETE CASCADE"
      , ")"
      ]

    -- Individual question scores for each student on each exam
    createExamQuestionScoresTableSQL = unlines
      [ "CREATE TABLE IF NOT EXISTS exam_question_scores ("
      , "  netid TEXT NOT NULL,"
      , "  exam_slug TEXT NOT NULL,"
      , "  zone_number INTEGER NOT NULL,"
      , "  question_number INTEGER NOT NULL,"
      , "  question_id TEXT NOT NULL,"      -- PrairieLearn question ID for analysis
      , "  score REAL NOT NULL,"
      , "  max_points REAL NOT NULL,"
      , "  override_reason TEXT,"           -- Reason for manual override (NULL if not overridden)
      , "  PRIMARY KEY (netid, exam_slug, zone_number, question_number),"
      , "  FOREIGN KEY (netid) REFERENCES students(netid) ON DELETE CASCADE,"
      , "  FOREIGN KEY (exam_slug, zone_number) REFERENCES exam_zones(exam_slug, zone_number) ON DELETE CASCADE"
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
      [ "INSERT INTO students"
      , "(netid, uin, admit_term, gender, name, email, credit, level, year,"
      , " subject, number, section, crn, degree_name, major_1_name, college,"
      , " program_code, program_name, ferpa, honors_credit, advisors)"
      , "VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
      , "ON CONFLICT (netid) DO UPDATE SET"
      , "  uin = EXCLUDED.uin,"
      , "  admit_term = EXCLUDED.admit_term,"
      , "  gender = EXCLUDED.gender,"
      , "  name = EXCLUDED.name,"
      , "  email = EXCLUDED.email,"
      , "  credit = EXCLUDED.credit,"
      , "  level = EXCLUDED.level,"
      , "  year = EXCLUDED.year,"
      , "  subject = EXCLUDED.subject,"
      , "  number = EXCLUDED.number,"
      , "  section = EXCLUDED.section,"
      , "  crn = EXCLUDED.crn,"
      , "  degree_name = EXCLUDED.degree_name,"
      , "  major_1_name = EXCLUDED.major_1_name,"
      , "  college = EXCLUDED.college,"
      , "  program_code = EXCLUDED.program_code,"
      , "  program_name = EXCLUDED.program_name,"
      , "  ferpa = EXCLUDED.ferpa,"
      , "  honors_credit = EXCLUDED.honors_credit,"
      , "  advisors = EXCLUDED.advisors"
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
    insertSQL = unlines
      [ "INSERT INTO categories (slug, title)"
      , "VALUES (?, ?)"
      , "ON CONFLICT (slug) DO UPDATE SET"
      , "  title = EXCLUDED.title"
      ]

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
      [ "INSERT INTO assignments"
      , "(slug, order_num, start_date, category, max_points, title, collected)"
      , "VALUES (?, ?, ?, ?, ?, ?, ?)"
      , "ON CONFLICT (slug) DO UPDATE SET"
      , "  order_num = EXCLUDED.order_num,"
      , "  start_date = EXCLUDED.start_date,"
      , "  category = EXCLUDED.category,"
      , "  max_points = EXCLUDED.max_points,"
      , "  title = EXCLUDED.title,"
      , "  collected = EXCLUDED.collected"
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
      [ "INSERT INTO scores"
      , "(netid, assignment, score, excused)"
      , "VALUES (?, ?, ?, ?)"
      , "ON CONFLICT (netid, assignment) DO UPDATE SET"
      , "  score = EXCLUDED.score,"
      , "  excused = EXCLUDED.excused"
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
        (sqlValueToBool' collected')
      where
        -- Helper to convert SqlValue to Bool (handles both INTEGER and TEXT)
        sqlValueToBool' :: SqlValue -> Bool
        sqlValueToBool' SqlNull = False
        sqlValueToBool' (SqlBool b) = b
        sqlValueToBool' (SqlInteger 0) = False
        sqlValueToBool' (SqlInteger _) = True
        sqlValueToBool' (SqlInt32 0) = False
        sqlValueToBool' (SqlInt32 _) = True
        sqlValueToBool' (SqlInt64 0) = False
        sqlValueToBool' (SqlInt64 _) = True
        sqlValueToBool' (SqlString "False") = False
        sqlValueToBool' (SqlString "false") = False
        sqlValueToBool' (SqlString "0") = False
        sqlValueToBool' (SqlString "") = False
        sqlValueToBool' _ = True
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
      , sqlValueToBool excused'
      , fromSql maxPoints'
      , fromSql category'
      , fromSql title'
      , sqlValueToBool collected'
      )
    rowToScoreTuple _ = error "Unexpected row format from scores query"

    -- Helper to convert SqlValue to Bool (handles INTEGER, TEXT, and BOOLEAN)
    sqlValueToBool :: SqlValue -> Bool
    sqlValueToBool SqlNull = False
    sqlValueToBool (SqlBool b) = b
    sqlValueToBool (SqlInteger 0) = False
    sqlValueToBool (SqlInteger _) = True
    sqlValueToBool (SqlInt32 0) = False
    sqlValueToBool (SqlInt32 _) = True
    sqlValueToBool (SqlInt64 0) = False
    sqlValueToBool (SqlInt64 _) = True
    sqlValueToBool (SqlString "False") = False
    sqlValueToBool (SqlString "false") = False
    sqlValueToBool (SqlString "0") = False
    sqlValueToBool (SqlString "") = False
    sqlValueToBool _ = True

-- | Get all category slugs from the database
getAllCategories :: IConnection conn => conn -> IO [T.Text]
getAllCategories conn = do
  results <- quickQuery' conn "SELECT slug FROM categories ORDER BY slug" []
  return $ map (\[slug'] -> fromSql slug') results

-- | Get all assignment slugs from the database
getAllAssignmentSlugs :: IConnection conn => conn -> IO [T.Text]
getAllAssignmentSlugs conn = do
  results <- quickQuery' conn "SELECT slug FROM assignments" []
  return $ map (\[slug'] -> fromSql slug') results

-- | Get all student netids from the database
getAllStudentNetids :: IConnection conn => conn -> IO [T.Text]
getAllStudentNetids conn = do
  results <- quickQuery' conn "SELECT netid FROM students" []
  return $ map (\[netid'] -> fromSql netid') results

-- | Insert an exam zone into the database
insertExamZone :: IConnection conn => conn -> ExamZone -> IO ()
insertExamZone conn zone = do
  _ <- run conn insertSQL
    [ toSql $ ezExamSlug zone
    , toSql $ ezZoneNumber zone
    , toSql $ ezZoneTitle zone
    , toSql $ ezQuestionCount zone
    ]
  return ()
  where
    insertSQL = unlines
      [ "INSERT INTO exam_zones"
      , "(exam_slug, zone_number, zone_title, question_count)"
      , "VALUES (?, ?, ?, ?)"
      , "ON CONFLICT (exam_slug, zone_number) DO UPDATE SET"
      , "  zone_title = EXCLUDED.zone_title,"
      , "  question_count = EXCLUDED.question_count"
      ]

-- | Insert an exam question score into the database
insertExamQuestionScore :: IConnection conn => conn -> ExamQuestionScore -> IO ()
insertExamQuestionScore conn qs = do
  _ <- run conn insertSQL
    [ toSql $ eqsNetId qs
    , toSql $ eqsExamSlug qs
    , toSql $ eqsZoneNumber qs
    , toSql $ eqsQuestionNumber qs
    , toSql $ eqsQuestionId qs
    , toSql $ eqsScore qs
    , toSql $ eqsMaxPoints qs
    , toSql $ eqsOverrideReason qs
    ]
  return ()
  where
    insertSQL = unlines
      [ "INSERT INTO exam_question_scores"
      , "(netid, exam_slug, zone_number, question_number, question_id, score, max_points, override_reason)"
      , "VALUES (?, ?, ?, ?, ?, ?, ?, ?)"
      , "ON CONFLICT (netid, exam_slug, zone_number, question_number) DO UPDATE SET"
      , "  question_id = EXCLUDED.question_id,"
      , "  score = EXCLUDED.score,"
      , "  max_points = EXCLUDED.max_points,"
      , "  override_reason = EXCLUDED.override_reason"
      ]

-- | Get all exam zones for a specific exam
getExamZonesForExam :: IConnection conn => conn -> T.Text -> IO [ExamZone]
getExamZonesForExam conn examSlug = do
  results <- quickQuery' conn querySQL [toSql examSlug]
  return $ map rowToExamZone results
  where
    querySQL = unlines
      [ "SELECT exam_slug, zone_number, zone_title, question_count"
      , "FROM exam_zones"
      , "WHERE exam_slug = ?"
      , "ORDER BY zone_number"
      ]

    rowToExamZone :: [SqlValue] -> ExamZone
    rowToExamZone [examSlug', zoneNum', zoneTitle', qCount'] =
      ExamZone
        (fromSql examSlug')
        (fromSql zoneNum')
        (fromSql zoneTitle')
        (fromSql qCount')
    rowToExamZone _ = error "Unexpected row format from exam_zones query"

-- | Get all exam question scores for a student on a specific exam
getExamQuestionScoresForStudent :: IConnection conn => conn -> T.Text -> T.Text -> IO [ExamQuestionScore]
getExamQuestionScoresForStudent conn netid examSlug = do
  results <- quickQuery' conn querySQL [toSql netid, toSql examSlug]
  return $ map (rowToExamQuestionScore netid examSlug) results
  where
    querySQL = unlines
      [ "SELECT zone_number, question_number, question_id, score, max_points, override_reason"
      , "FROM exam_question_scores"
      , "WHERE netid = ? AND exam_slug = ?"
      , "ORDER BY zone_number, question_number"
      ]

    rowToExamQuestionScore :: T.Text -> T.Text -> [SqlValue] -> ExamQuestionScore
    rowToExamQuestionScore netid' examSlug' [zoneNum', qNum', qId', score', maxPts', reason'] =
      ExamQuestionScore
        netid'
        examSlug'
        (fromSql zoneNum')
        (fromSql qNum')
        (fromSql qId')
        (fromSql score')
        (fromSql maxPts')
        (case reason' of
           SqlNull -> Nothing
           _ -> Just (fromSql reason'))
    rowToExamQuestionScore _ _ _ = error "Unexpected row format from exam_question_scores query"

-- | Apply an override to an exam question score (takes max of existing and override)
applyExamQuestionOverride :: IConnection conn => conn -> T.Text -> T.Text -> Int -> Int -> Double -> Double -> T.Text -> IO ()
applyExamQuestionOverride conn netid examSlug zoneNum qNum overrideScore overrideMaxPts reason = do
  _ <- run conn updateSQL
    [ toSql overrideScore
    , toSql overrideMaxPts
    , toSql reason
    , toSql netid
    , toSql examSlug
    , toSql zoneNum
    , toSql qNum
    ]
  return ()
  where
    -- Use GREATEST to take the better score; update reason regardless
    -- GREATEST works in both PostgreSQL and SQLite (3.38+)
    updateSQL = unlines
      [ "UPDATE exam_question_scores"
      , "SET score = GREATEST(score, CAST(? AS REAL)),"
      , "    max_points = ?,"
      , "    override_reason = ?"
      , "WHERE netid = ? AND exam_slug = ? AND zone_number = ? AND question_number = ?"
      ]

-- | Update the score for an assignment (used to set computed exam scores)
updateAssignmentScore :: IConnection conn => conn -> T.Text -> T.Text -> Double -> IO ()
updateAssignmentScore conn netid assignmentSlug scoreValue = do
  _ <- run conn insertSQL
    [ toSql netid
    , toSql assignmentSlug
    , toSql scoreValue
    ]
  return ()
  where
    insertSQL = unlines
      [ "INSERT INTO scores (netid, assignment, score, excused)"
      , "VALUES (?, ?, ?, FALSE)"
      , "ON CONFLICT (netid, assignment) DO UPDATE SET"
      , "  score = EXCLUDED.score"
      ]

-- | Get all distinct exam slugs from exam_zones table
getAllExamSlugs :: IConnection conn => conn -> IO [T.Text]
getAllExamSlugs conn = do
  results <- quickQuery' conn "SELECT DISTINCT exam_slug FROM exam_zones ORDER BY exam_slug" []
  return [fromSql slug | [slug] <- results]
