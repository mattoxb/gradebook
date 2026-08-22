{-# LANGUAGE OverloadedStrings #-}

-- | Rebuild the entire gradebook database from the committed CSVs.
--
-- The gradebook's authoritative state is the git repo, not Postgres. Every
-- table here is a pure function of the files in @data-files/@ and
-- @score-files/@, so a rebuild can drop everything and replay from scratch.
--
-- Why this exists: the incremental @load-*@ commands are gated on
-- change-detection ("did the CSV change since last time?"). That gate answers a
-- question about a *cache*, but it was being used to decide whether to write to
-- the *database* — and the two only agree if the DB already reflects the
-- current CSV. Nothing guaranteed that. One failed download or aborted run and
-- the CSV and DB desynchronise permanently, because the next run sees "no
-- change" and skips the load forever. It is a latch: once wrong, silently
-- wrong. (Su26: three students' exam-3 scores were absent from one host's DB
-- for weeks while the CSV on disk was byte-identical to the good host's.)
--
-- Making the DB reproducible dissolves that whole class of bug. Two hosts agree
-- iff their git checkouts agree, which is trivially checkable; any drift is
-- erased on the next rebuild rather than persisting invisibly.
module Gradebook.Rebuild
  ( runRebuild
  , RebuildPlan(..)
  , planRebuild
  ) where

import Database.HDBC
import Control.Monad (forM_, unless, when)
import Data.List (isPrefixOf, isSuffixOf, sortOn, stripPrefix)
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.Exit (exitFailure)
import System.FilePath ((</>), takeFileName)

import Gradebook.Config (Config(..), GradingConfig(..), ExamConfig(..), loadConfig)
import Gradebook.Database (initDatabase)
import Gradebook.Commands
  ( openConnection
  , runLoadRoster
  , runLoadDropped
  , runLoadCategories
  , runLoadAssignments
  , runLoadScores
  , runLoadPenalties
  , runLoadExamZones
  , runLoadExam
  , runLoadExamOverrides
  )

-- | Everything a rebuild will replay, in dependency order.
data RebuildPlan = RebuildPlan
  { rpRoster      :: FilePath
  , rpCategories  :: FilePath
  , rpAssignments :: FilePath
  , rpScoreFiles  :: [FilePath]
    -- ^ every @score-files/*.csv@ (MP/activity/POGIL scores)
  , rpPenalties   :: Maybe FilePath
  , rpPrimaryExams :: [(String, FilePath, Maybe FilePath)]
    -- ^ (slug, zones CSV, instance-questions CSV)
  , rpRetakeExams  :: [(String, FilePath, Maybe FilePath)]
    -- ^ same, but loaded strictly after every primary (see 'planRebuild')
  , rpOverrides   :: [(String, FilePath)]
    -- ^ (exam slug, overrides CSV), applied last
  } deriving (Show)

dataDir, scoreDir :: FilePath
dataDir  = "data-files"
scoreDir = "score-files"

-- | Tables dropped by a rebuild, in an order that respects the foreign keys.
-- @CASCADE@ makes the order redundant, but being explicit documents the
-- dependency structure and keeps the drop honest if a FK is ever removed.
rebuildTables :: [String]
rebuildTables =
  [ "penalties"
  , "exam_question_scores"
  , "exam_questions"
  , "exam_zones"
  , "scores"
  , "assignments"
  , "categories"
  , "students"
  ]

-- | Work out what to replay. Pure-ish: only touches the filesystem to see
-- which files exist.
--
-- Ordering is load-bearing in two places:
--
-- * Primary exams must be loaded before retakes. @load-exam@ on a retake slug
--   calls @loadExamScoresWithRetake@, which reads the primary exam's rows out
--   of the DB to compute the combined score. Load a retake first and it
--   combines against nothing.
--
-- * Overrides must come last. The score loader deliberately refuses to clobber
--   a row already marked as an override, so applying overrides before scores
--   would let the next score load silently win.
planRebuild :: Config -> IO RebuildPlan
planRebuild config = do
  dataFiles <- listDirectoryIfExists dataDir
  scoreFiles <- listDirectoryIfExists scoreDir

  let examCfgs = maybe [] exams (grading config)
      primarySlugs = [T.unpack (examSlug c) | c <- examCfgs]
      retakeSlugs  = mapMaybe (fmap T.unpack . examRetakeSlug) examCfgs

      -- A zones CSV is the marker that an exam is known to the gradebook; the
      -- instance-questions CSV may legitimately be absent (exam released but
      -- nobody has taken it yet).
      --
      -- Two naming conventions are in play and both must be honoured:
      --   * `<slug>-instance-questions.csv` -- the per-slug splits that
      --     bin/splitFinalRetake derives from the combined final (retakes).
      --   * `<prefix>_<TAG>_instance_questions.csv` -- PrairieLearn's own
      --     download filename, kept verbatim for primary exams.
      -- Preferring the per-slug name means a split file wins when both exist.
      examEntry slug =
        ( slug
        , dataDir </> (slug ++ "-zones.csv")
        , (dataDir </> (slug ++ "-instance-questions.csv"))
          : [ dataDir </> f
            | f <- dataFiles
            , "_instance_questions.csv" `isSuffixOf` f
            , matchesTag slug f ]
        )

      -- `exam-1` -> tag `E1`, `exam-2-retake` -> `E2f`. Derived here rather
      -- than read from the PL definitions so a rebuild stays offline.
      matchesTag slug f =
        case stripPrefix "exam-" slug of
          Nothing -> False
          Just rest ->
            let (n, suffix) = span (/= '-') rest
                tag = "E" ++ n ++ (if null suffix then "" else "f")
            in ("_" ++ tag ++ "_instance_questions.csv") `isSuffixOf` f

  primaries <- mapM (resolveExam . examEntry) primarySlugs
  retakes   <- mapM (resolveExam . examEntry) retakeSlugs

  penaltiesPath <- optionalFile (dataDir </> "penalties.csv")

  -- Override CSVs are named `<exam-slug>-<question>-overrides.csv`. Attribute
  -- each to its exam by longest-matching slug, so `exam-1-retake-*` is not
  -- mistaken for an `exam-1` override (the retake slug has the primary slug as
  -- a prefix).
  let allSlugs = sortOn (negate . length) (primarySlugs ++ retakeSlugs)
      overrideFiles =
        [ f | f <- dataFiles, "-overrides.csv" `isSuffixOf` f ]
      attribute f =
        case [ s | s <- allSlugs, (s ++ "-") `isPrefixOf` f ] of
          (s:_) -> Just (s, dataDir </> f)
          []    -> Nothing

  return RebuildPlan
    { rpRoster       = dataDir </> "roster.csv"
    , rpCategories   = dataDir </> "categories.csv"
    , rpAssignments  = dataDir </> "assignments.csv"
    , rpScoreFiles   = sortOn id [scoreDir </> f | f <- scoreFiles, ".csv" `isSuffixOf` f]
    , rpPenalties    = penaltiesPath
    , rpPrimaryExams = primaries
    , rpRetakeExams  = retakes
    , rpOverrides    = sortOn fst (mapMaybe attribute (sortOn id overrideFiles))
    }
  where
    -- Take the first candidate scores file that actually exists.
    resolveExam (slug, zones, candidates) = do
      hasZones <- doesFileExist zones
      found <- firstExisting candidates
      return ( slug
             , if hasZones then zones else ""
             , found )

    firstExisting [] = return Nothing
    firstExisting (p:ps) = do
      ok <- doesFileExist p
      if ok then return (Just p) else firstExisting ps

-- | @listDirectory@, but an absent directory is empty rather than an error.
listDirectoryIfExists :: FilePath -> IO [FilePath]
listDirectoryIfExists d = do
  ok <- doesDirectoryExist d
  if ok then map takeFileName <$> listDirectory d else return []

optionalFile :: FilePath -> IO (Maybe FilePath)
optionalFile p = do
  ok <- doesFileExist p
  return (if ok then Just p else Nothing)

-- | Drop and replay everything.
--
-- @force@ currently only affects how loudly we complain about missing inputs:
-- a rebuild is always a full replay. It exists so the caller can distinguish
-- "rebuild what's here" from "I know some inputs are stale, rebuild anyway".
runRebuild :: Bool -> IO ()
runRebuild force = do
  config <- loadConfig "config.yaml"
  plan <- planRebuild config

  -- Fail before touching the DB if the irreplaceable inputs are missing;
  -- dropping every table and then discovering there is no roster would be a
  -- spectacularly bad outcome.
  forM_ [rpRoster plan, rpCategories plan, rpAssignments plan] $ \p -> do
    ok <- doesFileExist p
    unless ok $ do
      putStrLn $ "Error: required input missing: " ++ p
      putStrLn "Refusing to rebuild — the database would be left empty."
      exitFailure

  putStrLn "== Dropping existing tables =="
  conn <- openConnection config
  forM_ rebuildTables $ \t ->
    run conn ("DROP TABLE IF EXISTS " ++ t ++ " CASCADE") [] >> return ()
  commit conn
  initDatabase conn
  commit conn
  disconnect conn

  putStrLn "== Roster / categories / assignments =="
  runLoadRoster (rpRoster plan)
  -- After the roster: `roster.csv` is a current snapshot, so students who
  -- dropped are absent from it entirely. The drop record re-creates them
  -- (enrolled = FALSE) so their early scores still have a row to attach to.
  runLoadDropped (dataDir </> "dropped.csv")
  runLoadCategories (rpCategories plan)
  runLoadAssignments (rpAssignments plan)

  putStrLn "== Assignment scores =="
  forM_ (rpScoreFiles plan) runLoadScores

  putStrLn "== Exam structure and scores (primaries) =="
  forM_ (rpPrimaryExams plan) (loadExam force)

  -- Retakes strictly after primaries: the retake loader combines against the
  -- primary's rows, which must already be in the DB.
  putStrLn "== Exam structure and scores (retakes) =="
  forM_ (rpRetakeExams plan) (loadExam force)

  -- Overrides last: the score loaders refuse to overwrite override rows, so
  -- these must be applied on top of a fully loaded score set.
  putStrLn "== Exam overrides =="
  forM_ (rpOverrides plan) $ \(slug, path) ->
    runLoadExamOverrides slug path

  forM_ (rpPenalties plan) $ \p -> do
    putStrLn "== Penalties =="
    runLoadPenalties p

  putStrLn "Rebuild complete."
  where
    loadExam f (slug, zones, mscores) = do
      hasZones <- doesFileExist zones
      if not hasZones
        then when f $ putStrLn $ "  (no zones CSV for " ++ slug ++ ", skipping)"
        else do
          runLoadExamZones slug zones
          forM_ mscores (runLoadExam slug)
