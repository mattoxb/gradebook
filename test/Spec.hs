{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Test.Hspec

import Gradebook.Database (ExamQuestionScore(..))
import Gradebook.ExamScores (PrairieLearnRow(..), buildQuestionScores)
import Gradebook.InfoAssessment
  ( InfoAssessment(..)
  , flattenQuestions
  , graderZones
  , Zone(..)
  )

main :: IO ()
main = hspec $ do
  describe "Gradebook.InfoAssessment" $ do
    it "skips Workspace zones (points: 0)" $ do
      let json = sampleInfoAssessment
      Right info <- pure (A.eitherDecode (BL.pack json) :: Either String InfoAssessment)
      map (zTitle . snd) (graderZones info) `shouldBe`
        ["Continuation Passing Style", "Interpreters"]

    it "preserves PrairieLearn zone numbering (JSON index + 1)" $ do
      -- Workspace is at JSON index 0 → zone 1 (filtered out).
      -- Graded zones keep numbers 2, 3 — not repacked to 1, 2.
      let json = sampleInfoAssessment
      Right info <- pure (A.eitherDecode (BL.pack json) :: Either String InfoAssessment)
      map fst (graderZones info) `shouldBe` [2, 3]

    it "numbers questions in JSON order, not alphabetical" $ do
      -- Interpreters zone has short-answer FIRST in JSON; alphabetical would
      -- put `mc/...` first.
      let json = sampleInfoAssessment
      Right info <- pure (A.eitherDecode (BL.pack json) :: Either String InfoAssessment)
      let rows = flattenQuestions info
      filter (\(z, _, _) -> z == 3) rows `shouldBe`
        [ (3, 1, "short-answer/procedure-vs-function-exam")
        , (3, 2, "mc/interpreters/closures-02")
        , (3, 2, "mc/interpreters/closures-03")
        ]

  describe "Gradebook.ExamScores.buildQuestionScores" $ do
    let qmap = M.fromList
          [ ((2, "code-haskell/continuations/composing/min"), 1)
          , ((2, "short-answer/cps-vs-tail-exam"), 2)
          ]
        plRow zone qid score =
          PrairieLearnRow
            { plrUID = "x@illinois.edu"
            , plrUIN = ""
            , plrName = ""
            , plrRole = "Student"
            , plrAssessment = "Exam 2 Retake"
            , plrZoneNumber = zone
            , plrZoneTitle = "z"
            , plrQuestion = qid
            , plrQuestionPoints = score
            , plrMaxPoints = 10
            }

    it "assigns question_numbers from the lookup map" $ do
      let rows =
            [ plRow 2 "short-answer/cps-vs-tail-exam" 10
            , plRow 2 "code-haskell/continuations/composing/min" 10
            ]
      let Right scores = buildQuestionScores qmap "exam-2-retake" "aegould2" rows
      map eqsQuestionNumber scores `shouldBe` [2, 1]
      map eqsQuestionId scores `shouldBe`
        ["short-answer/cps-vs-tail-exam", "code-haskell/continuations/composing/min"]

    it "hard-fails on a question_id not in the map" $ do
      let rows = [plRow 2 "unknown/question" 5]
      case buildQuestionScores qmap "exam-2-retake" "aegould2" rows of
        Left err -> do
          T.isInfixOf "Error: PrairieLearn CSV contains a question" err
            `shouldBe` True
          T.isInfixOf "unknown/question" err `shouldBe` True
          T.isInfixOf "gb load-exam-zones" err `shouldBe` True
        Right _ -> expectationFailure "expected Left, got Right"

-- | Trimmed-down fixture mirroring midterm2's structure: a Workspace zone
-- followed by two graded zones (Zone 3 puts short-answer FIRST to exercise
-- the JSON-order-vs-alphabetical-order distinction).
sampleInfoAssessment :: String
sampleInfoAssessment = unlines
  [ "{"
  , "  \"type\": \"Exam\","
  , "  \"title\": \"Midterm 2\","
  , "  \"zones\": ["
  , "    {"
  , "      \"title\": \"Workspace\","
  , "      \"questions\": ["
  , "        { \"numberChoose\": 1, \"points\": 0, \"alternatives\": [ { \"id\": \"workspace\" } ] }"
  , "      ]"
  , "    },"
  , "    {"
  , "      \"title\": \"Continuation Passing Style\","
  , "      \"questions\": ["
  , "        { \"numberChoose\": 1, \"points\": [10, 10],"
  , "          \"alternatives\": ["
  , "            { \"id\": \"code-haskell/continuations/composing/sqr\" },"
  , "            { \"id\": \"code-haskell/continuations/composing/min\" }"
  , "          ] },"
  , "        { \"numberChoose\": 1, \"manualPoints\": 10,"
  , "          \"alternatives\": ["
  , "            { \"id\": \"short-answer/cps-vs-tail-exam\" }"
  , "          ] }"
  , "      ]"
  , "    },"
  , "    {"
  , "      \"title\": \"Interpreters\","
  , "      \"questions\": ["
  , "        { \"numberChoose\": 1, \"manualPoints\": 10,"
  , "          \"alternatives\": ["
  , "            { \"id\": \"short-answer/procedure-vs-function-exam\" }"
  , "          ] },"
  , "        { \"numberChoose\": 1, \"points\": [10, 8],"
  , "          \"alternatives\": ["
  , "            { \"id\": \"mc/interpreters/closures-02\" },"
  , "            { \"id\": \"mc/interpreters/closures-03\" }"
  , "          ] }"
  , "      ]"
  , "    }"
  , "  ]"
  , "}"
  ]
