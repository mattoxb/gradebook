{-# LANGUAGE OverloadedStrings #-}

module Gradebook.Reports
  ( ReportData(..)
  , generateReport
  ) where

import qualified Data.Text as T
import Gradebook.GradeCalculation (CategoryGrade(..), RequirementResult(..), ExamGrade(..))
import Gradebook.Config (GradingConfig(..), GradingMode(..), GradeThreshold(..))
import qualified Gradebook.Reports.Default as Default
import qualified Gradebook.Reports.CS421 as CS421
import qualified Gradebook.Reports.CS491 as CS491

-- | All data a report formatter might need
data ReportData = ReportData
  { rdNetid          :: T.Text
  , rdStudentName    :: Maybe T.Text
  , rdGradingConfig  :: GradingConfig
  , rdCategoryGrades :: [CategoryGrade]
  , rdExamGrades     :: [(ExamGrade, Double)]
  , rdRequirements   :: [RequirementResult]
  , rdThresholds     :: [GradeThreshold]
  }

-- | Generate a report by dispatching to the appropriate formatter
-- based on the report-format field in the grading config.
generateReport :: ReportData -> T.Text
generateReport rd =
  let fmt = reportFormat (rdGradingConfig rd)
  in case fmt of
    "cs421-v1" -> CS421.formatReport
                    (rdNetid rd)
                    (rdStudentName rd)
                    (rdGradingConfig rd)
                    (rdCategoryGrades rd)
                    (rdExamGrades rd)
                    (rdThresholds rd)

    "cs491-v1" -> CS491.formatReport
                    (rdNetid rd)
                    (rdStudentName rd)
                    (rdCategoryGrades rd)
                    (rdRequirements rd)

    -- Default format: dispatch by grading mode
    _ -> case gradingMode (rdGradingConfig rd) of
           Weighted ->
             Default.formatWeightedReport
               (rdNetid rd)
               (rdCategoryGrades rd)
               (rdExamGrades rd)
           PassFail ->
             Default.formatPassFailReport
               (rdNetid rd)
               (rdCategoryGrades rd)
               (rdRequirements rd)
               (rdExamGrades rd)
           LetterGrade ->
             Default.formatLetterGradeReport
               (rdNetid rd)
               (rdCategoryGrades rd)
               (rdThresholds rd)
               (rdExamGrades rd)
