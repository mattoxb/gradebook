# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is a CS421 (Programming Languages and Compilers) gradebook management system implemented in Haskell. The project processes student roster data and manages grade information for the course.

## Live Course Data

Sample data from live courses is available at:
- `~/class/cs421-haskell/gradebook` - CS 421 Programming Languages (Spring 2026)
- `~/class/cs491cap/gradebook` - CS 491 CAP course

These contain real `config.yaml` files, roster data, and score files. Do not modify the production databases; create test databases for development.

## Data Files

The `data-files/` directory contains:
- `roster.csv`: Course roster with student information including Net ID, UIN, name, email, program, college, and advisor details
  - Contains FERPA-protected student data
  - CSV format with 22 columns including demographic and academic information
- `categories.csv`: Grade category definitions (slug, title)
- `assignments.csv`: Assignment definitions (order, start_date, category, slug, max_points, title, collected)

## Development Environment

This project uses **Nix flakes** with **haskell.nix** for reproducible development environments.

### Setup
```bash
# Build the project
nix build

# Run the executable
./result/bin/gb --help

# Enter development shell (for cabal, HLS, etc.)
nix develop

# Or use direnv for automatic shell activation
direnv allow
```

### Build Commands
- **Build**: `nix build`
- **Run**: `./result/bin/gb <command>` or `nix run . -- <command>`
- **Dev shell**: `nix develop` (provides cabal, HLS, ghc)
- **In dev shell**: `cabal build`, `cabal run gb -- <command>`

### Available Commands
- `gb load-roster [-r FILE]`: Load roster CSV into database
- `gb load-categories [-c FILE]`: Load grade categories
- `gb load-assignments [-a FILE]`: Load assignments
- `gb load-scores FILE`: Load student scores from CSV
- `gb load-exam -e SLUG FILE`: Load exam scores from PrairieLearn CSV
- `gb report [-n NETID] [-p] [-a]`: Generate grade report
- `gb collect SLUG...`: Mark assignments as collected
- `gb netid`: Interactive student search using fzf
- `gb version`: Show version information

### Nix Configuration Details
- GHC version: 9.8.4 (configured in flake.nix via haskell.nix)
- Haskell Language Server (HLS) is available in dev shell
- PostgreSQL and SQLite clients available for database access

## Code Architecture

### Module Structure

```
src/Gradebook/
├── Database.hs      -- Database schema and operations (HDBC)
├── Config.hs        -- Configuration file reading (YAML)
├── Roster.hs        -- CSV roster parsing (cassava)
├── Categories.hs    -- CSV categories parsing
├── Assignments.hs   -- CSV assignments parsing
├── Scores.hs        -- CSV scores parsing
├── ExamScores.hs    -- PrairieLearn exam CSV parsing
├── ExamOverrides.hs -- Exam score override parsing
├── Version.hs       -- Version information
├── GradeCalculation.hs -- Grade computation logic
├── Reports.hs       -- Report formatting and generation
├── Commands.hs      -- Command implementations
└── CLI.hs           -- CLI parser (optparse-applicative)

app/
└── Main.hs          -- Executable entry point
```

### Key Components

- **Database Layer** (Database.hs): Uses HDBC for database abstraction, supporting both SQLite3 and PostgreSQL. Tables: `students`, `categories`, `assignments`, `scores`, `exam_zones`, `exam_question_scores`.

- **Configuration** (Config.hs): Reads `config.yaml` for database settings, grading configuration (weighted/pass-fail/letter-grade modes), category weights, and exam configurations.

- **Grade Calculation** (GradeCalculation.hs): Computes category grades with drop-lowest logic, evaluates pass/fail requirements, calculates letter grades from thresholds, and combines exam scores with retake policies.

- **Exam Support** (ExamScores.hs, Commands.hs): Parses PrairieLearn `instance_questions` CSV exports, stores individual question scores per zone, supports retake policies (max, max-if-better-avg-if-worse).

- **Reports** (Reports.hs): Generates formatted grade reports including exam zone breakdowns.

### Database Schema

**Core Tables:**
- `students`: Student roster (netid PK, uin, name, email, etc.)
- `categories`: Grade categories (slug PK, title)
- `assignments`: Assignments (slug PK, order_num, category FK, max_points, title, collected)
- `scores`: Student scores (netid FK, assignment FK, score, excused)

**Exam Tables:**
- `exam_zones`: Exam structure (exam_slug FK, zone_number, zone_title, question_count)
- `exam_question_scores`: Individual question scores (netid FK, exam_slug, zone_number, question_number, question_id, score, max_points)

### Configuration Example

```yaml
database: cs421-grades-sp26.db
db-type: sqlite3  # or postgresql
repo-prefix: "https://github.com/org/prefix_"

grading:
  mode: weighted  # or pass-fail, letter-grade
  categories:
    exams:
      weight: 0.20
      drop-lowest: 0
    activities:
      weight: 0.30
      drop-lowest: 2
  exams:
    - slug: exam-1
      title: "Midterm 1"
      retake-slug: exam-1-retake
      retake-policy: max  # or max-if-better-avg-if-worse
      final-slug: exam-1f  # optional
      final-policy: max    # optional
```

### Exam Loading Workflow

1. Load the primary exam: `gb load-exam -e exam-1 midterm1_instance_questions.csv`
2. Load retake (if any): `gb load-exam -e exam-1-retake midterm1_retake_instance_questions.csv`
   - Automatically detects retake from config and combines scores
3. Apply overrides (if needed): `gb load-exam-overrides -e exam-1 overrides.csv`
   - Takes max of existing score and override (won't lower scores)
   - Recalculates overall exam score after applying overrides
4. Generate report: `gb report -n netid` shows zone-by-zone breakdown

### Exam Override CSV Format

For correcting bad questions or giving makeup credit:

```csv
netid,zone_number,question_number,score,max_points,reason
student1,7,1,10,10,bad lambda calculus variant
student2,7,1,10,10,exam opened before fix was ready
```

- Override takes the **max** of existing and override score (won't lower a score)
- `reason` is stored in database for audit trail
- Can query overrides later: `SELECT * FROM exam_question_scores WHERE override_reason IS NOT NULL`

### External Dependencies

- **fzf**: Required for `gb netid` command (fuzzy search interface)
- **git**: Required for `gb report -p` (push reports to student repos)

## Version Management

The version is defined in `src/Gradebook/Version.hs`. **Increment the version when adding new features:**
- **Major** (x.0.0): Breaking changes to commands or config format
- **Minor** (0.x.0): New commands or features
- **Patch** (0.0.x): Bug fixes

After making changes, update the installed command with:
```bash
nix build && nix profile upgrade gradebook
```

## Important Notes

- **Student Data Privacy**: The roster.csv contains FERPA-protected student information. Never commit changes that expose this data outside the repository or in logs.
- **Haskell Style**: Follow standard Haskell conventions (hlint can help with style checking)
- **Functional Approach**: Prefer pure functions, immutability, and type-safe operations typical of Haskell development
- **Git Tracking for Nix**: New source files must be `git add`ed before `nix build` will see them (flakes only see tracked files)
