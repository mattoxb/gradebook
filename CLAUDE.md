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
- **System-wide installation (first time):** `nix profile install gradebook`  Note: this will not replace an existing installation. Use update instead.
- **System-wide update:** `nix profile upgrade gradebook`

### Available Commands
- `gb load-roster [-r FILE]`: Load roster CSV into database. Prints an add/drop report (netid, name, email) of students *added*, *re-added* (previously dropped, now back), and *dropped* (in DB but absent from the CSV). Dropped students are marked `students.enrolled = FALSE` but **kept** in the DB along with their scores; anyone present in the CSV is set `enrolled = TRUE`. Idempotent: re-loading the same CSV reports 0/0/0. Useful during the first ~10 days of add/drop churn.
- `gb load-categories [-c FILE]`: Load grade categories
- `gb load-assignments [-a FILE]`: Load assignments
- `gb load-scores FILE`: Load student scores from CSV
- `gb load-penalties [-p FILE]`: Load letter-grade-reduction penalties CSV (`netid,steps,reason`; default `data-files/penalties.csv`) into the `penalties` table. `steps` is how many notches to drop the student's *computed* letter grade down the `grade-thresholds` list (1 step = A → A-). Applied at `final-grades` time, never stored as a computed letter. Idempotent and CSV-authoritative: re-loading converges to the file (penalties for students no longer listed are deleted); unknown netids are warned and skipped.
- `gb gen-exam-zones -e SLUG INFO_JSON [-o FILE] [--force]`: Generate `data-files/<slug>-zones.csv` from a PrairieLearn `infoAssessment.json` (refuses to overwrite without `--force`)
- `gb load-exam-zones -e SLUG FILE`: Load zone/question structure from a zones CSV (run before `load-exam`)
- `gb load-exam -e SLUG FILE`: Load exam scores from PrairieLearn CSV — hard-fails if a question_id is missing from `exam_questions`
- `gb load-exam-overrides -e SLUG FILE`: Apply per-question score overrides keyed on `question_id`; hard-fails if target row missing
- `gb report [-n NETID] [-p] [-a]`: Generate grade report
- `gb final-grades [-o FILE]`: Write registrar upload spreadsheet (.xlsx) — needs `term-code` and `grade-thresholds` in config
- `gb collect SLUG...`: Mark assignments as collected
- `gb info [-n NETID]`: Print one student's roster details (net id, name, UIN, email, gender, section, CRN, credit, major, program, college, advisors). Selects with fzf when `-n` is omitted. Read-only; intended for laptop lookups against the source-of-truth DB.
- `gb repo [-n NETID]`: Clone (if needed) or `git pull` a student's repository into `repos/<netid>`, using `repo-prefix` from config. Unlike `report --push`, it never writes/commits/pushes — just makes a fresh local checkout available. Selects with fzf when `-n` is omitted.
- `gb netid [--email] [-m|--multi]`: Interactive student search using fzf. `--email` prints the email column instead of the netid; `--multi` enables fzf multi-select (Tab to mark) and prints one identifier per selected line.
- `gb version`: Show version information

### Load Command Invariants

All `gb load-*` commands MUST be idempotent: re-running a load with the same input is a no-op, and re-running with updated input converges to the new state without leaving stale rows behind. This was learned the hard way after a non-idempotent retake-loading bug silently lowered exam totals across the cohort.

Loading order matters: `load-exam-zones` (populates `exam_questions`) → `load-exam` (primary) → `load-exam` (retake) → `load-exam-overrides`. `load-exam` hard-fails if it sees a question_id not in `exam_questions`. Overrides go last because they edit per-question rows in-place and don't re-derive themselves from CSV on a subsequent load; running an earlier load step after overrides could wipe those edits.

The zones CSV is hand-editable and authoritative — not the JSON. PrairieLearn's `infoAssessment.json` describes the *live* exam, but the gradebook needs to track historical question_ids too (e.g. a question removed mid-exam still has DB rows). `gb gen-exam-zones` seeds the CSV from JSON but refuses to overwrite; add removed-mid-exam alternatives by hand with a `comment`.

### Nix Configuration Details
- GHC version: 9.8.4 (configured in flake.nix via haskell.nix)
- Haskell Language Server (HLS) is available in dev shell
- PostgreSQL client available for database access

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
├── Penalties.hs     -- CSV penalties parsing + applyGradeReduction (letter-grade notch-down)
├── ExamScores.hs    -- PrairieLearn exam CSV parsing (uses exam_questions for question_number lookup)
├── ExamOverrides.hs -- Exam score override parsing (CSV keyed on question_id)
├── ExamZonesCSV.hs  -- Read/write data-files/<slug>-zones.csv
├── InfoAssessment.hs -- PrairieLearn infoAssessment.json parser (used by gen-exam-zones)
├── Version.hs       -- Version information
├── GradeCalculation.hs -- Grade computation logic
├── Reports.hs       -- Report formatting and generation
├── Commands.hs      -- Command implementations
└── CLI.hs           -- CLI parser (optparse-applicative)

app/
└── Main.hs          -- Executable entry point
```

### Key Components

- **Database Layer** (Database.hs): Uses HDBC over **PostgreSQL** (SQLite support was removed in v0.13.0). Tables: `students`, `categories`, `assignments`, `scores`, `penalties`, `exam_zones`, `exam_questions`, `exam_question_scores`. `initDatabase` is the poor-man's migration tool: every table is `CREATE TABLE IF NOT EXISTS` and column additions are done with idempotent `ALTER TABLE ... ADD COLUMN IF NOT EXISTS` (e.g. `students.enrolled`), so it's safe to re-run against an existing course DB. There is no ORM / no auto-migration framework.

- **Configuration** (Config.hs): Reads `config.yaml` for database settings, grading configuration (weighted/pass-fail/letter-grade modes), category weights, and exam configurations.

- **Grade Calculation** (GradeCalculation.hs): Computes category grades with drop-lowest logic, evaluates pass/fail requirements, calculates letter grades from thresholds, and combines exam scores with retake policies.

- **Exam Support** (ExamScores.hs, Commands.hs): Parses PrairieLearn `instance_questions` CSV exports, stores individual question scores per zone, supports retake policies (max, max-if-better-avg-if-worse).

- **Exam totals are computed at report time, not stored.** `exam_question_scores` is the single source of truth for exam data. `gb report` (via `buildExamGradeForStudent`) reads those per-question rows and computes per-zone-then-per-exam averages on the fly; the exams contribution to the total is synthesized from that live computation. There are NO `scores.exam-*` rows. Do not add writeback of computed exam totals to the `scores` table — that path was removed in v0.11.0 after repeated bugs caused stale rows to silently lower student totals.

- **Multi-credit / extra-credit assignments.** In pass-fail courses, a single assignment row with a score > 1 contributes its full score (floor) to the requirement counter and the per-category "N/M completed" summary, instead of always counting as 1. Used in CS 491 to give 5 solves of extra credit for IPL via one `ipl` assignment row with score=5 in `score-files/ipl.csv`. Reports annotate such rows with `(×N)` after the checkmark. No schema change is needed; just put the multi-credit count in the score column.

- **Reports** (Reports.hs): Generates formatted grade reports including exam zone breakdowns.

### Database Schema

**Core Tables:**
- `students`: Student roster (netid PK, uin, name, email, etc.). `enrolled BOOLEAN NOT NULL DEFAULT TRUE` tracks add/drop status — set FALSE by `load-roster` when a student disappears from the roster CSV. Dropped students (and their scores) are retained, not deleted. Note: no other command filters on `enrolled` yet; it's currently informational + drives the `load-roster` add/drop report.
- `categories`: Grade categories (slug PK, title)
- `assignments`: Assignments (slug PK, order_num, category FK, max_points, title, collected)
- `scores`: Student scores (netid FK, assignment FK, score, excused)
- `penalties`: Letter-grade-reduction penalties (netid PK, steps, reason; FK to students ON DELETE CASCADE). One row per student. `steps` notches drop the computed letter grade at `final-grades` time; if a penalty pushes a grade to F, the registrar row's last-attended-date column is re-derived. Loaded by `load-penalties`; the CSV is authoritative.

**Exam Tables:**
- `exam_zones`: Exam structure (exam_slug FK, zone_number, zone_title, question_count)
- `exam_questions`: Question slot mapping (exam_slug, zone_number, question_number, question_id). One row per PrairieLearn alternative; multiple alternatives for the same slot share a `question_number`. Loaded from `data-files/<slug>-zones.csv` by `load-exam-zones`.
- `exam_question_scores`: Individual question scores (netid FK, exam_slug, zone_number, question_number, question_id, score, max_points, override_reason). `question_number` is derived from `exam_questions` at load time — *not* by alphabetical sort of question_id (that bug silently misaligned overrides for months).

### Configuration Example

```yaml
database: cs421-grades-sp26   # PostgreSQL database name (passed as dbname=...)
db-type: postgresql           # optional; only 'postgresql' is supported (SQLite removed in v0.13.0)
repo-prefix: "git@github.com:org/prefix_"   # netid is appended directly; SSH (host:path) or HTTPS (host/path) both work — SSH avoids credential prompts on a headless server
term-code: "120261"  # required by `gb final-grades`; UIUC Banner term code

grading:
  mode: weighted  # or pass-fail, letter-grade
  show-letter-grade: false  # default false: `gb report` shows the numeric total but NOT the letter grade (which reads "F" for everyone before grades exist). Flip to true after the last midterm. Does NOT affect `gb final-grades` (registrar xlsx always emits the letter).
  grade-thresholds:  # required by `gb final-grades`; also drives the letter grade line in reports (only shown when show-letter-grade is true)
    - {grade: "A+", min-percent: 97}
    - {grade: "A",  min-percent: 93}
    # ... through F
    - {grade: "F",  min-percent: 0}
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

1. (First time per exam, or after a JSON change) generate the zones CSV: `gb gen-exam-zones -e exam-1 path/to/midterm1/infoAssessment.json`. Hand-edit `data-files/exam-1-zones.csv` to add any alternatives that exist in DB but not in the live JSON (e.g., questions removed mid-exam), giving them a `comment`.
2. Load the zone/question structure: `gb load-exam-zones -e exam-1 data-files/exam-1-zones.csv`. Must precede `load-exam`.
3. Load the primary exam: `gb load-exam -e exam-1 midterm1_instance_questions.csv`. Hard-fails if the CSV references an unknown question_id (remediation: add it to the zones CSV and re-run `load-exam-zones`).
4. Load retake (if any): `gb load-exam -e exam-1-retake midterm1_retake_instance_questions.csv`. Automatically detects retake from config.
5. Apply overrides (if needed): `gb load-exam-overrides -e exam-1 overrides.csv`. Takes max of existing and override; hard-fails if the target (netid, exam, question_id) row doesn't exist.
6. Generate report: `gb report -n netid` shows zone + per-question breakdown.

### Exam Override CSV Format

For correcting bad questions or giving makeup credit:

```csv
netid,question_id,score,max_points,reason
student1,lambda-calculus/evaluation/midterm1,10,10,bad lambda calculus variant
student2,short-answer/cps-vs-tail-exam,10,10,LLM-graded retake
```

- Keyed on `question_id` (the PrairieLearn slug) — stable across JSON reorderings; alphabetical-vs-JSON divergence used to silently misapply overrides.
- Override takes the **max** of existing and override score (won't lower a score).
- `reason` is stored in `exam_question_scores.override_reason` for audit trail.
- Can query overrides later: `SELECT * FROM exam_question_scores WHERE override_reason IS NOT NULL`.

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

## TODOs

- **Report markdown rendering (do before Summer 2026 course starts):** `gb report` output is technically markdown but uses space-padded columns for alignment, which collapses ugly in GitHub/Obsidian/etc. Switch the table-ish sections to real GFM tables. Covers both `Reports/CS421.hs` and `Reports/CS491.hs`; exam zone breakdown (nested Q rows, dynamic column widths) is the trickiest case.

## Important Notes

- **Student Data Privacy**: The roster.csv contains FERPA-protected student information. Never commit changes that expose this data outside the repository or in logs.
- **Haskell Style**: Follow standard Haskell conventions (hlint can help with style checking)
- **Functional Approach**: Prefer pure functions, immutability, and type-safe operations typical of Haskell development
- **Git Tracking for Nix**: New source files must be `git add`ed before `nix build` will see them (flakes only see tracked files)
