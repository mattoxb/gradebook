# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is a CS421 (Programming Languages and Compilers) gradebook management system implemented in Haskell. The project processes student roster data and manages grade information for the course.

## Data Files

The `data-files/` directory contains:
- `roster.csv`: Course roster with student information including Net ID, UIN, name, email, program, college, and advisor details
  - Contains FERPA-protected student data
  - CSV format with 22 columns including demographic and academic information
  - Used for CS 421 Section B3 (CRN 31375)

## Development Environment

This project uses **Nix flakes** for reproducible development environments.

### Setup
```bash
# Enter the Nix development shell (provides GHC 9.8.4, Stack, HLS, Cabal)
nix develop

# Or use direnv for automatic shell activation
direnv allow
```

### Build Commands (run inside `nix develop`)
- **Build**: `stack build`
- **Test**: `stack test`
- **Run**: `stack exec gb -- <command>`
- **REPL**: `stack ghci`
- **Clean**: `stack clean`

### Available Commands
- `gb load-roster [-r FILE]`: Load roster CSV into database (default: data-files/roster.csv)
- `gb netid`: Search for a student using fzf and output their netid (requires interactive terminal)

### Running Commands
**Important:** Interactive commands like `gb netid` require a proper terminal. Use one of these methods:

```bash
# Method 1: Enter the nix shell first (recommended for interactive commands)
nix develop
stack exec gb -- netid

# Method 2: Use a local installation
stack install  # Installs to ~/.local/bin
gb netid       # Run directly (make sure ~/.local/bin is in PATH)

# Method 3: For non-interactive commands (like load-roster)
nix develop --command stack exec gb -- load-roster
```

### Nix Configuration Details
- GHC version: 9.8.4 (configured in flake.nix)
- Stack is configured to use system GHC from Nix (no separate GHC download)
- Haskell Language Server (HLS) is available for IDE integration
- The `stack.yaml` should have `system-ghc: true` and `install-ghc: false`

## Code Architecture

### Module Structure

The project follows a modular architecture:

```
src/Gradebook/
├── Database.hs      -- Database schema and operations (HDBC)
├── Config.hs        -- Configuration file reading (YAML)
├── Roster.hs        -- CSV roster parsing (cassava)
├── Commands.hs      -- Command implementations
└── CLI.hs           -- CLI parser (optparse-applicative)

app/
└── Main.hs          -- Executable entry point
```

### Key Components

- **Database Layer** (Database.hs): Uses HDBC for database abstraction, supporting both SQLite3 and PostgreSQL. The `students` table stores all roster information except the "On Pending Degree List" field.

- **Configuration** (Config.hs): Reads `config.yaml` to determine database name and type. Currently configured for SQLite3 with database file `cs421-grades-sp26.db`.

- **CSV Parsing** (Roster.hs): Parses the 22-column university roster CSV format using cassava, mapping each row to a `Student` record.

- **CLI** (CLI.hs): Implements git-style subcommands using optparse-applicative:
  - `load-roster`: Imports roster CSV into the database
  - `netid`: Interactive student search using fzf (searches netid, name, email, UIN)

- **Commands** (Commands.hs): Contains the business logic for each CLI command. Opens database connections based on config and executes operations.

### Database Schema

The `students` table includes:
- Primary key: `netid`
- Student info: `uin`, `name`, `email`, `gender`, `admit_term`
- Academic info: `credit`, `level`, `year`, `subject`, `number`, `section`, `crn`
- Program info: `degree_name`, `major_1_name`, `college`, `program_code`, `program_name`
- Other: `ferpa`, `honors_credit`, `advisors`

### External Dependencies

- **fzf**: Required for the `gb netid` command (fuzzy search interface)

## Important Notes

- **Student Data Privacy**: The roster.csv contains FERPA-protected student information. Never commit changes that expose this data outside the repository or in logs.
- **Haskell Style**: Follow standard Haskell conventions (hlint can help with style checking)
- **Functional Approach**: Prefer pure functions, immutability, and type-safe operations typical of Haskell development
