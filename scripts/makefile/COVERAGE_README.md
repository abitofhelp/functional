# GNATcoverage Quick Start

**Version:** 1.0.0
**Date:** November 18, 2025
**Copyright:** © 2025 Michael Gardner, A Bit of Help, Inc.
**SPDX-License-Identifier:** BSD-3-Clause

## Overview

This project uses GNATcoverage for source-trace coverage analysis. The runtime library is built automatically when needed.

## Prerequisites

- **gnatcov** installed via Alire (automatically managed)
- **gprbuild** available
- Unit and integration tests built

## Quick Start

### First Time Setup

When you clone this project, run:

```bash
make build-coverage-runtime
```

This builds the GNATcoverage runtime library and installs it to `external/gnatcov_rts/install/`.

**Note:** The `external/` directory is git-ignored since it's generated from the gnatcov crate sources.

### Running Coverage Analysis

```bash
make test-coverage
```

This will:
1. **Auto-build runtime** if not present (via `build_gnatcov_runtime.py`)
2. Clean previous build artifacts
3. Build your project
4. Instrument unit and integration tests
5. Build instrumented tests with coverage runtime
6. Run all tests and collect trace files
7. Generate coverage reports

### Output

Coverage reports are generated in:
- **Text summary:** `coverage/summary.txt`
- **HTML report:** `coverage/report/index.html`
- **DHTML interactive:** `coverage/report/dhtml/` (if using run_gnatcov.sh)

## Manual Runtime Build

If you need to rebuild the runtime:

```bash
# Via Makefile
make build-coverage-runtime

# Or directly
python3 scripts/makefile/build_gnatcov_runtime.py
```

## How It Works

### 1. Runtime Sources

The gnatcov crate includes runtime sources at:
```
alire/cache/dependencies/gnatcov_*/share/gnatcoverage/gnatcov_rts/
```

### 2. Build Process

The `build_gnatcov_runtime.py` script:
1. Locates gnatcov installation via `alr printenv`
2. Copies runtime sources to `external/gnatcov_rts/build/`
3. Builds static library using `gprbuild`
4. Installs to `external/gnatcov_rts/install/`:
   - `lib/` - Static library (libgnatcov_rts_full.a)
   - `include/` - Ada specification files (.ads)
   - `share/gpr/` - GPR project files

### 3. Coverage Workflow

The `coverage.sh` script:
1. Checks for runtime, builds if missing
2. Instruments tests with `gnatcov instrument`
3. Builds with `--implicit-with=gnatcov_rts_full.gpr`
4. Runs tests to generate `.srctrace` files
5. Analyzes traces with `gnatcov coverage`

## Architecture

```
simple_hybrid/
├── scripts/makefile/
│   ├── build_gnatcov_runtime.py  # Builds runtime from gnatcov sources
│   ├── coverage.sh               # Main coverage workflow
│   └── run_gnatcov.sh            # Alternative coverage runner
├── external/                     # Git-ignored, generated
│   └── gnatcov_rts/
│       ├── build/                # Build directory
│       └── install/              # Installed runtime
│           ├── lib/              # libgnatcov_rts_full.a
│           ├── include/          # Ada specs
│           └── share/gpr/        # GPR projects
└── coverage/                     # Coverage reports (git-ignored)
    ├── traces/                   # .srctrace files
    └── report/                   # HTML/text reports
```

## Troubleshooting

### Runtime not found

```bash
ERROR: Cannot find gnatcov_rts runtime under external/gnatcov_rts/install
```

**Solution:** Run `make build-coverage-runtime`

### gnatcov not installed

```bash
ERROR: gnatcov not found in project dependencies
```

**Solution:** The project should already have gnatcov as a dependency. If not:
```bash
alr with gnatcov
```

### Build failures

If the runtime build fails, try:

```bash
# Clean and rebuild
rm -rf external/gnatcov_rts/
make build-coverage-runtime
```

## For New Project Clones

When someone clones this repository:

1. **Clone project:**
   ```bash
   git clone <repo-url>
   cd simple_hybrid
   ```

2. **Let Alire install dependencies:**
   ```bash
   alr build
   ```

3. **Run coverage** (runtime builds automatically):
   ```bash
   make test-coverage
   ```

The runtime is built on-demand, so new clones "just work"!

## Development Notes

- The runtime is **project-local** (not global) - each project has its own copy
- Runtime is **deterministic** - built from gnatcov crate sources
- **No manual downloads** required - everything is managed by Alire
- **Cross-platform** - works on macOS, Linux, Windows (wherever gnatcov runs)

## See Also

- `build_gnatcov_runtime.py` - Runtime build script
- `coverage.sh` - Main coverage workflow
- `run_gnatcov.sh` - Alternative coverage runner
- Makefile targets: `build-coverage-runtime`, `test-coverage`
