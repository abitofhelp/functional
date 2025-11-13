# Project Scripts

**Version:** 2.0.0
**Date:** November 13, 2025
**SPDX-License-Identifier:** BSD-3-Clause
**License File:** See the LICENSE file in the project root.
**Copyright:** © 2025 Michael Gardner, A Bit of Help, Inc.
**Status:** Released

**Python automation scripts for development tasks**

---

## Overview

This directory contains Python scripts that automate complex development tasks. Using Python instead of complex shell scripts in the Makefile provides:

- **Better readability** - Easier to understand for educational purposes
- **Cross-platform compatibility** - Works on macOS, Linux, and Windows
- **Error handling** - Proper exception handling and user feedback
- **Testability** - Scripts can be unit tested
- **Maintainability** - Easier to modify and extend

---

## Available Scripts

### `common.py`

**Purpose:** Shared utilities and helper functions for all scripts

**Features:**
- Terminal color output (ANSI codes)
- OS detection (macOS, Linux, Windows)
- Command existence checking
- Package manager detection
- Common print functions (success, error, warning, info)

**Usage:** Import by other scripts
```python
from common import print_success, command_exists, is_macos
```

---

### `install_tools.py`

**Purpose:** Install all required development dependencies

**What it installs:**
- **GMP library** - Required math library for GNAT
  - macOS: via Homebrew (`brew install gmp`)
  - Linux: via apt/yum (`libgmp-dev` or `gmp-devel`)
- **gcovr** - Coverage report generator (via pip3)
- **gnatformat** - Ada code formatter (via Alire)

**Usage:**
```bash
# Via Makefile
make install-tools

# Direct execution
python3 scripts/install_tools.py
./scripts/install_tools.py
```

**Features:**
- Automatically detects OS and package manager
- Checks if tools are already installed (no unnecessary reinstalls)
- Provides helpful error messages
- Verifies installations after completion

**Exit Codes:**
- `0` - All critical tools installed successfully
- `1` - One or more critical installations failed

---

### `run_coverage.py`

**Purpose:** Run test suite with coverage analysis and generate HTML reports

**What it does:**
1. Cleans previous coverage artifacts (`.gcda`, `.gcno`, `.gcov`)
2. Builds test suite with coverage instrumentation using `tests-coverage.gpr`
3. Runs the test suite (`tests/bin/test_runner`)
4. Generates HTML coverage report using `gcovr`
5. Provides instructions for viewing the report

**Usage:**
```bash
# Via Makefile
make test-coverage

# Direct execution
python3 scripts/run_coverage.py
./scripts/run_coverage.py
```

**Output:**
- `coverage/coverage.html` - Main coverage report (click to view)
- `coverage/*.html` - Detailed per-file coverage reports
- Terminal summary of coverage statistics

**Requirements:**
- Python 3
- gcovr (`pip3 install gcovr`)
- GNAT toolchain with gcov support

**Fallback:** If gcovr is not installed, basic `.gcov` files are still generated

---

### `sync_versions.py`

**Purpose:** Synchronize version numbers across all alire.toml files

**What it does:**
1. Reads the version from root `alire.toml` (single source of truth)
2. Updates version in all layer `alire.toml` files to match:
   - application/alire.toml
   - bootstrap/alire.toml
   - domain/alire.toml
   - infrastructure/alire.toml
   - presentation/alire.toml
   - shared/alire.toml

**Usage:**
```bash
# Via Makefile
make sync-versions

# Direct execution
python3 scripts/sync_versions.py
python3 scripts/sync_versions.py --dry-run
python3 scripts/sync_versions.py --verbose
```

**Options:**
- `--dry-run` - Show what would be changed without modifying files
- `--verbose` - Show detailed information about each file processed

**Use Case:** Ensures all crates in the hybrid architecture have synchronized version numbers

---

### `generate_version.py`

**Purpose:** Generate Ada version package from alire.toml

**What it does:**
1. Extracts version from `alire.toml`
2. Parses semantic version (MAJOR.MINOR.PATCH-PRERELEASE+BUILD)
3. Generates `shared/src/hybrid-version.ads` with version constants
4. Provides version checking functions (Is_Prerelease, Is_Development, Is_Stable)

**Usage:**
```bash
# Via Makefile
make generate-version

# Direct execution
python3 scripts/generate_version.py alire.toml shared/src/hybrid-version.ads
```

**Generated Package:**
```ada
package Hybrid.Version is
   Major : constant Natural := 0;
   Minor : constant Natural := 1;
   Patch : constant Natural := 0;
   Version : constant String := "0.1.0-dev";
   function Is_Prerelease return Boolean;
   function Is_Development return Boolean;
   function Is_Stable return Boolean;
end Hybrid.Version;
```

**Use Case:** Single source of truth for version info accessible from Ada code

---

### `add_md_headers.py`

**Purpose:** Add or update standard metadata headers in markdown files

**What it does:**
1. Finds all `.md` files in project (docs/, root)
2. Adds standard header with Version, Date, Copyright, Status
3. Updates existing headers to current version/date
4. Preserves file content below header

**Usage:**
```bash
# Via Makefile
make add-md-headers

# Direct execution
python3 scripts/add_md_headers.py
```

**Header Format:**
```markdown
---
**Version:** 1.0.0  
**Date:** October 28, 2025  
**Copyright:** © 2025 Michael Gardner, A Bit of Help, Inc.  
**License:** BSD-3-Clause
**Status:** Released  
---
```

**Use Case:** Maintain consistent metadata across all documentation files

---

### `release.py`

**Purpose:** Complete release management and orchestration

**What it does:**

**Prepare Command:**
1. Ensures markdown files have standard headers
2. Updates root alire.toml version
3. Syncs all layer alire.toml files
4. Generates Hybrid.Version Ada package
5. Updates Ada source file copyright years
6. Updates markdown file versions and dates
7. Updates CHANGELOG.md
8. Generates PlantUML diagrams
9. Runs build verification
10. Runs test suite

**Release Command:**
1. Verifies clean git working tree
2. Creates annotated git tag
3. Pushes changes and tag to GitHub
4. Creates GitHub release with notes from CHANGELOG

**Diagrams Command:**
1. Generates all PlantUML diagrams to SVG format

**Usage:**
```bash
# Via Makefile
make release-prepare    # prompts for version
make release-create     # prompts for version

# Direct execution
python3 scripts/release.py prepare 1.0.0
python3 scripts/release.py release 1.0.0
python3 scripts/release.py diagrams
```

**Semantic Versioning Support:**
- Stable: `1.0.0`, `2.3.4`
- Pre-release: `1.0.0-dev`, `1.0.0-alpha.1`, `1.0.0-beta.2`, `1.0.0-rc.1`
- With build: `1.0.0+build.123`, `1.0.0-rc.1+commit.abc123`

**Use Case:** Automated, consistent release process with zero manual errors

---

### `validate_release.py`

**Purpose:** Comprehensive release validation scan (RELEASE_CHECKLIST Step 3)

**What it does:**

Automates all validation checks from the release checklist:
1. **File Headers** - Verifies all Ada source (.ads/.adb) and test files have copyright © 2025 and SPDX-License-Identifier
2. **Markdown Status** - Checks Status fields are "Released" (not "Pre-release")
3. **Build Warnings** - Runs `make build` and verifies zero warnings
4. **Test Suite** - Runs `make test` and verifies all 9 test suites pass
5. **TODOs/FIXMEs** - Searches source code for remaining TODO/FIXME comments
6. **Diagrams** - Verifies all .puml files have corresponding .svg exports
7. **Guides** - Checks all required architecture guides are present
8. **Temporary Files** - Finds .tmp, .bak, ~, .backup files that should be cleaned

**Usage:**
```bash
# Full validation (includes build and tests - ~2 minutes)
python3 scripts/validate_release.py

# Verbose output with detailed checks
python3 scripts/validate_release.py --verbose

# Quick validation (skip slow build/tests - ~10 seconds)
python3 scripts/validate_release.py --quick

# Verbose + quick
python3 scripts/validate_release.py --verbose --quick
```

**Exit Codes:**
- `0` - All validations passed (ready for release)
- `1` - One or more validations failed (needs fixing)
- `2` - Script error or missing dependencies

**Output:**
```
======================================================================
RELEASE VALIDATION SCAN - v1.0.0
======================================================================

🔍 Verifying Ada source file headers (.ads, .adb)...
✅ All 87 Ada source files have proper headers

🔍 Verifying test file headers...
✅ All 15 test files have proper headers

🔍 Verifying markdown Status fields...
✅ Markdown Status fields correct

🔍 Building project and checking for warnings...
✅ Build completed with ZERO warnings

🔍 Running complete test suite...
✅ All tests passed (9/9 suites)

🔍 Checking for TODO/FIXME comments...
✅ No TODOs or FIXMEs found

🔍 Verifying diagrams are up-to-date...
✅ All 12 diagrams have SVG exports

🔍 Verifying architecture guides...
✅ All 5 required guides present (15 total)

🔍 Checking for temporary files...
✅ No temporary files found

======================================================================
VALIDATION SUMMARY
======================================================================

PASS - Ada Source Headers
PASS - Test File Headers
PASS - Markdown Status
PASS - Build Warnings
PASS - Test Suite
PASS - TODOs/FIXMEs
PASS - Diagrams
PASS - Guides
PASS - Temporary Files

Results: 9/9 checks passed

✅ 🎉 ALL VALIDATIONS PASSED - READY FOR RELEASE!
```

**Integration:**
- Called automatically by `release.py prepare` command
- Can be run standalone during development
- Suitable for CI/CD pipeline integration
- Referenced in RELEASE_CHECKLIST.md Step 3

**Use Case:** Automated validation that ensures release quality and consistency

---

## Development Guidelines

### Adding New Scripts

When adding new automation scripts:

1. **Use Python 3** - Maximize portability and readability
2. **Import from common.py** - Reuse utilities for consistency
3. **Add docstrings** - Document purpose and usage
4. **Handle errors gracefully** - Provide helpful error messages
5. **Make executable** - `chmod +x scripts/your_script.py`
6. **Add shebang** - `#!/usr/bin/env python3`
7. **Document here** - Update this README

### Script Structure Template

```python
#!/usr/bin/env python3
"""
Brief description of what this script does.

Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
SPDX-License-Identifier: BSD-3-Clause
"""

import sys
from pathlib import Path

# Add scripts directory to path
sys.path.insert(0, str(Path(__file__).parent))

from common import print_success, print_error, print_info

def main() -> int:
    """Main entry point."""
    print_info("Starting task...")

    try:
        # Do work here
        print_success("Task complete!")
        return 0
    except Exception as e:
        print_error(f"Task failed: {e}")
        return 1

if __name__ == '__main__':
    sys.exit(main())
```

### Testing Scripts

Test scripts manually before committing:

```bash
# Test on clean system
python3 scripts/your_script.py

# Test with --help (if you add argparse)
python3 scripts/your_script.py --help

# Test error handling
# (Temporarily rename dependencies to trigger errors)
```

---

## Integration with Makefile

Scripts are invoked from Makefile targets:

```makefile
# Development tools
install-tools:
	@python3 scripts/install_tools.py

test-coverage:
	@python3 scripts/run_coverage.py

# Version management
sync-versions:
	@python3 scripts/sync_versions.py

generate-version:
	@python3 scripts/generate_version.py alire.toml shared/src/hybrid-version.ads

add-md-headers:
	@python3 scripts/add_md_headers.py

# Release management
release-prepare:
	@python3 scripts/release.py prepare $$version

release-create:
	@python3 scripts/release.py release $$version
```

This keeps the Makefile clean and delegates complex logic to testable Python code.

---

## Future Script Candidates

Complex Makefile targets that could be converted to scripts:

- **format.py** - Code formatting with gnatformat/gnatpp
- **check_links.py** - Documentation link validation
- **stats.py** - Project statistics (LOC, test count, etc.)
- **setup_hooks.py** - Git pre-commit hook installation
- **watch.py** - File watching and auto-rebuild

---

## Dependencies

### Required
- **Python 3.7+** - All scripts use modern Python features
- **pathlib** - File path operations (built-in)
- **subprocess** - Running external commands (built-in)
- **shutil** - File operations (built-in)

### Optional
- **gcovr** - Required for `run_coverage.py` HTML reports
- **pip3** - Required for installing Python packages

---

## Educational Value

These scripts demonstrate:

1. **Professional project organization** - Scripts separated from build logic
2. **Cross-platform development** - OS detection and adaptation
3. **Error handling** - Proper exception handling and user feedback
4. **Code reuse** - Shared utilities in `common.py`
5. **Documentation** - Clear docstrings and comments
6. **Python best practices** - Type hints, descriptive names, modular design

---

**Last Updated:** 2025-10-28
**Python Version:** 3.7+
**License:** BSD-3-Clause
