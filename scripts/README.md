# Project Scripts

**Organized automation scripts for development, testing, and release management**

---

## Directory Structure

```
scripts/
├── makefile/          # Scripts invoked by Makefile targets
├── release/           # Release management and version control
└── [utilities]        # General-purpose helper scripts
```

---

## 📁 Makefile Scripts (`scripts/makefile/`)

Scripts directly invoked by Makefile targets for build, test, and quality workflows.

### `arch_guard.py`

**Purpose:** Validate hexagonal architecture boundaries

**What it does:**
- Enforces layer dependency rules (Domain → Application → Infrastructure → Presentation)
- Detects illegal imports that violate architecture boundaries
- Validates that Domain layer has zero external dependencies

**Usage:**
```bash
# Via Makefile (recommended)
make check-arch

# Direct execution
python3 scripts/makefile/arch_guard.py
```

**Makefile Target:** `check-arch`

**Exit Codes:**
- `0` - Architecture is clean (or warnings only)
- `1` - Critical architecture violations found

---

### `coverage.sh`

**Purpose:** Run GNATcoverage source-trace analysis workflow

**What it does:**
1. Locates GNATcoverage runtime (vendored in external/gnatcov_rts/)
2. Instruments unit and integration test projects
3. Builds instrumented tests with coverage runtime
4. Executes all test runners
5. Generates coverage reports (HTML, DHTML, text)

**Usage:**
```bash
# Via Makefile (recommended)
make test-coverage

# Direct execution
bash scripts/makefile/coverage.sh
```

**Makefile Target:** `test-coverage`

**Output:**
- `coverage/index.html` - Main coverage dashboard
- `coverage/dhtml/` - Interactive DHTML reports
- Terminal summary with percentage coverage

**Requirements:**
- GNATcoverage (via Alire: `alr exec -- gnatcov`)
- Vendored gnatcov_rts in external/

---

### `run_coverage.py`

**Purpose:** Alternative Python-based coverage runner

**What it does:**
- Simplified coverage workflow using gcov/gcovr
- Cleans previous coverage artifacts
- Builds tests with coverage instrumentation
- Generates HTML coverage reports

**Usage:**
```bash
python3 scripts/makefile/run_coverage.py
```

**Requirements:**
- gcovr (`pip3 install gcovr`)

---

### `run_gnatcov.sh`

**Purpose:** Complete GNATcoverage workflow automation

**What it does:**
1. Instruments test projects for source-trace coverage
2. Builds instrumented tests
3. Runs test suite
4. Generates coverage reports with line/branch info

**Usage:**
```bash
bash scripts/makefile/run_gnatcov.sh
```

---

### `install_tools.py`

**Purpose:** Install development dependencies and tools

**What it installs:**
- **GMP library** - Required for GNAT math operations
  - macOS: `brew install gmp`
  - Linux: `apt-get install libgmp-dev` or `yum install gmp-devel`
- **gcovr** - Coverage report generator (`pip3 install gcovr`)
- **gnatformat** - Ada code formatter (`alr get gnatformat`)

**Usage:**
```bash
# Via Makefile (recommended)
make install-tools

# Direct execution
python3 scripts/makefile/install_tools.py
```

**Makefile Target:** `install-tools`

**Features:**
- Auto-detects OS and package manager
- Checks existing installations before installing
- Verifies successful installation
- Provides helpful error messages

---

### `ada_formatter_pipeline.donotuse.py`

**Status:** ⚠️  DISABLED - Awaiting adafmt tool

**Purpose:** Ada source code formatting pipeline

**Why disabled:**
- Current implementation has issues with comment formatting
- Being replaced by custom adafmt tool (in development)
- Commented out in Makefile format targets

**Do not use this script** - It will be replaced when adafmt is ready.

---

## 📁 Release Scripts (`scripts/release/`)

Automated release management, version synchronization, and GitHub release creation.

### `release.py`

**Purpose:** Complete release orchestration and workflow automation

**What it does:**

**Prepare Command:** (`python scripts/release/release.py prepare 1.0.0`)
1. Cleans temporary files (.bak, .o, .ali, __pycache__)
2. Updates root alire.toml version
3. Syncs all layer alire.toml files to match
4. Generates Simple_Hybrid.Version Ada package
5. ~~Generates Ada docstrings~~ (disabled - tzif-specific)
6. Formats code (`make format`) if available
7. ~~Rebuilds formal documentation~~ (disabled - tzif-specific)
8. Updates markdown file versions and dates
9. Updates CHANGELOG.md (creates for v0.1.0, updates for later)
10. Generates PlantUML diagrams to SVG
11. Runs build verification
12. Runs test suite

**Release Command:** (`python scripts/release/release.py release 1.0.0`)
1. Verifies clean git working tree
2. Creates annotated git tag (v1.0.0)
3. Pushes changes and tag to origin
4. Creates GitHub release with CHANGELOG notes

**Diagrams Command:** (`python scripts/release/release.py diagrams`)
1. Generates all PlantUML diagrams to SVG format

**Usage:**
```bash
# Prepare release (updates versions, runs tests)
python3 scripts/release/release.py prepare 1.0.0

# Create release (tags and publishes to GitHub)
python3 scripts/release/release.py release 1.0.0

# Just regenerate diagrams
python3 scripts/release/release.py diagrams
```

**Semantic Versioning Support:**
- Stable: `1.0.0`, `2.3.4`
- Pre-release: `1.0.0-dev`, `1.0.0-alpha.1`, `1.0.0-rc.1`
- With build metadata: `1.0.0+build.123`

**Requirements:**
- Python 3.7+
- gh CLI (for GitHub releases)
- plantuml (for diagram generation)
- Clean git working tree (for release command)

---

### `sync_versions.py`

**Purpose:** Synchronize version numbers across all alire.toml files

**What it does:**
1. Reads version from root alire.toml (single source of truth)
2. Updates version in all layer alire.toml files:
   - application/alire.toml
   - bootstrap/alire.toml
   - domain/alire.toml
   - infrastructure/alire.toml
   - presentation/alire.toml
   - shared/alire.toml

**Usage:**
```bash
# Called by release.py, or standalone:
python3 scripts/release/sync_versions.py 1.0.0
```

**Use Case:** Ensures version consistency across hexagonal architecture layers

---

### `generate_version.py`

**Purpose:** Generate Ada version package from alire.toml

**What it does:**
1. Extracts version from alire.toml
2. Parses semantic version (MAJOR.MINOR.PATCH-PRERELEASE+BUILD)
3. Generates `shared/src/simple_hybrid-version.ads` with constants

**Usage:**
```bash
# Called by release.py, or standalone:
python3 scripts/release/generate_version.py alire.toml shared/src/simple_hybrid-version.ads
```

**Generated Package:**
```ada
package Simple_Hybrid.Version is
   Major : constant Natural := 1;
   Minor : constant Natural := 0;
   Patch : constant Natural := 0;
   Version : constant String := "1.0.0";
   -- ... version checking functions
end Simple_Hybrid.Version;
```

---

### `validate_release.py`

**Purpose:** Comprehensive pre-release validation checks

**What it does:**
1. Verifies Ada source file headers (copyright, SPDX)
2. Checks markdown Status fields (Released vs Pre-release)
3. Runs build and checks for warnings
4. Runs test suite and verifies all pass
5. Searches for TODO/FIXME comments
6. Verifies PlantUML diagrams have SVG exports
7. Checks required architecture guides exist
8. Finds temporary files that should be cleaned

**Usage:**
```bash
# Full validation (includes build/tests ~2 min)
python3 scripts/release/validate_release.py

# Quick validation (skip build/tests ~10 sec)
python3 scripts/release/validate_release.py --quick

# Verbose output
python3 scripts/release/validate_release.py --verbose
```

**Exit Codes:**
- `0` - All validations passed (ready for release)
- `1` - One or more validations failed
- `2` - Script error

**Use Case:** Pre-flight checks before creating a release

---

## 📄 General Utilities (Root `scripts/`)

### `common.py`

**Purpose:** Shared utilities and helper functions

**Features:**
- Terminal color output (ANSI codes)
- OS detection (macOS, Linux, Windows)
- Command existence checking
- Package manager detection
- Print functions (success, error, warning, info)

**Usage:** Import by other scripts
```python
from common import print_success, command_exists, is_macos
```

---

### `cleanup_temp_files.py`

**Purpose:** Remove temporary build artifacts and backup files

**What it removes:**
- `.bak`, `.backup` files
- `.o`, `.ali` object files
- `.DS_Store` (macOS)
- `__pycache__/` directories

**Usage:**
```bash
# Called by release.py, or standalone:
python3 scripts/cleanup_temp_files.py
```

---

### `cleanup_markdown_docs.py`

**Purpose:** Clean and normalize markdown documentation

**What it does:**
- Removes trailing whitespace
- Normalizes line endings
- Fixes markdown formatting issues

**Usage:**
```bash
python3 scripts/cleanup_markdown_docs.py
```

---

### `add_md_headers.py`

**Purpose:** Add/update standard metadata headers in markdown files

**What it does:**
1. Finds all `.md` files in project
2. Adds standard header with Version, Date, Copyright, Status
3. Updates existing headers to current version/date

**Header Format:**
```markdown
**Version:** 1.0.0
**Date:** November 18, 2025
**Copyright:** © 2025 Michael Gardner, A Bit of Help, Inc.
**SPDX-License-Identifier:** BSD-3-Clause
**Status:** Released
```

**Usage:**
```bash
python3 scripts/add_md_headers.py
```

---

### `fix_markdown_headers.py`

**Purpose:** Fix malformed markdown headers

**Usage:**
```bash
python3 scripts/fix_markdown_headers.py
```

---

### `curate_guides.py`

**Purpose:** Organize and maintain architecture guide documentation

**Usage:**
```bash
python3 scripts/curate_guides.py
```

---

### `fix_warnings.py`

**Purpose:** Automated fixes for common Ada compiler warnings

**Usage:**
```bash
python3 scripts/fix_warnings.py
```

---

### `rename_packages.py`

**Purpose:** Bulk rename Ada packages (for refactoring)

**Usage:**
```bash
python3 scripts/rename_packages.py
```

---

## Integration with Makefile

Scripts are organized by purpose and invoked through clean Makefile targets:

```makefile
# Architecture validation
check-arch:
	@python3 scripts/makefile/arch_guard.py

# Coverage analysis
test-coverage:
	@bash scripts/makefile/coverage.sh

# Tool installation
install-tools:
	@python3 scripts/makefile/install_tools.py

# Release preparation
prepare-release:
	@python3 scripts/release/release.py prepare $(version)
```

This organization provides:
- ✅ **Clear separation of concerns** - Makefile scripts vs release scripts
- ✅ **Easy to navigate** - Find scripts by their purpose
- ✅ **Maintainable** - Related scripts grouped together
- ✅ **Professional** - Industry-standard structure

---

## Development Guidelines

### Adding New Scripts

When creating new automation scripts:

1. **Choose the right location:**
   - `scripts/makefile/` - If invoked by a Makefile target
   - `scripts/release/` - If part of release workflow
   - `scripts/` root - If general-purpose utility

2. **Use Python 3** - Maximize portability and readability
3. **Import from common.py** - Reuse utilities
4. **Add docstrings** - Document purpose and usage
5. **Handle errors gracefully** - Helpful error messages
6. **Make executable** - `chmod +x scripts/your_script.py`
7. **Add shebang** - `#!/usr/bin/env python3`
8. **Update this README** - Document your script

### Script Template

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
sys.path.insert(0, str(Path(__file__).parent.parent))

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

---

## Dependencies

### Required
- **Python 3.7+** - All scripts require modern Python
- **pathlib** - File operations (built-in)
- **subprocess** - External commands (built-in)

### Optional
- **gcovr** - HTML coverage reports (`pip3 install gcovr`)
- **plantuml** - Diagram generation (`brew install plantuml`)
- **gh** - GitHub CLI (`brew install gh`)

---

## Educational Value

This script organization demonstrates:

1. **Professional project structure** - Purpose-driven organization
2. **Separation of concerns** - Build vs release vs utilities
3. **Maintainability** - Easy to find and modify scripts
4. **Best practices** - Clear naming, documentation, error handling
5. **Cross-platform support** - OS detection and adaptation

---

**Last Updated:** November 18, 2025
**Python Version:** 3.7+
**License:** BSD-3-Clause
