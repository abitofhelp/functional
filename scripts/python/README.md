<!-- SPDX-License-Identifier: BSD-3-Clause -->

# Project Scripts

**Organized automation scripts for development, testing, and project branding**

---

## Directory Structure

```
scripts/python/
├── arch_guard/        # Architecture validation tool
├── brand_project/     # Project template branding tool
├── makefile/          # Makefile helper scripts
├── release/           # Release management tool
└── common.py          # Shared utilities
```

---

## Architecture Guard (`scripts/python/arch_guard/`)

Validates hexagonal architecture boundaries in Go projects.

### `arch_guard.py`

**Purpose:** Validate hexagonal architecture boundaries

**What it does:**
- Enforces layer dependency rules (Domain -> Application -> Infrastructure -> API/outer layers)
- Detects illegal imports that violate architecture boundaries
- Validates that Domain layer has zero external dependencies
- Parses go.mod files (including multi-line require blocks)

**Usage:**
```bash
# Via Makefile (recommended)
make check-arch

# Direct execution
python3 scripts/python/arch_guard/arch_guard.py
```

**Makefile Target:** `check-arch`

**Exit Codes:**
- `0` - Architecture is clean (or warnings only)
- `1` - Critical architecture violations found

---

## Project Branding (`scripts/python/brand_project/`)

A unified tool to instantiate new projects from hybrid_app/hybrid_lib templates.

### Features

- **Multi-language support:** Go, Ada (Rust planned)
- **Automatic language detection:** Detects from go.mod, alire.toml, etc.
- **Smart text replacement:** Handles snake_case, PascalCase, UPPER_CASE, Ada_Pascal_Case
- **Module path updates:** Updates go.mod import paths automatically
- **Dry-run mode:** Preview changes before applying

### Usage

```bash
# From project root - dry run (preview changes)
python3 scripts/python/brand_project/brand_project.py --git-repo github.com/account/my_app --dry-run

# From project root - create in current directory
python3 scripts/python/brand_project/brand_project.py --git-repo github.com/account/my_app

# Specify output directory
python3 scripts/python/brand_project/brand_project.py --git-repo github.com/account/my_app -o ~/projects

# As a module
python3 -m scripts.brand_project --git-repo github.com/account/my_app --dry-run

# With explicit source and output directories
python3 scripts/python/brand_project/brand_project.py --git-repo github.com/account/my_app --source ../hybrid_lib_go -o /tmp

# Verbose output
python3 scripts/python/brand_project/brand_project.py --git-repo github.com/account/my_app -v
```

### Options

| Option | Description |
|--------|-------------|
| `--git-repo` | Git repository URL for new project (required) |
| `--source` | Source template directory (default: current directory) |
| `--output, -o` | Output directory where project is created (default: current directory) |
| `--dry-run` | Show what would be done without making changes |
| `--verbose, -v` | Show detailed progress |

### Supported Git URL Formats

```
github.com/account/my_app
github.com/account/my_app.git
https://github.com/account/my_app
https://github.com/account/my_app.git
git@github.com:account/my_app.git
```

### What It Does

1. **Copies template files** - Excludes .git, vendor, bin, build artifacts
2. **Renames files** - Files containing the old project name
3. **Replaces text** - Project name references in file contents
4. **Updates config files** - go.mod module paths, go.work paths
5. **Verifies** - No old references remain

### Module Structure

```
brand_project/
├── __init__.py        # Package exports
├── __main__.py        # Module entry point
├── brand_project.py   # Main engine and CLI
├── models.py          # Data models (GitRepoUrl, ProjectConfig)
└── adapters/
    ├── __init__.py    # Adapter exports
    ├── base.py        # Abstract base adapter
    ├── go.py          # Go language adapter
    └── ada.py         # Ada language adapter
```

---

## Shared Utilities (`scripts/python/common.py`)

**Purpose:** Shared utilities and helper functions

**Features:**
- Terminal color output (ANSI codes)
- OS detection (macOS, Linux, Windows)
- Command existence checking
- Package manager detection
- Print functions (success, error, warning, info, section)

**Usage:** Import by other scripts
```python
from common import print_success, print_error, print_info, print_warning, print_section
```

---

## Integration with Makefile

Scripts are invoked through clean Makefile targets:

```makefile
# Architecture validation
check-arch:
	@python3 scripts/python/arch_guard/arch_guard.py
```

---

## Development Guidelines

### Adding New Scripts

When creating new automation scripts:

1. **Choose the right location:**
   - `scripts/python/arch_guard/` - If related to architecture validation
   - `scripts/python/brand_project/` - If part of branding workflow
   - `scripts/python/` root - If general-purpose utility

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

---

**Last Updated:** November 25, 2025
**Python Version:** 3.7+
**License:** BSD-3-Clause
