#!/usr/bin/env python3
# ==============================================================================
# build_gnatcov_runtime.py - Build GNATcoverage runtime library
# ==============================================================================
# Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
# SPDX-License-Identifier: BSD-3-Clause
# See LICENSE file in the project root.
#
# Purpose:
#   Builds the GNATcoverage runtime library (gnatcov_rts) from sources
#   included with the gnatcov crate. This runtime is required for
#   source-trace coverage analysis.
#
# Usage:
#   python3 scripts/makefile/build_gnatcov_runtime.py
#   make build-coverage-runtime
#
# Output:
#   external/gnatcov_rts/install/  - Installed runtime (lib, include, share)
#
# Requirements:
#   - gnatcov crate installed via Alire (alr with gnatcov)
#   - gprbuild
#
# Design Notes:
#   - Locates runtime sources from gnatcov installation
#   - Builds static library suitable for linking with instrumented tests
#   - Installs to project-local external/ directory (not global)
#   - Safe to run multiple times (cleans build artifacts first)
#
# See Also:
#   coverage.sh - Uses the installed runtime for coverage analysis
#   COVERAGE_README.md - Coverage workflow documentation
# ==============================================================================

import sys
import os
import shutil
import subprocess
from pathlib import Path

# Add scripts directory to path for imports
sys.path.insert(0, str(Path(__file__).parent.parent))

from common import (
    print_success,
    print_error,
    print_info,
    print_warning,
    print_section,
    run_command,
)


def find_project_root() -> Path:
    """Find the project root directory."""
    # Try git root first
    result = subprocess.run(
        ['git', 'rev-parse', '--show-toplevel'],
        capture_output=True,
        text=True,
        check=False
    )
    if result.returncode == 0:
        return Path(result.stdout.strip())

    # Fall back to current directory
    return Path.cwd()


def find_gnatcov_prefix() -> str:
    """Find gnatcov installation prefix from Alire environment."""
    result = subprocess.run(
        ['alr', 'printenv'],
        capture_output=True,
        text=True,
        check=False
    )

    if result.returncode != 0:
        return None

    for line in result.stdout.splitlines():
        if 'GNATCOV_ALIRE_PREFIX' in line:
            # Format: export GNATCOV_ALIRE_PREFIX="/path/to/gnatcov"
            parts = line.split('=', 1)
            if len(parts) == 2:
                return parts[1].strip().strip('"')

    return None


def check_gnatcov_installed() -> bool:
    """Check if gnatcov is installed via Alire."""
    result = subprocess.run(
        ['alr', 'show', '--solve', 'gnatcov'],
        capture_output=True,
        text=True,
        check=False
    )
    return result.returncode == 0


def build_runtime(root: Path) -> bool:
    """Build and install the gnatcov runtime library."""

    print_section("GNATcov Runtime Builder")
    print()

    # 1. Check that gnatcov is installed
    print_info("Checking for gnatcov installation...")
    if not check_gnatcov_installed():
        print_error("gnatcov not found in project dependencies")
        print("Add it with: alr with gnatcov")
        return False

    # 2. Find gnatcov installation path
    gnatcov_prefix = find_gnatcov_prefix()
    if not gnatcov_prefix:
        print_error("Could not determine gnatcov installation path")
        return False

    print_success(f"Found gnatcov at: {gnatcov_prefix}")

    # 3. Locate runtime sources
    rts_src = Path(gnatcov_prefix) / "share" / "gnatcoverage" / "gnatcov_rts"
    if not rts_src.exists():
        print_error(f"Runtime sources not found at: {rts_src}")
        return False

    print_success("Found runtime sources")

    # 4. Setup build directories
    build_dir = root / "external" / "gnatcov_rts" / "build"
    install_prefix = root / "external" / "gnatcov_rts" / "install"

    print()
    print_info("Preparing build directories...")

    # Clean existing directories
    if build_dir.exists():
        shutil.rmtree(build_dir)
    if install_prefix.exists():
        shutil.rmtree(install_prefix)

    build_dir.mkdir(parents=True)
    install_prefix.mkdir(parents=True)

    # 5. Copy runtime sources to build directory
    print_info("Copying runtime sources...")
    for item in rts_src.iterdir():
        if item.is_file():
            shutil.copy2(item, build_dir)

    # 6. Build the runtime library
    print()
    print_info("Building gnatcov_rts library...")

    # Change to build directory
    original_dir = Path.cwd()
    try:
        os.chdir(build_dir)

        # run_command() raises exception on failure when check=True
        run_command(
            ['alr', 'exec', '--', 'gprbuild', '-p', '-P', 'gnatcov_rts_full.gpr',
             '-XLIBRARY_TYPE=static']
        )

        print_success("Runtime library built")

    except Exception as e:
        print_error(f"Failed to build runtime library: {e}")
        return False
    finally:
        os.chdir(original_dir)

    # 7. Install to external/gnatcov_rts/install/
    print()
    print_info("Installing runtime...")

    # Install library files
    lib_dir = install_prefix / "lib"
    lib_dir.mkdir(parents=True)

    lib_src = build_dir / "lib-gnatcov_rts_full.static"
    if lib_src.exists():
        for item in lib_src.iterdir():
            if item.is_file():
                shutil.copy2(item, lib_dir)
    else:
        print_error(f"Library directory not found: {lib_src}")
        return False

    # Install Ada specification files (.ads)
    include_dir = install_prefix / "include"
    include_dir.mkdir(parents=True)

    for ads_file in build_dir.glob("*.ads"):
        shutil.copy2(ads_file, include_dir)

    # Install GPR project files
    gpr_dir = install_prefix / "share" / "gpr"
    gpr_dir.mkdir(parents=True)

    shutil.copy2(build_dir / "gnatcov_rts_full.gpr", gpr_dir)
    shutil.copy2(build_dir / "gnatcov_rts.gpr", gpr_dir)

    # Update GPR to use install paths
    gpr_file = gpr_dir / "gnatcov_rts_full.gpr"
    content = gpr_file.read_text()

    # Replace Library_Dir path
    content = content.replace(
        'for Library_Dir use "lib-gnatcov_rts_full." & Library_Type;',
        'for Library_Dir use external("GNATCOV_RTS_PREFIX", ".") & "/lib";'
    )

    # Replace Object_Dir path
    content = content.replace(
        'for Object_Dir use "obj-gnatcov_rts_full." & Library_Type;',
        'for Object_Dir use external("GNATCOV_RTS_PREFIX", ".") & "/lib";'
    )

    gpr_file.write_text(content)

    print_success(f"Runtime installed to: {install_prefix}")

    # 8. Verify installation
    print()
    print_info("Verifying installation...")

    lib_file = lib_dir / "libgnatcov_rts_full.a"
    if lib_file.exists():
        print_success(f"Library file found: libgnatcov_rts_full.a")
    else:
        print_error("Library file missing!")
        return False

    gpr_file = gpr_dir / "gnatcov_rts_full.gpr"
    if gpr_file.exists():
        print_success("GPR project file found")
    else:
        print_error("GPR project file missing!")
        return False

    ads_count = len(list(include_dir.glob("*.ads")))
    print_success(f"Installed {ads_count} Ada specification files")

    print()
    print_section("Runtime Build Complete!")
    print()
    print(f"Installed to: {install_prefix}")
    print()
    print("You can now run: make test-coverage")
    print()

    return True


def main() -> int:
    """Main entry point."""
    try:
        root = find_project_root()

        if build_runtime(root):
            return 0
        else:
            return 1

    except KeyboardInterrupt:
        print()
        print_warning("Build interrupted by user")
        return 130

    except Exception as e:
        print_error(f"Unexpected error: {e}")
        import traceback
        traceback.print_exc()
        return 1


if __name__ == '__main__':
    sys.exit(main())
