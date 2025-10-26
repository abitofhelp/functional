#!/usr/bin/env python3
# ==============================================================================
# run_coverage.py
# ==============================================================================
# Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
# SPDX-License-Identifier: BSD-3-Clause
# See LICENSE file in the project root.
#
# Purpose:
#   Test coverage analysis automation for Functional library.
#       Builds tests with coverage instrumentation, runs test suite,
#       and generates HTML coverage reports using gcovr.
#
# Usage:
#   Run directly or via Makefile:
#          python3 scripts/run_coverage.py
#          make test-coverage
#
#          Automatically cleans previous artifacts
#          Generates HTML report in coverage/ directory
#
# Design Notes:
#   Four-phase workflow: clean, build, test, report
#       Uses tests/tests.gpr with coverage flags for instrumentation build
#       Provides fallback if gcovr not installed (basic .gcov files)
#
# See Also:
#   tests/tests.gpr - test build configuration
#       common.py - command execution utilities
#       Makefile - test-coverage target
# ==============================================================================

import shutil
import subprocess
import sys
from pathlib import Path

# Add scripts directory to path for imports
sys.path.insert(0, str(Path(__file__).parent))

from common import (
    Colors,
    command_exists,
    is_macos,
    print_error,
    print_info,
    print_section,
    print_success,
    print_warning,
    run_command,
)


def clean_coverage_artifacts() -> None:
    """Remove existing coverage artifacts."""
    print_section("Cleaning previous coverage artifacts...")

    # Remove coverage files
    coverage_patterns = ['*.gcda', '*.gcno', '*.gcov']
    for pattern in coverage_patterns:
        for path in Path('.').rglob(pattern):
            try:
                path.unlink()
            except Exception:
                pass

    # Remove coverage directories
    for dir_name in ['tests/obj', 'coverage']:
        dir_path = Path(dir_name)
        if dir_path.exists():
            shutil.rmtree(dir_path)

    print_success("Coverage artifacts cleaned")


def build_with_coverage() -> bool:
    """Build test suite with coverage instrumentation."""
    print_section("Building test suite with coverage instrumentation...")

    try:
        # Build with coverage flags
        cmd = [
            'gprbuild', '-P', 'tests/tests.gpr',
            '-p', '-q',
            '-cargs', '-fprofile-arcs', '-ftest-coverage',
            '-largs', '-fprofile-arcs', '-ftest-coverage',
        ]

        run_command(cmd)
        print_success("Test suite built with coverage instrumentation")
        return True

    except Exception as e:
        print_error(f"Build failed: {e}")
        return False


def run_tests() -> bool:
    """Run the test suite."""
    print_section("Running test suite...")

    try:
        test_runner = Path('tests/bin/test_runner')
        if not test_runner.exists():
            print_error(f"Test runner not found: {test_runner}")
            return False

        run_command([str(test_runner)])
        print_success("Tests completed")
        return True

    except Exception as e:
        print_error(f"Tests failed: {e}")
        return False


def generate_coverage_report() -> bool:
    """Generate HTML coverage report using gcovr."""
    print_section("Generating coverage report...")

    if not command_exists('gcovr'):
        print_warning("gcovr not found - generating basic .gcov files only")
        print_info("Install gcovr with: pip3 install gcovr")
        return False

    try:
        coverage_dir = Path('coverage')
        coverage_dir.mkdir(exist_ok=True)

        cmd = [
            'gcovr',
            '-r', '.',
            '--html',
            '--html-details',
            '--html-title', 'Functional Library Test Coverage Report',
            '--exclude', 'tests/.*',
            '--exclude', 'obj/.*',
            '--print-summary',
            '-o', 'coverage/coverage.html',
        ]

        run_command(cmd)
        print_success("Coverage report generated: coverage/coverage.html")

        # Print instructions for viewing
        print()
        print_info("To view the coverage report:")
        if is_macos():
            print(f"  {Colors.GREEN}open coverage/coverage.html{Colors.NC}")
        else:
            print(f"  {Colors.GREEN}xdg-open coverage/coverage.html{Colors.NC}")

        return True

    except Exception as e:
        print_error(f"Failed to generate coverage report: {e}")
        return False


def main() -> int:
    """Main entry point."""
    print_info("Running tests with coverage analysis...")
    print()

    # Step 1: Clean previous coverage artifacts
    clean_coverage_artifacts()
    print()

    # Step 2: Build with coverage instrumentation
    if not build_with_coverage():
        return 1
    print()

    # Step 3: Run tests
    if not run_tests():
        return 1
    print()

    # Step 4: Generate coverage report
    generate_coverage_report()
    print()

    print_success("Coverage analysis complete!")
    return 0


if __name__ == '__main__':
    sys.exit(main())
