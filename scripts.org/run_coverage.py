#!/usr/bin/env python3
# ==============================================================================
# run_coverage.py
# ==============================================================================
# Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
# SPDX-License-Identifier: BSD-3-Clause
# See LICENSE file in the project root.
#
# Purpose:
#   Test coverage analysis automation.
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
#       Uses tests-coverage.gpr for instrumentation build
#       Provides fallback if gcovr not installed (basic .gcov files)
#
# See Also:
#   tests-coverage.gpr - coverage build configuration
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
    coverage_patterns = ['*.gcda', '*.gcno', '*.gcov', '*.gcov.json.gz']
    for pattern in coverage_patterns:
        for path in Path('.').rglob(pattern):
            try:
                path.unlink()
            except Exception:
                pass

    # Also remove .gcov.json.gz files from project root
    for gcov_file in Path('.').glob('*.gcov.json.gz'):
        try:
            gcov_file.unlink()
        except Exception:
            pass

    # Remove coverage directories
    for dir_name in ['test/obj', 'coverage', 'obj/test']:
        dir_path = Path(dir_name)
        if dir_path.exists():
            shutil.rmtree(dir_path)

    print_success("Coverage artifacts cleaned")


def build_with_coverage() -> bool:
    """Build test suite with coverage instrumentation."""
    print_section("Building test suite with coverage instrumentation...")

    try:
        # Build unit tests with coverage flags
        cmd_unit = [
            'alr', 'exec', '--', 'gprbuild', '-P', 'test/unit/unit_tests.gpr',
            '-p', '-q',
            '-cargs', '-fprofile-arcs', '-ftest-coverage',
            '-largs', '-fprofile-arcs', '-ftest-coverage',
        ]

        # Build integration tests with coverage flags
        cmd_integration = [
            'alr', 'exec', '--', 'gprbuild', '-P', 'test/integration/integration_tests.gpr',
            '-p', '-q',
            '-cargs', '-fprofile-arcs', '-ftest-coverage',
            '-largs', '-fprofile-arcs', '-ftest-coverage',
        ]

        print_info("Building unit tests...")
        run_command(cmd_unit)

        print_info("Building integration tests...")
        run_command(cmd_integration)

        print_success("Test suite built with coverage instrumentation")
        return True

    except Exception as e:
        print_error(f"Build failed: {e}")
        return False


def run_tests() -> bool:
    """Run the test suites (unit and integration)."""
    print_section("Running test suites...")

    import os

    # Don't use GCOV_PREFIX - let gcov write to obj directories
    # We'll find the .gcda files there
    env = os.environ.copy()

    try:
        # Run unit tests
        unit_runner = Path('test/bin/unit_runner')
        if not unit_runner.exists():
            print_error(f"Unit test runner not found: {unit_runner}")
            return False

        print_info("Running unit tests...")
        subprocess.run([str(unit_runner)], env=env, check=True)

        # Run integration tests
        integration_runner = Path('test/bin/integration_runner')
        if not integration_runner.exists():
            print_error(f"Integration test runner not found: {integration_runner}")
            return False

        print_info("Running integration tests...")
        subprocess.run([str(integration_runner)], env=env, check=True)

        print_success("All tests completed")
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
        import os
        coverage_dir = Path('coverage')
        coverage_dir.mkdir(exist_ok=True)

        # Use the gcov from the Alire toolchain to match compiler version
        project_root = Path('.').resolve()
        gcov_path = '/Users/mike/.local/share/alire/toolchains/gnat_native_15.2.1_fffe07e8/bin/gcov'

        # Change to coverage directory so intermediate files go there
        original_dir = Path.cwd()
        os.chdir(coverage_dir)

        try:
            cmd = [
                'gcovr',
                '--root', str(project_root),
                '--gcov-executable', gcov_path,
                '--delete',  # Delete intermediate .gcov files after processing
                '--html',
                '--html-details',
                '--html-title', 'TZif Library Test Coverage Report',
                '--exclude', 'test/.*',
                '--exclude', '.*/aunit.*',
                '--exclude', 'coverage/.*',
                '--print-summary',
                '-o', 'coverage.html',
            ]

            result = subprocess.run(cmd, cwd=str(project_root), check=False, capture_output=True, text=True)
            print(result.stdout)
            if result.stderr:
                print(result.stderr)
            if result.returncode != 0:
                raise RuntimeError(f"gcovr failed with exit code {result.returncode}")
        finally:
            os.chdir(original_dir)
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


def validate_architecture() -> bool:
    """Validate hexagonal architecture boundaries."""
    print_section("Validating architecture boundaries...")

    try:
        cmd = ['python3', 'scripts/arch_guard.py']
        run_command(cmd)
        print_success("Architecture validation passed")
        return True
    except Exception as e:
        print_error(f"Architecture validation failed: {e}")
        return False


def main() -> int:
    """Main entry point."""
    print_info("Running tests with coverage analysis...")
    print()

    # Step 0: Validate architecture
    if not validate_architecture():
        return 1
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
