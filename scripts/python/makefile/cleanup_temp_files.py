#!/usr/bin/env python3
# ==============================================================================
# cleanup_temp_files.py - Remove temporary files and build artifacts
# ==============================================================================
# Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
# SPDX-License-Identifier: BSD-3-Clause
# See LICENSE file in the project root.
# ==============================================================================
"""
Remove temporary files, backup files, and build artifacts from the project.

Supports both Ada and Go projects.

This script finds and removes:
- Backup files (.bak, .backup, ~, .orig, .swp, .swo)
- Ada build artifacts (.o, .ali, .a, .so, .dylib, .dll, .exe)
- Go build artifacts (.test, coverage.out, *.coverprofile)
- Editor temp files (.DS_Store, Thumbs.db, .tmp)
- Python cache (__pycache__/, *.pyc, *.pyo)
- Ada coverage artifacts (*.gcda, *.gcno, *.gcov)
- Go coverage artifacts (coverage.out, coverage.html, *.coverprofile)
- Log files (*.log)
- Core dumps (core, core.*)

This is safe to run as it skips important directories like .git/, alire/, vendor/, etc.

Usage:
    python3 scripts/makefile/cleanup_temp_files.py [--dry-run] [--verbose] [--aggressive]

Options:
    --dry-run      Show what would be removed without deleting
    --verbose      Show detailed information about each file
    --aggressive   Also remove obj/ and bin/ directories
"""

import sys
import argparse
from pathlib import Path
from typing import List, Dict
import shutil

# Add scripts directory to path for imports (go up one level from makefile/)
sys.path.insert(0, str(Path(__file__).parent.parent))

from common import print_success, print_error, print_info, print_warning


class TempFileCleaner:
    """Finds and removes temporary files from the project."""

    def __init__(self, project_root: Path, dry_run: bool = False,
                 verbose: bool = False, aggressive: bool = False):
        self.project_root = project_root
        self.dry_run = dry_run
        self.verbose = verbose
        self.aggressive = aggressive

        # Directories to always skip (Ada and Go)
        self.skip_dirs = {
            # Version control
            '.git',
            # Ada (Alire)
            '.alire', 'alire', 'config',
            # Go
            'vendor',
            # Python
            '.venv', 'venv',
            # Node
            'node_modules',
            # Other
            '.cache', 'tools', 'fixtures', 'test/fixtures',
        }

        # File extensions to remove (temporary/backup files)
        self.temp_extensions = {
            # Backup files
            '.bak', '.backup', '.orig', '.old', '.tmp',
            # Editor temp files
            '.swp', '.swo', '.swn', '~',
            # Ada build artifacts
            '.o', '.ali', '.a', '.so', '.dylib', '.dll', '.exe',
            # Go build artifacts
            '.test',  # Go test binaries
            '.coverprofile',  # Go coverage profiles
            # Ada coverage artifacts (GNATcov/GCC)
            '.gcda', '.gcno', '.gcov',
            # Python cache
            '.pyc', '.pyo',
            # Log files
            '.log',
        }

        # Specific filenames to remove
        self.temp_filenames = {
            # Editor/OS temp files
            '.DS_Store', 'Thumbs.db', 'desktop.ini',
            # Core dumps
            'core',
            # Python
            '__pycache__', '.coverage',
            # Go coverage output files
            'coverage.out', 'coverage.html', 'coverage.txt',
        }

        # Directories to remove if aggressive mode
        self.aggressive_dirs = {
            'obj', 'bin', 'lib', 'build', 'dist',
            'coverage', 'htmlcov', '.pytest_cache',
        }

    def should_skip_dir(self, dir_path: Path) -> bool:
        """Check if directory should be skipped."""
        # Check if any parent is in skip_dirs
        for parent in dir_path.parents:
            if parent.name in self.skip_dirs:
                return True
        return dir_path.name in self.skip_dirs

    def find_temp_files(self) -> Dict[str, List[Path]]:
        """Find all temporary files in the project."""
        results: Dict[str, List[Path]] = {
            'backup_files': [],
            'build_artifacts': [],
            'editor_temp': [],
            'python_cache': [],
            'coverage_files': [],
            'log_files': [],
            'other_temp': [],
            'temp_dirs': [],
        }

        for item in self.project_root.rglob('*'):
            # Skip directories we don't want to search
            if item.is_dir():
                if self.should_skip_dir(item):
                    continue

                # Check for temp directory names
                if item.name in self.temp_filenames:
                    results['temp_dirs'].append(item)

                # Aggressive mode: remove build directories
                if self.aggressive and item.name in self.aggressive_dirs:
                    # Only if it's at project root or in a subproject
                    if item.parent == self.project_root or \
                       any(gpr.exists() for gpr in item.parent.glob('*.gpr')):
                        results['temp_dirs'].append(item)

                continue

            if not item.is_file():
                continue

            # Check parent directory first (optimization)
            if self.should_skip_dir(item.parent):
                continue

            # Categorize by extension
            suffix = item.suffix.lower()
            name = item.name

            if suffix in ['.bak', '.backup', '.orig', '.old'] or name.endswith('~'):
                results['backup_files'].append(item)
            elif suffix in ['.o', '.ali', '.a', '.so', '.dylib', '.dll', '.exe', '.test']:
                # Ada: .o, .ali, .a, .so, .dylib  Go: .test
                results['build_artifacts'].append(item)
            elif suffix in ['.swp', '.swo', '.swn'] or name == '.DS_Store' or name == 'Thumbs.db':
                results['editor_temp'].append(item)
            elif suffix in ['.pyc', '.pyo'] or '__pycache__' in str(item):
                results['python_cache'].append(item)
            elif suffix in ['.gcda', '.gcno', '.gcov', '.coverprofile'] or \
                 name in ['coverage.out', 'coverage.html', 'coverage.txt']:
                # Ada: .gcda, .gcno, .gcov  Go: coverage.out, .coverprofile
                results['coverage_files'].append(item)
            elif suffix == '.log':
                results['log_files'].append(item)
            elif suffix in self.temp_extensions or name in self.temp_filenames:
                results['other_temp'].append(item)

        return results

    def cleanup(self) -> None:
        """Find and remove temporary files."""
        print_info(f"Scanning for temporary files in {self.project_root}...")
        print(f"   Skipping: {', '.join(sorted(self.skip_dirs))}")

        if self.aggressive:
            print_warning(
                f"AGGRESSIVE MODE: Will also remove "
                f"{', '.join(sorted(self.aggressive_dirs))} directories"
            )

        # Find all temp files
        temp_files = self.find_temp_files()

        # Count totals
        total_files = sum(
            len(files) for category, files in temp_files.items()
            if category != 'temp_dirs'
        )
        total_dirs = len(temp_files['temp_dirs'])
        total_count = total_files + total_dirs

        if total_count == 0:
            print()
            print_success("No temporary files found - project is clean!")
            return

        # Report findings
        print(f"\nFound {total_count} temporary items:")

        for category, files in temp_files.items():
            if not files:
                continue

            category_name = category.replace('_', ' ').title()
            print(f"\n  {category_name}: {len(files)}")

            if self.verbose:
                for f in sorted(files)[:10]:  # Show first 10
                    rel_path = f.relative_to(self.project_root)
                    if f.is_dir():
                        print(f"    [dir]  {rel_path}/")
                    else:
                        size = f.stat().st_size
                        print(f"    [file] {rel_path} ({size:,} bytes)")

                if len(files) > 10:
                    print(f"    ... and {len(files) - 10} more")

        # Perform removal
        if not self.dry_run:
            print(f"\nRemoving {total_count} items...")
            removed_count = 0

            # Remove directories first (deepest first)
            sorted_dirs = sorted(
                temp_files['temp_dirs'],
                key=lambda p: len(p.parts),
                reverse=True
            )
            for temp_dir in sorted_dirs:
                try:
                    if temp_dir.exists():  # Check again as parent might be deleted
                        shutil.rmtree(temp_dir)
                        removed_count += 1
                        if self.verbose:
                            rel_path = temp_dir.relative_to(self.project_root)
                            print(f"    Removed {rel_path}/")
                except Exception as e:
                    print(f"    Warning: Could not remove {temp_dir}: {e}")

            # Remove files
            for category, files in temp_files.items():
                if category == 'temp_dirs':
                    continue

                for temp_file in files:
                    try:
                        # Check again as parent dir might be deleted
                        if temp_file.exists():
                            temp_file.unlink()
                            removed_count += 1
                            if self.verbose:
                                rel_path = temp_file.relative_to(self.project_root)
                                print(f"    Removed {rel_path}")
                    except Exception as e:
                        print(f"    Warning: Could not remove {temp_file}: {e}")

            print()
            print_success(f"Successfully removed {removed_count} items")

        else:
            print(f"\nRun without --dry-run to remove these {total_count} items")
            if not self.aggressive:
                print("   Add --aggressive to also remove build directories")

        # Calculate space saved (approximate)
        if total_files > 0:
            total_size = 0
            for category, files in temp_files.items():
                if category == 'temp_dirs':
                    continue
                for f in files:
                    try:
                        if f.exists():
                            total_size += f.stat().st_size
                    except Exception:
                        pass

            if total_size > 0:
                size_mb = total_size / (1024 * 1024)
                print(f"   Approximate space to reclaim: {size_mb:.2f} MB")


def main() -> int:
    """Main entry point."""
    parser = argparse.ArgumentParser(
        description="Remove temporary files and build artifacts"
    )
    parser.add_argument(
        "--dry-run",
        action="store_true",
        help="Show what would be removed without deleting"
    )
    parser.add_argument(
        "--verbose",
        action="store_true",
        help="Show detailed information about each file"
    )
    parser.add_argument(
        "--aggressive",
        action="store_true",
        help="Also remove obj/ and bin/ build directories"
    )

    args = parser.parse_args()

    # Project root is two levels up from scripts/makefile/
    project_root = Path(__file__).parent.parent.parent

    cleaner = TempFileCleaner(
        project_root, args.dry_run, args.verbose, args.aggressive
    )
    cleaner.cleanup()
    return 0


if __name__ == '__main__':
    sys.exit(main())
