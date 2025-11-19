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

This script finds and removes:
- Backup files (.bak, .backup, ~, .orig, .swp, .swo)
- Build artifacts (.o, .ali, .a, .so, .dylib, .dll, .exe)
- Editor temp files (.DS_Store, Thumbs.db, .tmp)
- Python cache (__pycache__/, *.pyc, *.pyo)
- Coverage artifacts (*.gcda, *.gcno, *.gcov)
- Log files (*.log)
- Core dumps (core, core.*)

This is safe to run as it skips important directories like .git/, alire/, etc.

Usage:
    python3 scripts/cleanup_temp_files.py [--dry-run] [--verbose] [--aggressive]

Options:
    --dry-run      Show what would be removed without deleting
    --verbose      Show detailed information about each file
    --aggressive   Also remove obj/ and bin/ directories
"""

import sys
import argparse
from pathlib import Path
from typing import List, Set, Dict
import shutil


class TempFileCleaner:
    def __init__(self, project_root: Path, dry_run: bool = False,
                 verbose: bool = False, aggressive: bool = False):
        self.project_root = project_root
        self.dry_run = dry_run
        self.verbose = verbose
        self.aggressive = aggressive

        # Directories to always skip
        self.skip_dirs = {
            '.git', '.alire', 'alire', 'node_modules', '.venv', 'venv',
            '.cache', 'tools', 'fixtures', 'test/fixtures'
        }

        # File extensions to remove (temporary/backup files)
        self.temp_extensions = {
            # Backup files
            '.bak', '.backup', '.orig', '.old', '.tmp',
            # Editor temp files
            '.swp', '.swo', '.swn', '~',
            # Build artifacts
            '.o', '.ali', '.a', '.so', '.dylib', '.dll', '.exe',
            # Coverage artifacts
            '.gcda', '.gcno', '.gcov',
            # Python cache
            '.pyc', '.pyo',
            # Log files
            '.log',
        }

        # Specific filenames to remove
        self.temp_filenames = {
            '.DS_Store', 'Thumbs.db', 'desktop.ini',
            'core', '__pycache__', '.coverage',
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
        results = {
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
            elif suffix in ['.o', '.ali', '.a', '.so', '.dylib', '.dll', '.exe']:
                results['build_artifacts'].append(item)
            elif suffix in ['.swp', '.swo', '.swn'] or name == '.DS_Store' or name == 'Thumbs.db':
                results['editor_temp'].append(item)
            elif suffix in ['.pyc', '.pyo'] or '__pycache__' in str(item):
                results['python_cache'].append(item)
            elif suffix in ['.gcda', '.gcno', '.gcov']:
                results['coverage_files'].append(item)
            elif suffix == '.log':
                results['log_files'].append(item)
            elif suffix in self.temp_extensions or name in self.temp_filenames:
                results['other_temp'].append(item)

        return results

    def cleanup(self):
        """Find and remove temporary files."""
        print(f"🔍 Scanning for temporary files in {self.project_root}...")
        print(f"   Skipping: {', '.join(sorted(self.skip_dirs))}")

        if self.aggressive:
            print(f"   ⚠️  AGGRESSIVE MODE: Will also remove {', '.join(sorted(self.aggressive_dirs))} directories")

        # Find all temp files
        temp_files = self.find_temp_files()

        # Count totals
        total_files = sum(len(files) for category, files in temp_files.items() if category != 'temp_dirs')
        total_dirs = len(temp_files['temp_dirs'])
        total_count = total_files + total_dirs

        if total_count == 0:
            print(f"\n✅ No temporary files found - project is clean!")
            return

        # Report findings
        print(f"\n📊 Found {total_count} temporary items:")

        for category, files in temp_files.items():
            if not files:
                continue

            category_name = category.replace('_', ' ').title()
            print(f"\n  {category_name}: {len(files)}")

            if self.verbose:
                for f in sorted(files)[:10]:  # Show first 10
                    rel_path = f.relative_to(self.project_root)
                    if f.is_dir():
                        print(f"    📁 {rel_path}/")
                    else:
                        size = f.stat().st_size
                        print(f"    📄 {rel_path} ({size:,} bytes)")

                if len(files) > 10:
                    print(f"    ... and {len(files) - 10} more")

        # Perform removal
        if not self.dry_run:
            print(f"\n🗑️  Removing {total_count} items...")
            removed_count = 0

            # Remove directories first (deepest first)
            for temp_dir in sorted(temp_files['temp_dirs'], key=lambda p: len(p.parts), reverse=True):
                try:
                    if temp_dir.exists():  # Check again as parent might be deleted
                        shutil.rmtree(temp_dir)
                        removed_count += 1
                        if self.verbose:
                            rel_path = temp_dir.relative_to(self.project_root)
                            print(f"    ✓ Removed {rel_path}/")
                except Exception as e:
                    print(f"    ⚠️  Could not remove {temp_dir}: {e}")

            # Remove files
            for category, files in temp_files.items():
                if category == 'temp_dirs':
                    continue

                for temp_file in files:
                    try:
                        if temp_file.exists():  # Check again as parent dir might be deleted
                            temp_file.unlink()
                            removed_count += 1
                            if self.verbose:
                                rel_path = temp_file.relative_to(self.project_root)
                                print(f"    ✓ Removed {rel_path}")
                    except Exception as e:
                        print(f"    ⚠️  Could not remove {temp_file}: {e}")

            print(f"\n✅ Successfully removed {removed_count} items")

        else:
            print(f"\n💡 Run without --dry-run to remove these {total_count} items")
            if not self.aggressive:
                print(f"   Add --aggressive to also remove build directories")

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
                    except:
                        pass

            if total_size > 0:
                size_mb = total_size / (1024 * 1024)
                print(f"   💾 Approximate space to reclaim: {size_mb:.2f} MB")


def main():
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

    # Project root (assumes script is in scripts/)
    project_root = Path(__file__).parent.parent

    cleaner = TempFileCleaner(project_root, args.dry_run, args.verbose, args.aggressive)
    cleaner.cleanup()


if __name__ == '__main__':
    main()
