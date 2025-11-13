#!/usr/bin/env python3
# ==============================================================================
# fix_markdown_headers.py - Fix markdown headers to include trailing spaces
# ==============================================================================
# Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
# SPDX-License-Identifier: BSD-3-Clause
# See LICENSE file in the project root.
# ==============================================================================
"""
Validate and fix markdown metadata headers to ensure proper line breaks.

This script:
- Finds all markdown files with metadata headers
- Validates that each metadata line ends with two spaces
- Fixes any lines missing the trailing spaces
- Reports all fixes made

Markdown requires two trailing spaces at the end of a line for a line break.
Without them, multiple lines render as a single wrapped line.

Usage:
    python3 scripts/fix_markdown_headers.py [--dry-run] [--verbose]

Options:
    --dry-run    Show what would be fixed without making changes
    --verbose    Show detailed information about each file
"""

import sys
import argparse
import re
from pathlib import Path
from typing import List, Dict, Tuple


class MarkdownHeaderFixer:
    def __init__(self, project_root: Path, dry_run: bool = False, verbose: bool = False):
        self.project_root = project_root
        self.dry_run = dry_run
        self.verbose = verbose

        # Metadata patterns that should have trailing spaces
        self.metadata_patterns = [
            r'^\*\*Version:\*\*',
            r'^\*\*Date:\*\*',
            r'^\*\*SPDX-License-Identifier:\*\*',
            r'^\*\*License File:\*\*',
            r'^\*\*Copyright:\*\*',
            r'^\*\*Status:\*\*',
        ]

    def is_metadata_line(self, line: str) -> bool:
        """Check if line is a metadata line that needs trailing spaces."""
        for pattern in self.metadata_patterns:
            if re.match(pattern, line):
                return True
        return False

    def needs_fix(self, line: str) -> bool:
        """Check if a metadata line is missing trailing spaces."""
        if not self.is_metadata_line(line):
            return False

        # Check if line ends with exactly two spaces before newline
        # Line might be "**Version:** 1.0.0\n" or "**Version:** 1.0.0  \n"
        stripped = line.rstrip('\n\r')
        return not stripped.endswith('  ')

    def fix_line(self, line: str) -> str:
        """Add trailing spaces to metadata line if missing."""
        if not self.is_metadata_line(line):
            return line

        # Remove existing trailing whitespace (but keep newline)
        has_newline = line.endswith('\n')
        stripped = line.rstrip()

        # Add two trailing spaces and restore newline
        fixed = stripped + '  '
        if has_newline:
            fixed += '\n'

        return fixed

    def fix_file(self, md_file: Path) -> Tuple[bool, int]:
        """
        Fix markdown file headers.
        Returns: (was_modified, num_lines_fixed)
        """
        try:
            with open(md_file, 'r', encoding='utf-8') as f:
                lines = f.readlines()

            fixed_lines = []
            lines_fixed = 0

            for line in lines:
                if self.needs_fix(line):
                    fixed_lines.append(self.fix_line(line))
                    lines_fixed += 1
                else:
                    fixed_lines.append(line)

            if lines_fixed > 0 and not self.dry_run:
                with open(md_file, 'w', encoding='utf-8') as f:
                    f.writelines(fixed_lines)

            return (lines_fixed > 0, lines_fixed)

        except Exception as e:
            print(f"  ⚠️  Error processing {md_file}: {e}")
            return (False, 0)

    def find_and_fix_markdown_files(self) -> Dict[str, List[Path]]:
        """Find and fix all markdown files with metadata headers."""
        results = {
            'fixed': [],
            'already_correct': [],
            'no_metadata': [],
        }

        # Find all markdown files
        md_files = list(self.project_root.rglob('*.md'))

        # Skip certain directories
        skip_dirs = {'.git', '.alire', 'alire', 'node_modules', 'archive'}

        for md_file in sorted(md_files):
            # Skip files in excluded directories
            if any(skip_dir in md_file.parts for skip_dir in skip_dirs):
                continue

            try:
                with open(md_file, 'r', encoding='utf-8') as f:
                    content = f.read()

                # Check if file has metadata
                has_metadata = any(
                    re.search(pattern, content, re.MULTILINE)
                    for pattern in self.metadata_patterns
                )

                if not has_metadata:
                    results['no_metadata'].append(md_file)
                    continue

                # Check and fix the file
                was_modified, lines_fixed = self.fix_file(md_file)

                if was_modified:
                    results['fixed'].append((md_file, lines_fixed))
                    if self.verbose:
                        rel_path = md_file.relative_to(self.project_root)
                        action = "Would fix" if self.dry_run else "Fixed"
                        print(f"  {action}: {rel_path} ({lines_fixed} lines)")
                else:
                    results['already_correct'].append(md_file)
                    if self.verbose:
                        rel_path = md_file.relative_to(self.project_root)
                        print(f"  ✓ Already correct: {rel_path}")

            except Exception as e:
                print(f"  ⚠️  Error reading {md_file}: {e}")

        return results

    def run(self):
        """Main execution method."""
        print(f"🔍 Scanning for markdown files in {self.project_root}...")

        if self.dry_run:
            print("   📋 DRY RUN MODE - No files will be modified")

        results = self.find_and_fix_markdown_files()

        # Report results
        print(f"\n📊 Results:")
        print(f"   - Files with metadata: {len(results['fixed']) + len(results['already_correct'])}")
        print(f"   - Files fixed: {len(results['fixed'])}")
        print(f"   - Files already correct: {len(results['already_correct'])}")
        print(f"   - Files without metadata: {len(results['no_metadata'])}")

        if results['fixed']:
            total_lines = sum(count for _, count in results['fixed'])
            print(f"\n✅ Fixed {len(results['fixed'])} files ({total_lines} lines total):")
            for md_file, count in results['fixed']:
                rel_path = md_file.relative_to(self.project_root)
                action = "would be fixed" if self.dry_run else "fixed"
                print(f"   - {rel_path}: {count} lines {action}")

        if self.dry_run and results['fixed']:
            print(f"\n💡 Run without --dry-run to apply fixes")
        elif not results['fixed']:
            print(f"\n✅ All markdown headers are already correct!")


def main():
    parser = argparse.ArgumentParser(
        description="Validate and fix markdown metadata headers"
    )
    parser.add_argument(
        "--dry-run",
        action="store_true",
        help="Show what would be fixed without making changes"
    )
    parser.add_argument(
        "--verbose",
        action="store_true",
        help="Show detailed information about each file"
    )

    args = parser.parse_args()

    # Project root (assumes script is in scripts/)
    project_root = Path(__file__).parent.parent

    fixer = MarkdownHeaderFixer(project_root, args.dry_run, args.verbose)
    fixer.run()


if __name__ == '__main__':
    main()
