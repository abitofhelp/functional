#!/usr/bin/env python3
# ==============================================================================
# add_md_headers.py - Add standard metadata headers to markdown files
# ==============================================================================
# Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
# SPDX-License-Identifier: BSD-3-Clause
# See LICENSE file in the project root.
# ==============================================================================
"""
Add standard metadata headers to all markdown files.

This script ensures all markdown files (.md) in the project have a consistent
header with version, date, copyright, and license information.

The header format matches README.md and is inserted after the title (first # heading)
if not already present.

Usage:
    python3 tools/add_md_headers.py [--dry-run] [--force]

Options:
    --dry-run    Show what would be changed without modifying files
    --force      Add header even if partial metadata already exists
"""

import argparse
import re
import sys
from datetime import datetime
from pathlib import Path
from typing import Optional, Tuple


def get_current_version(project_root: Path) -> str:
    """Extract current version from root alire.toml"""
    toml_path = project_root / "alire.toml"
    try:
        with open(toml_path, 'r') as f:
            for line in f:
                match = re.match(r'^\s*version\s*=\s*"([^"]+)"', line)
                if match:
                    return match.group(1)
    except Exception as e:
        print(f"Warning: Could not read version from alire.toml: {e}")
    return "0.1.0-dev"  # Fallback


def create_header(version: str, year: int) -> str:
    """Create the standard markdown header"""
    date_str = datetime.now().strftime("%B %d, %Y")

    return f"""**Version:** {version}
**Date:** {date_str}
**SPDX-License-Identifier:** BSD-3-Clause
**License File:** See the LICENSE file in the project root.
**Copyright:** © {year} Michael Gardner, A Bit of Help, Inc.
**Status:** Unreleased
"""


def has_metadata_header(content: str) -> bool:
    """Check if file already has version metadata"""
    # Look for at least 2 of the key fields
    has_version = bool(re.search(r'\*\*Version:\*\*', content))
    has_copyright = bool(re.search(r'\*\*Copyright:\*\*', content))
    has_license = bool(re.search(r'\*\*SPDX-License-Identifier:\*\*', content))

    # Consider it has header if it has at least 2 fields
    return sum([has_version, has_copyright, has_license]) >= 2


def find_title_line(lines: list) -> Optional[int]:
    """Find the first # heading (title) line index"""
    for i, line in enumerate(lines):
        if re.match(r'^#\s+\S', line):  # Matches "# Title" but not "##"
            return i
    return None


def insert_header(file_path: Path, version: str, year: int, force: bool = False) -> Tuple[bool, str]:
    """
    Insert standard header into markdown file.

    Returns (was_modified, reason)
    """
    try:
        with open(file_path, 'r', encoding='utf-8') as f:
            content = f.read()
            lines = content.splitlines(keepends=True)

        # Check if header already exists
        if has_metadata_header(content) and not force:
            return False, "Already has metadata header"

        # Find the title line
        title_idx = find_title_line(lines)
        if title_idx is None:
            return False, "No title (# heading) found"

        # Create the header
        header = create_header(version, year)
        header_lines = [line + '\n' for line in header.split('\n') if line.strip()]

        # Insert header after title with blank line separator
        new_lines = (
            lines[:title_idx + 1] +  # Title
            ['\n'] +                   # Blank line
            header_lines +             # Header
            ['\n'] +                   # Blank line
            lines[title_idx + 1:]      # Rest of content
        )

        # Write back
        with open(file_path, 'w', encoding='utf-8') as f:
            f.writelines(new_lines)

        return True, "Header added"

    except Exception as e:
        return False, f"Error: {e}"


def find_markdown_files(project_root: Path) -> list[Path]:
    """Find all markdown files in the project"""
    md_files = []

    # Search in common locations
    search_paths = [
        project_root / "docs",
        project_root / "*.md",
    ]

    for search_path in search_paths:
        if '*' in str(search_path):
            # Glob pattern
            md_files.extend(project_root.glob(str(search_path.relative_to(project_root))))
        else:
            # Directory
            if search_path.exists() and search_path.is_dir():
                md_files.extend(search_path.rglob("*.md"))

    # Exclude certain files
    excluded = {'CHANGELOG.md', 'LICENSE.md'}
    md_files = [f for f in md_files if f.name not in excluded]

    return sorted(set(md_files))


def main():
    parser = argparse.ArgumentParser(
        description="Add standard metadata headers to all markdown files"
    )
    parser.add_argument(
        "--dry-run",
        action="store_true",
        help="Show what would be changed without modifying files"
    )
    parser.add_argument(
        "--force",
        action="store_true",
        help="Add header even if partial metadata already exists"
    )

    args = parser.parse_args()

    # Find project root
    script_dir = Path(__file__).parent
    project_root = script_dir.parent

    # Get current version and year
    version = get_current_version(project_root)
    year = datetime.now().year

    print(f"📋 Adding standard headers to markdown files")
    print(f"   Version: {version}")
    print(f"   Year: {year}")

    if args.dry_run:
        print("   🔍 DRY RUN MODE - No files will be modified\n")

    if args.force:
        print("   ⚠️  FORCE MODE - Will overwrite existing headers\n")

    # Find all markdown files
    md_files = find_markdown_files(project_root)

    if not md_files:
        print("⚠️  No markdown files found")
        return

    print(f"📁 Found {len(md_files)} markdown file(s):\n")

    # Process each file
    added_count = 0
    skipped_count = 0

    for md_file in md_files:
        rel_path = md_file.relative_to(project_root)

        if args.dry_run:
            # Just check, don't modify
            try:
                with open(md_file, 'r') as f:
                    content = f.read()

                if has_metadata_header(content) and not args.force:
                    print(f"⏭️  {rel_path}: Already has header (would skip)")
                    skipped_count += 1
                else:
                    title_idx = find_title_line(content.splitlines(keepends=True))
                    if title_idx is None:
                        print(f"⏭️  {rel_path}: No title found (would skip)")
                        skipped_count += 1
                    else:
                        print(f"✏️  {rel_path}: Would add header")
                        added_count += 1
            except Exception as e:
                print(f"❌ {rel_path}: Error - {e}")
                skipped_count += 1
        else:
            # Actually modify
            was_modified, reason = insert_header(md_file, version, year, args.force)

            if was_modified:
                print(f"✅ {rel_path}: {reason}")
                added_count += 1
            else:
                print(f"⏭️  {rel_path}: {reason}")
                skipped_count += 1

    # Summary
    print(f"\n{'─' * 60}")
    print(f"📊 Summary:")
    print(f"   - Files processed: {len(md_files)}")
    print(f"   - Headers added: {added_count}")
    print(f"   - Skipped: {skipped_count}")

    if args.dry_run and added_count > 0:
        print(f"\n💡 Run without --dry-run to apply changes")
    elif added_count > 0:
        print(f"\n✅ Standard headers added successfully!")
    else:
        print(f"\n✅ All files already have headers")


if __name__ == "__main__":
    main()
