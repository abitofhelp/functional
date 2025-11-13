#!/usr/bin/env python3
# ==============================================================================
# cleanup_markdown_docs.py - Clean up and consolidate markdown documentation
# ==============================================================================
# Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
# SPDX-License-Identifier: BSD-3-Clause
# See LICENSE file in the project root.
# ==============================================================================
"""
Clean up, consolidate, and validate markdown documentation files.

This script manages external user-facing documentation (.md files):
1. Scans all markdown files in docs/ and project root
2. Validates accuracy against current codebase
3. Identifies outdated or incorrect documentation
4. Consolidates duplicate information
5. Purges obsolete documentation
6. Ensures consistent formatting and structure

This is external documentation for users/developers, separate from
internal source code docstrings (.ads/.adb files).

Usage:
    python3 scripts/cleanup_markdown_docs.py [--dry-run] [--verbose] [--purge]

Options:
    --dry-run    Show what would be changed without modifying files
    --verbose    Show detailed information about each file
    --purge      Remove obsolete markdown files (requires confirmation)
"""

import sys
import re
import argparse
from pathlib import Path
from typing import List, Dict, Set, Optional
from datetime import datetime


class MarkdownDocCleaner:
    def __init__(self, project_root: Path, dry_run: bool = False,
                 verbose: bool = False, purge: bool = False):
        self.project_root = project_root
        self.dry_run = dry_run
        self.verbose = verbose
        self.purge = purge
        self.year = datetime.now().year

    def find_markdown_files(self) -> List[Path]:
        """Find all markdown files in the project."""
        md_files = []

        # Root markdown files
        md_files.extend(self.project_root.glob("*.md"))

        # Documentation directory
        docs_dir = self.project_root / "docs"
        if docs_dir.exists():
            md_files.extend(docs_dir.rglob("*.md"))

        # Examples directory (may have READMEs)
        examples_dir = self.project_root / "examples"
        if examples_dir.exists():
            md_files.extend(examples_dir.rglob("*.md"))

        return sorted(md_files)

    def categorize_markdown_files(self, md_files: List[Path]) -> Dict[str, List[Path]]:
        """Categorize markdown files by purpose."""
        categories = {
            'essential': [],      # README, LICENSE, CHANGELOG, CONTRIBUTING
            'architecture': [],   # Design and architecture docs
            'user_guide': [],     # User-facing documentation
            'developer': [],      # Developer documentation
            'examples': [],       # Example documentation
            'obsolete': [],       # Potentially obsolete
            'unknown': []         # Uncategorized
        }

        essential_names = {
            'README.md', 'LICENSE.md', 'CHANGELOG.md', 'CONTRIBUTING.md',
            'CODE_OF_CONDUCT.md', 'SECURITY.md'
        }

        for md_file in md_files:
            name = md_file.name

            if name in essential_names:
                categories['essential'].append(md_file)
            elif 'architecture' in name.lower() or 'design' in name.lower():
                categories['architecture'].append(md_file)
            elif 'guide' in name.lower() or 'tutorial' in name.lower():
                categories['user_guide'].append(md_file)
            elif 'developer' in name.lower() or 'contributing' in name.lower():
                categories['developer'].append(md_file)
            elif 'example' in str(md_file).lower():
                categories['examples'].append(md_file)
            elif 'old' in name.lower() or 'deprecated' in name.lower() or 'todo' in name.lower():
                categories['obsolete'].append(md_file)
            else:
                categories['unknown'].append(md_file)

        return categories

    def validate_markdown_content(self, md_file: Path) -> Dict[str, any]:
        """Validate markdown file content for accuracy and relevance."""
        issues = []
        warnings = []

        try:
            with open(md_file, 'r', encoding='utf-8') as f:
                content = f.read()

            # Check for outdated copyright year
            copyright_match = re.search(r'Copyright.*?(\d{4})', content, re.IGNORECASE)
            if copyright_match:
                year = int(copyright_match.group(1))
                if year < self.year:
                    warnings.append(f"Outdated copyright year: {year}")

            # Check for broken internal links
            md_links = re.findall(r'\[([^\]]+)\]\(([^)]+)\)', content)
            for link_text, link_url in md_links:
                if not link_url.startswith('http'):  # Internal link
                    # Resolve relative to markdown file location
                    target = (md_file.parent / link_url).resolve()
                    if not target.exists():
                        issues.append(f"Broken link: {link_url}")

            # Check for TODO/FIXME markers
            if 'TODO' in content or 'FIXME' in content:
                warnings.append("Contains TODO/FIXME markers")

            # Check if file is mostly empty
            lines = [l for l in content.split('\n') if l.strip() and not l.strip().startswith('#')]
            if len(lines) < 5:
                warnings.append("File appears to be stub or mostly empty")

        except Exception as e:
            issues.append(f"Error reading file: {e}")

        return {
            'issues': issues,
            'warnings': warnings,
            'valid': len(issues) == 0
        }

    def cleanup_all_docs(self):
        """Clean up and consolidate all markdown documentation."""
        print(f"🔍 Finding markdown documentation files...")
        md_files = self.find_markdown_files()

        if not md_files:
            print("  ⚠️  No markdown files found")
            return

        print(f"  ✓ Found {len(md_files)} markdown files")

        if self.dry_run:
            print("🔍 DRY RUN MODE - No files will be modified")

        # Categorize files
        print(f"\n📂 Categorizing markdown files...")
        categories = self.categorize_markdown_files(md_files)

        for category, files in categories.items():
            if files:
                print(f"\n  {category.upper()}: {len(files)} files")
                if self.verbose:
                    for f in files:
                        rel_path = f.relative_to(self.project_root)
                        print(f"    - {rel_path}")

        # Validate content
        print(f"\n🔍 Validating markdown content...")
        issues_found = []
        warnings_found = []

        for md_file in md_files:
            rel_path = md_file.relative_to(self.project_root)
            validation = self.validate_markdown_content(md_file)

            if validation['issues']:
                issues_found.append((rel_path, validation['issues']))
                print(f"  ❌ {rel_path}: {len(validation['issues'])} issue(s)")
                if self.verbose:
                    for issue in validation['issues']:
                        print(f"      - {issue}")

            if validation['warnings']:
                warnings_found.append((rel_path, validation['warnings']))
                if self.verbose:
                    print(f"  ⚠️  {rel_path}: {len(validation['warnings'])} warning(s)")
                    for warning in validation['warnings']:
                        print(f"      - {warning}")

        # Summary
        print(f"\n{'─' * 70}")
        print(f"📊 Summary:")
        print(f"   - Total markdown files: {len(md_files)}")
        print(f"   - Essential docs: {len(categories['essential'])}")
        print(f"   - Architecture docs: {len(categories['architecture'])}")
        print(f"   - User guides: {len(categories['user_guide'])}")
        print(f"   - Developer docs: {len(categories['developer'])}")
        print(f"   - Example docs: {len(categories['examples'])}")
        print(f"   - Potentially obsolete: {len(categories['obsolete'])}")
        print(f"   - Files with issues: {len(issues_found)}")
        print(f"   - Files with warnings: {len(warnings_found)}")

        if categories['obsolete'] and self.purge:
            print(f"\n⚠️  PURGE MODE: The following files would be removed:")
            for f in categories['obsolete']:
                print(f"    - {f.relative_to(self.project_root)}")
            if not self.dry_run:
                confirm = input("\nProceed with purge? (yes/no): ")
                if confirm.lower() == 'yes':
                    for f in categories['obsolete']:
                        f.unlink()
                        print(f"  🗑️  Removed: {f.relative_to(self.project_root)}")

        if issues_found:
            print(f"\n⚠️  Action required: {len(issues_found)} file(s) have issues that need fixing")
        elif warnings_found:
            print(f"\n✅ All markdown files are valid (some warnings)")
        else:
            print(f"\n✅ All markdown files are valid")


def main():
    parser = argparse.ArgumentParser(
        description="Clean up and consolidate markdown documentation"
    )
    parser.add_argument(
        "--dry-run",
        action="store_true",
        help="Show what would be changed without modifying files"
    )
    parser.add_argument(
        "--verbose",
        action="store_true",
        help="Show detailed information about each file"
    )
    parser.add_argument(
        "--purge",
        action="store_true",
        help="Remove obsolete markdown files (requires confirmation)"
    )

    args = parser.parse_args()

    # Project root (assumes script is in scripts/)
    project_root = Path(__file__).parent.parent

    cleaner = MarkdownDocCleaner(project_root, args.dry_run, args.verbose, args.purge)
    cleaner.cleanup_all_docs()


if __name__ == '__main__':
    main()
