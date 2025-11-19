#!/usr/bin/env python3
# ==============================================================================
# curate_guides.py - Curate documentation guides by removing dev milestones
# ==============================================================================
# Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
# SPDX-License-Identifier: BSD-3-Clause
# See LICENSE file in the project root.
# ==============================================================================
"""
Curate documentation guides by filtering out development milestones and temporary docs.

This script identifies and removes guides that were useful during development
but have no lasting value for future maintainers/users:
- Completed milestone markers (*_COMPLETE.md, *_ADDED.md)
- Solved issue documentation (*_ISSUE.md)
- Outdated status reports (*_STATUS.md, *_PROGRESS.md, *_GAPS.md)
- Chat logs and questions (GPT*, *_QUESTION.md)
- Obsolete specs that became formal docs (*_spec.md)

Keeps guides with lasting reference value:
- Architecture patterns and enforcement
- Design strategies (error handling, caching)
- Configuration guides (build profiles)
- Process documentation (release checklist, test organization)

Usage:
    python3 scripts/curate_guides.py [--dry-run] [--verbose] [--archive]

Options:
    --dry-run    Show what would be removed without deleting files
    --verbose    Show detailed information about each guide
    --archive    Move removed guides to docs/guides/archived/ instead of deleting
"""

import sys
import re
import argparse
from pathlib import Path
from typing import List, Dict, Set
from datetime import datetime


class GuideCurator:
    def __init__(self, project_root: Path, dry_run: bool = False,
                 verbose: bool = False, archive: bool = False):
        self.project_root = project_root
        self.dry_run = dry_run
        self.verbose = verbose
        self.archive = archive

        # Patterns for guides to remove (no lasting value)
        self.purge_patterns = [
            r'.*_COMPLETE\.md$',      # Milestone markers
            r'.*_ADDED\.md$',          # Feature completion markers
            r'.*_ISSUE\.md$',          # Solved issues
            r'.*_STATUS\.md$',         # Outdated status reports
            r'.*_PROGRESS\.md$',       # Progress tracking
            r'.*_GAPS\.md$',           # Gap analysis (becomes outdated)
            r'.*_QUESTION\.md$',       # Q&A that's no longer relevant
            r'^GPT.*\.md$',            # Chat logs
            r'.*_spec\.md$',           # Old specs (likely in formal docs now)
        ]

        # Patterns for guides to keep (lasting reference value)
        self.keep_patterns = [
            r'.*ARCHITECTURE.*\.md$',  # Architecture guides
            r'.*ERROR_HANDLING.*\.md$', # Design patterns
            r'.*PORTS.*\.md$',         # Architecture concepts
            r'.*BUILD.*\.md$',         # Build configuration
            r'.*RELEASE.*\.md$',       # Release processes
            r'.*TEST_ORGANIZATION.*\.md$', # Test structure
            r'.*CACHE.*\.md$',         # Design documentation
        ]

    def should_purge(self, filename: str) -> bool:
        """Determine if a guide should be purged based on patterns."""
        # First check if it matches a keep pattern
        for pattern in self.keep_patterns:
            if re.match(pattern, filename, re.IGNORECASE):
                return False

        # Then check if it matches a purge pattern
        for pattern in self.purge_patterns:
            if re.match(pattern, filename, re.IGNORECASE):
                return True

        return False

    def categorize_guides(self, guides_dir: Path) -> Dict[str, List[Path]]:
        """Categorize guides into keep vs purge."""
        if not guides_dir.exists():
            return {'keep': [], 'purge': []}

        keep = []
        purge = []

        for guide in sorted(guides_dir.glob("*.md")):
            # Skip index files
            if guide.name.lower() in ['index.md', 'readme.md']:
                keep.append(guide)
                continue

            if self.should_purge(guide.name):
                purge.append(guide)
            else:
                keep.append(guide)

        return {'keep': keep, 'purge': purge}

    def find_straggler_guides(self) -> List[Path]:
        """Find .md files outside of docs/ that might be guides."""
        stragglers = []

        # Search common locations for straggler guides
        search_paths = [
            self.project_root / "test",
            self.project_root / "src",
            self.project_root / "examples",
        ]

        for search_path in search_paths:
            if search_path.exists():
                for md_file in search_path.rglob("*.md"):
                    # Skip tool-specific READMEs (those are fine where they are)
                    if md_file.name.upper() == "README.MD":
                        continue
                    # This is likely a guide that should be in docs/guides/
                    stragglers.append(md_file)

        return stragglers

    def curate_guides(self):
        """Curate guides by removing dev milestones and temporary docs."""
        guides_dir = self.project_root / "docs" / "guides"

        if not guides_dir.exists():
            print(f"⚠️  No guides directory found at {guides_dir}")
            return

        print(f"🔍 Analyzing guides in {guides_dir.relative_to(self.project_root)}...")

        # Check for straggler guides
        stragglers = self.find_straggler_guides()
        if stragglers:
            print(f"\n⚠️  Found {len(stragglers)} straggler guide(s) outside docs/:")
            for straggler in stragglers:
                rel_path = straggler.relative_to(self.project_root)
                print(f"   - {rel_path}")
            print(f"   💡 Consider moving these to docs/guides/ or creating tool READMEs")

        # Categorize guides
        categorized = self.categorize_guides(guides_dir)
        keep_guides = categorized['keep']
        purge_guides = categorized['purge']

        print(f"\n📊 Analysis Results:")
        print(f"   - Total guides: {len(keep_guides) + len(purge_guides)}")
        print(f"   - Valuable guides to keep: {len(keep_guides)}")
        print(f"   - Dev milestones to purge: {len(purge_guides)}")

        if self.verbose:
            print(f"\n✅ Keeping {len(keep_guides)} guides with lasting value:")
            for guide in keep_guides:
                print(f"   - {guide.name}")

        if purge_guides:
            print(f"\n🗑️  Removing {len(purge_guides)} dev milestone guides:")
            for guide in purge_guides:
                print(f"   - {guide.name}")
                reason = self._get_purge_reason(guide.name)
                if self.verbose:
                    print(f"      Reason: {reason}")

        # Perform removal
        if not self.dry_run and purge_guides:
            if self.archive:
                archive_dir = guides_dir / "archived"
                archive_dir.mkdir(exist_ok=True)
                print(f"\n📦 Archiving to {archive_dir.relative_to(self.project_root)}...")

                for guide in purge_guides:
                    dest = archive_dir / guide.name
                    guide.rename(dest)
                    print(f"   ✓ Archived {guide.name}")
            else:
                print(f"\n🗑️  Deleting purged guides...")
                for guide in purge_guides:
                    guide.unlink()
                    print(f"   ✓ Deleted {guide.name}")

        # Summary
        print(f"\n{'─' * 70}")
        print(f"📊 Summary:")
        print(f"   - Guides kept: {len(keep_guides)}")
        print(f"   - Guides removed: {len(purge_guides)}")

        if self.dry_run and purge_guides:
            print(f"\n💡 Run without --dry-run to actually remove guides")
            if not self.archive:
                print(f"   Add --archive to move them instead of deleting")
        elif purge_guides:
            action = "archived" if self.archive else "deleted"
            print(f"\n✅ Guide curation complete - {len(purge_guides)} guides {action}")
        else:
            print(f"\n✅ All guides are valuable - nothing to remove")

    def _get_purge_reason(self, filename: str) -> str:
        """Get human-readable reason for purging a guide."""
        if re.match(r'.*_COMPLETE\.md$', filename, re.IGNORECASE):
            return "Completed milestone marker"
        if re.match(r'.*_ADDED\.md$', filename, re.IGNORECASE):
            return "Feature completion marker"
        if re.match(r'.*_ISSUE\.md$', filename, re.IGNORECASE):
            return "Solved issue documentation"
        if re.match(r'.*_(STATUS|PROGRESS|GAPS)\.md$', filename, re.IGNORECASE):
            return "Outdated status/progress report"
        if re.match(r'.*_QUESTION\.md$', filename, re.IGNORECASE):
            return "Obsolete Q&A"
        if re.match(r'^GPT.*\.md$', filename, re.IGNORECASE):
            return "Chat log/transcript"
        if re.match(r'.*_spec\.md$', filename, re.IGNORECASE):
            return "Obsolete spec (likely in formal docs now)"
        return "Development milestone"


def main():
    parser = argparse.ArgumentParser(
        description="Curate guides by removing dev milestones and temporary docs"
    )
    parser.add_argument(
        "--dry-run",
        action="store_true",
        help="Show what would be removed without deleting files"
    )
    parser.add_argument(
        "--verbose",
        action="store_true",
        help="Show detailed information about each guide"
    )
    parser.add_argument(
        "--archive",
        action="store_true",
        help="Move removed guides to docs/guides/archived/ instead of deleting"
    )

    args = parser.parse_args()

    # Project root (assumes script is in scripts/)
    project_root = Path(__file__).parent.parent

    curator = GuideCurator(project_root, args.dry_run, args.verbose, args.archive)
    curator.curate_guides()


if __name__ == '__main__':
    main()
