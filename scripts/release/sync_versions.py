#!/usr/bin/env python3
# ==============================================================================
# sync_versions.py - Synchronize versions across alire.toml files
# ==============================================================================
# Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
# SPDX-License-Identifier: BSD-3-Clause
# See LICENSE file in the project root.
# ==============================================================================
"""
Synchronize versions across all alire.toml files in the project.

This script reads the version from the root alire.toml file and updates
the version field in all layer alire.toml files to match.

This ensures a single source of truth for the project version while
maintaining separate Alire crates for each layer.

Usage:
    python3 tools/sync_versions.py [--dry-run] [--verbose]

Options:
    --dry-run    Show what would be changed without modifying files
    --verbose    Show detailed information about each file processed
"""

import sys
import re
import argparse
from pathlib import Path
from typing import List, Tuple


def find_alire_tomls(root: Path, exclude_root: bool = False) -> List[Path]:
    """Find all alire.toml files in the project"""
    tomls = []

    # Root alire.toml
    root_toml = root / "alire.toml"
    if root_toml.exists() and not exclude_root:
        tomls.append(root_toml)

    # Layer alire.toml files
    layer_dirs = [
        "application",
        "bootstrap",
        "domain",
        "infrastructure",
        "presentation",
        "shared",
    ]

    for layer in layer_dirs:
        toml_path = root / layer / "alire.toml"
        if toml_path.exists():
            tomls.append(toml_path)

    return sorted(tomls)


def extract_version(toml_path: Path) -> str:
    """Extract version string from alire.toml"""
    with open(toml_path, 'r') as f:
        for line in f:
            # Match: version = "0.1.0-dev"
            match = re.match(r'^\s*version\s*=\s*"([^"]+)"', line)
            if match:
                return match.group(1)
    raise ValueError(f"No version field found in {toml_path}")


def update_version(toml_path: Path, new_version: str, dry_run: bool = False) -> bool:
    """
    Update version in alire.toml file.

    Returns True if file was changed, False otherwise.
    """
    with open(toml_path, 'r') as f:
        lines = f.readlines()

    modified = False
    new_lines = []

    for line in lines:
        # Match version line
        match = re.match(r'^(\s*version\s*=\s*")([^"]+)(".*)', line)
        if match:
            prefix, old_version, suffix = match.groups()
            if old_version != new_version:
                new_line = f'{prefix}{new_version}{suffix}\n'
                new_lines.append(new_line)
                modified = True
            else:
                new_lines.append(line)
        else:
            new_lines.append(line)

    if modified and not dry_run:
        with open(toml_path, 'w') as f:
            f.writelines(new_lines)

    return modified


def main():
    parser = argparse.ArgumentParser(
        description="Set version across all alire.toml files"
    )
    parser.add_argument(
        "version",
        help="Version string to set in all alire.toml files (e.g., 0.1.0)"
    )
    parser.add_argument(
        "--dry-run",
        action="store_true",
        help="Show what would be changed without modifying files"
    )
    parser.add_argument(
        "--verbose",
        action="store_true",
        help="Show detailed information"
    )

    args = parser.parse_args()

    # Project root (assumes script is in scripts/)
    project_root = Path(__file__).parent.parent

    try:
        target_version = args.version
        print(f"📋 Target version: {target_version}")

        if args.dry_run:
            print("🔍 DRY RUN MODE - No files will be modified")

        # Find all alire.toml files (including root)
        toml_files = find_alire_tomls(project_root, exclude_root=False)

        if not toml_files:
            print("⚠️  No alire.toml files found")
            return

        print(f"\n📁 Found {len(toml_files)} alire.toml files:")
        for toml in toml_files:
            rel_path = toml.relative_to(project_root)
            print(f"   - {rel_path}")

        # Update each alire.toml
        print(f"\n🔄 Setting versions to {target_version}...\n")

        changed_count = 0
        unchanged_count = 0

        for toml_path in toml_files:
            rel_path = toml_path.relative_to(project_root)

            # Get current version
            try:
                current_version = extract_version(toml_path)
            except ValueError:
                print(f"⚠️  {rel_path}: No version field found, skipping")
                continue

            # Update version
            was_modified = update_version(toml_path, target_version, args.dry_run)

            if was_modified:
                status = "would be updated" if args.dry_run else "updated"
                print(f"✏️  {rel_path}: {current_version} → {target_version} ({status})")
                changed_count += 1
            else:
                if args.verbose:
                    print(f"✓  {rel_path}: already {target_version}")
                unchanged_count += 1

        # Summary
        print(f"\n{'─' * 60}")
        print(f"📊 Summary:")
        print(f"   - Files checked: {len(toml_files)}")
        print(f"   - Changed: {changed_count}")
        print(f"   - Unchanged: {unchanged_count}")

        if args.dry_run and changed_count > 0:
            print(f"\n💡 Run without --dry-run to apply changes")
        elif changed_count > 0:
            print(f"\n✅ All versions set to {target_version}")
        else:
            print(f"\n✅ All versions already set to {target_version}")

    except Exception as e:
        print(f"Error: {e}", file=sys.stderr)
        sys.exit(1)


if __name__ == '__main__':
    main()
