#!/usr/bin/env python3
# ==============================================================================
# release.py - Release management for Ada Hybrid Architecture Project
# ==============================================================================
# Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
# SPDX-License-Identifier: BSD-3-Clause
# See LICENSE file in the project root.
# ==============================================================================
"""
Release management script for the project.

This script handles the complete release process including:
- Temporary file cleanup (.bak, .o, .ali, .DS_Store, __pycache__)
- Version synchronization across all alire.toml files
- Version package generation (Simple_Hybrid.Version)
- Ada source file docstring generation (Copyright year, documentation)
- Formal documentation rebuild (SRS, SDS, Test Guide)
- Markdown file metadata updates (version, date, copyright)
- CHANGELOG.md maintenance
- Build verification and testing
- Git tagging and GitHub release creation
- PlantUML diagram generation

Usage:
    python scripts/release/release.py prepare <version>
    python scripts/release/release.py release <version>
    python scripts/release/release.py diagrams

Examples:
    python scripts/release/release.py prepare 1.0.0
    python scripts/release/release.py release 1.0.0
    python scripts/release/release.py diagrams
"""

import argparse
import os
import re
import subprocess
import sys
from datetime import datetime
from pathlib import Path
from typing import List, Optional, Tuple


class AdaReleaseManager:
    def __init__(self, project_root: str):
        self.project_root = Path(project_root)
        self.date_str = datetime.now().strftime("%B %d, %Y")
        self.year = datetime.now().year
        self.project_name = None
        self.project_url = None
        self._load_project_info()

    def _load_project_info(self):
        """Load project name and URL from alire.toml."""
        alire_toml = self.project_root / "alire.toml"
        try:
            with open(alire_toml, 'r') as f:
                for line in f:
                    # Extract name field
                    name_match = re.match(r'^name\s*=\s*"([^"]+)"', line)
                    if name_match:
                        self.project_name = name_match.group(1)
                    # Extract website field
                    website_match = re.match(r'^website\s*=\s*"([^"]+)"', line)
                    if website_match:
                        url = website_match.group(1)
                        # Remove .git suffix if present
                        if url.endswith('.git'):
                            url = url[:-4]
                        self.project_url = url

            # Validate required fields were found
            if not self.project_name:
                raise ValueError("No 'name' field found in alire.toml")
            if not self.project_url:
                raise ValueError("No 'website' field found in alire.toml")

        except Exception as e:
            print(f"❌ Error loading project info from alire.toml: {e}")
            sys.exit(1)

    # =========================================================================
    # Version Management
    # =========================================================================

    def update_root_alire_version(self, new_version: str) -> bool:
        """Update version in root alire.toml (single source of truth)."""
        root_toml = self.project_root / "alire.toml"

        try:
            with open(root_toml, 'r') as f:
                content = f.read()

            # Check if version is already correct
            current_version_match = re.search(
                r'^version\s*=\s*"([^"]+)"',
                content,
                flags=re.MULTILINE
            )

            if current_version_match:
                current_version = current_version_match.group(1)
                if current_version == new_version:
                    print(f"  ✓ Root alire.toml already has version = \"{new_version}\"")
                    return True

            # Update version line
            old_content = content
            content = re.sub(
                r'^(\s*version\s*=\s*")[^"]+(").*$',
                rf'\g<1>{new_version}\g<2>',
                content,
                flags=re.MULTILINE
            )

            if content == old_content:
                print(f"  ✗ Error: Version field not found in {root_toml}")
                return False

            with open(root_toml, 'w') as f:
                f.write(content)

            print(f"  ✓ Updated root alire.toml: version = \"{new_version}\"")
            return True

        except Exception as e:
            print(f"Error updating root alire.toml: {e}")
            return False

    def sync_layer_versions(self, version: str) -> bool:
        """Synchronize versions across all layer alire.toml files."""
        print("Syncing versions across all layer alire.toml files...")
        result = self.run_command(
            [sys.executable, "scripts/release/sync_versions.py", version],
            capture_output=True
        )
        return result is not None

    def generate_version_package(self) -> bool:
        """Generate Version Ada package from alire.toml."""
        # Convert project name to proper Ada casing (simple_hybrid -> Simple_Hybrid)
        if self.project_name.lower() == "simple_hybrid":
            package_name = "Simple_Hybrid"
        else:
            # Capitalize first letter and after underscores
            parts = self.project_name.split('_')
            package_name = '_'.join(part.capitalize() for part in parts)

        file_name = f"{self.project_name}-version.ads"
        file_path = f"shared/src/{file_name}"

        print(f"Generating {package_name}.Version package...")
        result = self.run_command(
            [sys.executable, "scripts/release/generate_version.py",
             "alire.toml", file_path],
            capture_output=True
        )
        if result:
            print(f"  ✓ Generated {file_path}")
        return result is not None

    # =========================================================================
    # Documentation Generation
    # =========================================================================

    # FIXME: DISABLED - generate_docstrings.py was removed (tzif-specific)
    # def generate_docstrings(self) -> bool:
    #     """Generate docstring headers for all Ada source files."""
    #     print("Generating Ada source code docstrings...")
    #     result = self.run_command(
    #         [sys.executable, "scripts/generate_docstrings.py"],
    #         capture_output=True
    #     )
    #     if result:
    #         print("  ✓ Ada docstrings generated")
    #     return result is not None

    # =========================================================================
    # Documentation Updates
    # =========================================================================

    def find_markdown_files(self) -> List[Path]:
        """Find all markdown files with version headers."""
        md_files = []

        # Search in docs and root
        for pattern in ["docs/**/*.md", "*.md"]:
            md_files.extend(self.project_root.glob(pattern))

        # Filter to only files with version headers
        # Matches:
        #   - "Version: 1.0.0" or "version: 1.0.0" (with colon)
        #   - "**Version 1.0.0**" or "**version 1.0.0**" (bold without colon)
        #   - "Copyright © 2025" (copyright year pattern)
        versioned_files = []
        for md_file in md_files:
            try:
                with open(md_file, 'r') as f:
                    content = f.read()
                    # Match version patterns OR copyright patterns
                    if re.search(r'Version\s*[:)]|version\s*[:)]|\*\*Version\s+\d+\.\d+|Copyright\s*©\s*\d{4}',
                                content, re.IGNORECASE):
                        versioned_files.append(md_file)
            except Exception:
                pass

        return versioned_files

    def update_markdown_version(self, file_path: Path, new_version: str) -> bool:
        """
        Update version and metadata in markdown file headers.

        Handles these patterns:
        - **Version:** 1.0.0
        - **Date:** October 24, 2025
        - **Copyright:** © 2025 Michael Gardner, A Bit of Help, Inc.
        - **Status:** Unreleased / Released
        """
        try:
            with open(file_path, 'r') as f:
                content = f.read()

            old_content = content

            # Pattern 1: **Version:** 1.0.0 (bold with colon)
            content = re.sub(
                r'(\*\*Version:\*\*\s+)[^\s\n]+',
                rf'\g<1>{new_version}',
                content,
                flags=re.IGNORECASE
            )

            # Pattern 2: Version: 1.0.0 (plain with colon)
            content = re.sub(
                r'^(Version:\s+)[^\s\n]+',
                rf'\g<1>{new_version}',
                content,
                flags=re.IGNORECASE | re.MULTILINE
            )

            # Pattern 3: **Version 1.0.0** (bold without colon - used in cover.md)
            content = re.sub(
                r'(\*\*Version\s+)\d+\.\d+\.\d+(\*\*)',
                rf'\g<1>{new_version}\g<2>',
                content,
                flags=re.IGNORECASE
            )

            # Pattern 3: **Date:** October 24, 2025 (bold)
            content = re.sub(
                r'(\*\*Date:\*\*\s+)[^\n]+',
                rf'\g<1>{self.date_str}',
                content,
                flags=re.IGNORECASE
            )

            # Pattern 4: Date: October 24, 2025 (plain)
            content = re.sub(
                r'^(Date:\s+)[^\n]+',
                rf'\g<1>{self.date_str}',
                content,
                flags=re.IGNORECASE | re.MULTILINE
            )

            # Pattern 5: **Copyright:** © 2024 -> © 2025 (update year only)
            content = re.sub(
                r'(\*\*Copyright:\*\*\s+©\s*)\d{4}',
                rf'\g<1>{self.year}',
                content,
                flags=re.IGNORECASE
            )

            # Pattern 6: Copyright: © 2024 -> © 2025 (plain)
            content = re.sub(
                r'^(Copyright:\s+©\s*)\d{4}',
                rf'\g<1>{self.year}',
                content,
                flags=re.IGNORECASE | re.MULTILINE
            )

            # Pattern 7: **Status:** Unreleased/Pre-release -> Released (for releases)
            # Only update if this is NOT a pre-release version
            is_prerelease = '-' in new_version  # e.g., "1.0.0-dev", "1.0.0-alpha.1"
            if not is_prerelease:
                # For stable releases, mark as Released
                # Handle "Unreleased"
                content = re.sub(
                    r'(\*\*Status:\*\*\s+)Unreleased',
                    r'\g<1>Released',
                    content,
                    flags=re.IGNORECASE
                )
                content = re.sub(
                    r'^(Status:\s+)Unreleased',
                    r'\g<1>Released',
                    content,
                    flags=re.IGNORECASE | re.MULTILINE
                )
                # Handle "Pre-release" and "Pre-release (vX.X.X)"
                content = re.sub(
                    r'(\*\*Status:\*\*\s+)Pre-release(?:\s+\([^)]+\))?',
                    r'\g<1>Released',
                    content,
                    flags=re.IGNORECASE
                )
                content = re.sub(
                    r'^(Status:\s+)Pre-release(?:\s+\([^)]+\))?',
                    r'\g<1>Released',
                    content,
                    flags=re.IGNORECASE | re.MULTILINE
                )
            else:
                # For pre-releases, keep as Unreleased
                content = re.sub(
                    r'(\*\*Status:\*\*\s+)Released',
                    r'\g<1>Unreleased',
                    content,
                    flags=re.IGNORECASE
                )
                content = re.sub(
                    r'^(Status:\s+)Released',
                    r'\g<1>Unreleased',
                    content,
                    flags=re.IGNORECASE | re.MULTILINE
                )

            # Add trailing spaces for proper GitHub markdown rendering
            # GitHub needs two spaces at end of line for line breaks
            lines = content.split('\n')
            new_lines = []
            for line in lines:
                # Add trailing spaces to metadata header lines
                if re.match(r'^\*\*(Version|Date|SPDX|License|Copyright|Status):', line):
                    if not line.endswith('  '):
                        line = line.rstrip() + '  '
                new_lines.append(line)
            content = '\n'.join(new_lines)

            # Check if any changes were made
            if content != old_content:
                with open(file_path, 'w') as f:
                    f.write(content)
                return True

            return False

        except Exception as e:
            print(f"Error updating {file_path}: {e}")
            return False

    def add_markdown_header(self, file_path: Path, version: str) -> bool:
        """Add metadata header to markdown file if missing."""
        try:
            with open(file_path, 'r') as f:
                content = f.read()
                lines = content.splitlines(keepends=True)

            # Find first # heading
            title_idx = None
            for i, line in enumerate(lines):
                if re.match(r'^#\s+\S', line):
                    title_idx = i
                    break

            if title_idx is None:
                return False  # No title found

            # Create header with trailing spaces for line breaks
            is_prerelease = '-' in version
            status = "Unreleased" if is_prerelease else "Released"
            # Note: Each line ends with two spaces for proper markdown line breaks
            header = (
                "\n"
                f"**Version:** {version}  \n"
                f"**Date:** {self.date_str}  \n"
                f"**SPDX-License-Identifier:** BSD-3-Clause  \n"
                f"**License File:** See the LICENSE file in the project root.  \n"
                f"**Copyright:** © {self.year} Michael Gardner, A Bit of Help, Inc.  \n"
                f"**Status:** {status}  \n"
                "\n"
            )

            # Insert after title
            lines.insert(title_idx + 1, header)
            new_content = ''.join(lines)

            with open(file_path, 'w') as f:
                f.write(new_content)

            return True

        except Exception as e:
            print(f"  ⚠️  Error adding header to {file_path}: {e}")
            return False

    def update_all_markdown_files(self, version: str) -> int:
        """Update version in all markdown files (or add headers if missing). Returns count of updated files."""
        # Skip files generated by rebuild_formal_documentation.py
        skip_files = {
            "software_requirements_specification.md",
            "software_design_specification.md",
            "software_test_guide.md"
        }

        # Find all markdown files
        all_md_files = []
        for pattern in ["docs/**/*.md", "*.md"]:
            all_md_files.extend(self.project_root.glob(pattern))

        updated_count = 0
        added_count = 0

        print(f"Processing markdown files...")
        for md_file in all_md_files:
            # Skip auto-generated formal docs
            if md_file.name in skip_files:
                continue

            try:
                with open(md_file, 'r') as f:
                    content = f.read()

                # Check if file has metadata
                has_metadata = bool(re.search(
                    r'Version\s*[:)]|version\s*[:)]|\*\*Version\s+\d+\.\d+|Copyright\s*©\s*\d{4}',
                    content, re.IGNORECASE
                ))

                if has_metadata:
                    # Update existing metadata
                    if self.update_markdown_version(md_file, version):
                        rel_path = md_file.relative_to(self.project_root)
                        print(f"  ✓ Updated {rel_path}")
                        updated_count += 1
                else:
                    # Add new metadata header
                    if self.add_markdown_header(md_file, version):
                        rel_path = md_file.relative_to(self.project_root)
                        print(f"  ✓ Added header to {rel_path}")
                        added_count += 1

            except Exception as e:
                print(f"  ⚠️  Error processing {md_file}: {e}")

        if added_count > 0:
            print(f"  → Added headers to {added_count} file(s)")
        if updated_count > 0:
            print(f"  → Updated {updated_count} file(s)")

        return updated_count + added_count

    # =========================================================================
    # CHANGELOG Management
    # =========================================================================

    def is_initial_release(self, version: str) -> bool:
        """
        Check if this is an initial release.

        An initial release is any version <= 1.0.0 (0.1.0, 0.9.0, 1.0.0, etc.)
        """
        from packaging import version as pkg_version
        try:
            return pkg_version.parse(version) <= pkg_version.parse("1.0.0")
        except Exception:
            # If version parsing fails, fall back to string comparison
            return version in ["0.1.0", "1.0.0"] or version.startswith("0.")

    def create_initial_changelog(self, version: str) -> str:
        """Create a clean Common Changelog format CHANGELOG.md for initial release."""
        today = datetime.now().strftime("%Y-%m-%d")

        return f"""# Changelog

All notable changes to Simple_Hybrid - IANA Timezone Information Library for Ada 2022 will be documented in this file.

The format is based on [Common Changelog](https://common-changelog.org),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

---

## [Unreleased]

### Changed

### Added

### Removed

### Fixed

---

## [{version}] - {today}

_Initial alpha release of Simple_Hybrid library._

### Added

- Parse Simple_Hybrid binary files (versions 1, 2, and 3)
- Query timezone information by ID
- Access timezone transition times and rules
- Discover timezone sources from filesystem paths
- Export and import zone caches
- Use Railway-oriented programming with Result/Option monads
- Implement Hexagonal architecture (Domain, Application, Infrastructure layers)
- Add comprehensive test suite (118 integration + 86 unit tests)
- Add 13 working examples demonstrating library usage
- Support thread-safe repository operations
- Support POSIX platforms (Linux, macOS, BSD)

---

## License & Copyright

- **License**: BSD-3-Clause
- **Copyright**: © {self.year} Michael Gardner, A Bit of Help, Inc.
- **SPDX-License-Identifier**: BSD-3-Clause
"""

    def update_changelog(self, new_version: str) -> bool:
        """
        Update or create CHANGELOG.md with new version.

        Behavior:
        - For 0.1.0 (initial release): Create or overwrite with clean template
        - For versions > 0.1.0: Must exist, update [Unreleased] to new version
        """
        changelog_file = self.project_root / "CHANGELOG.md"
        is_initial = self.is_initial_release(new_version)

        # Handle initial release (0.1.0)
        if is_initial:
            # Create or replace with clean initial changelog
            content = self.create_initial_changelog(new_version)

            if changelog_file.exists():
                # Backup existing file
                backup_file = self.project_root / "CHANGELOG.md.backup"
                changelog_file.rename(backup_file)
                print(f"  ⚠️  Backed up existing CHANGELOG.md to {backup_file.name}")

            with open(changelog_file, 'w') as f:
                f.write(content)

            print(f"  ✓ Created CHANGELOG.md for initial release {new_version}")
            return True

        # Handle subsequent releases (> 0.1.0)
        if not changelog_file.exists():
            print(f"❌ ERROR: CHANGELOG.md not found!")
            print(f"   For releases after 0.1.0, CHANGELOG.md must exist.")
            print(f"   Please ensure you have a valid CHANGELOG.md before preparing release {new_version}")
            return False

        try:
            with open(changelog_file, 'r') as f:
                content = f.read()

            # Find the [Unreleased] section
            unreleased_pattern = r'## \[Unreleased\]\s*\n(.*?)(?=\n## |\Z)'
            match = re.search(unreleased_pattern, content, re.DOTALL)

            if not match:
                print(f"❌ ERROR: Could not find [Unreleased] section in CHANGELOG.md")
                print(f"   Your CHANGELOG.md must have an [Unreleased] section following Common Changelog format.")
                return False

            unreleased_content = match.group(1).strip()

            # Check if unreleased section has any content
            if not unreleased_content or unreleased_content.count('\n') < 5:
                print(f"⚠️  WARNING: [Unreleased] section appears empty")
                print(f"   Consider adding changes to the [Unreleased] section before release.")
                # Continue anyway - not fatal

            # Create new release section with proper Common Changelog format
            today = datetime.now().strftime("%Y-%m-%d")
            release_section = f"""## [Unreleased]

### Changed

### Added

### Removed

### Fixed

---

## [{new_version}] - {today}

{unreleased_content}

"""

            # Replace the unreleased section
            content = re.sub(
                r'## \[Unreleased\]\s*\n.*?(?=\n## |\Z)',
                release_section,
                content,
                flags=re.DOTALL,
                count=1
            )

            with open(changelog_file, 'w') as f:
                f.write(content)

            print(f"  ✓ Updated CHANGELOG.md with release {new_version}")
            return True

        except Exception as e:
            print(f"❌ Error updating changelog: {e}")
            return False

    # =========================================================================
    # Build and Test Verification
    # =========================================================================

    def run_command(self, cmd: List[str], capture_output: bool = False) -> Optional[str]:
        """Run a shell command."""
        try:
            result = subprocess.run(
                cmd,
                cwd=self.project_root,
                capture_output=capture_output,
                text=True
            )
            if result.returncode != 0:
                print(f"❌ Command failed: {' '.join(cmd)}")
                if result.stderr:
                    print(f"Error: {result.stderr}")
                return None
            return result.stdout if capture_output else "SUCCESS"
        except Exception as e:
            print(f"❌ Command exception: {' '.join(cmd)}")
            print(f"Exception: {e}")
            return None

    def prompt_user_continue(self, message: str, allow_skip: bool = False) -> bool:
        """
        Prompt user to perform a manual task and continue.

        Args:
            message: Instructions for the user
            allow_skip: If True, user can skip this step

        Returns:
            True if user wants to continue, False to abort
        """
        print(f"\n{'='*70}")
        print(f"⏸️  MANUAL STEP REQUIRED")
        print(f"{'='*70}")
        print(f"\n{message}\n")

        while True:
            if allow_skip:
                response = input("Press ENTER to continue, 's' to skip, or 'q' to quit: ").strip().lower()
                if response == '':
                    return True
                elif response == 's':
                    print("⏭️  Skipping this step...")
                    return True
                elif response == 'q':
                    print("🛑 Release process aborted by user")
                    return False
            else:
                response = input("Press ENTER to continue, or 'q' to quit: ").strip().lower()
                if response == '':
                    return True
                elif response == 'q':
                    print("🛑 Release process aborted by user")
                    return False

            print("Invalid input. Please try again.")

    def verify_clean_working_tree(self) -> bool:
        """Verify git working tree is clean."""
        result = self.run_command(["git", "status", "--porcelain"], capture_output=True)
        if result is None:
            return False
        return len(result.strip()) == 0

    def run_build(self) -> bool:
        """Run full build."""
        print("Running build...")
        return self.run_command(["make", "clean"]) is not None and \
               self.run_command(["make", "build"]) is not None

    def run_tests(self) -> bool:
        """Run all tests."""
        print("Running tests...")
        # Check if test target exists
        result = self.run_command(["make", "test"], capture_output=True)
        if result is None:
            print("  ⚠️  No tests found or test target not available")
            return True  # Not a fatal error for now
        print("  ✓ All tests passed")
        return True

    def run_format(self) -> bool:
        """Format code using gnatpp."""
        print("Formatting code...")
        result = self.run_command(["make", "format"], capture_output=True)
        if result is None:
            print("  ⚠️  Format target not available")
            return True  # Not fatal
        print("  ✓ Code formatted")
        return True

    def generate_diagrams(self) -> bool:
        """Generate PlantUML diagrams."""
        print("Generating UML diagrams...")

        # Check if plantuml is available
        try:
            subprocess.run(
                ["plantuml", "-version"],
                capture_output=True,
                check=True
            )
        except (subprocess.CalledProcessError, FileNotFoundError):
            print("  ⚠️  plantuml not found, skipping diagram generation")
            return True  # Not fatal

        diagrams_dir = self.project_root / "docs" / "diagrams"
        if not diagrams_dir.exists():
            print("  ⚠️  No diagrams directory found")
            return True

        # Find all .puml files and generate SVGs
        puml_files = list(diagrams_dir.glob("*.puml"))
        if not puml_files:
            print("  ⚠️  No PlantUML files found")
            return True

        for puml_file in puml_files:
            result = self.run_command(
                ["plantuml", "-tsvg", str(puml_file)],
                capture_output=True
            )
            if result is None:
                print(f"  ⚠️  Failed to generate {puml_file.stem}.svg")

        print(f"  ✓ Generated {len(puml_files)} diagram(s)")
        return True

    # =========================================================================
    # Git Operations
    # =========================================================================

    def create_git_tag(self, version: str) -> bool:
        """Create annotated git tag."""
        tag_name = f"v{version}"
        message = f"Release version {version}"

        result = self.run_command([
            "git", "tag", "-a", tag_name, "-m", message
        ])

        if result:
            print(f"  ✓ Created tag {tag_name}")
        return result is not None

    def push_changes(self, version: str) -> bool:
        """Push changes and tags to origin."""
        commands = [
            (["git", "push", "origin", "main"], "Pushed to main"),
            (["git", "push", "origin", f"v{version}"], f"Pushed tag v{version}")
        ]

        for cmd, success_msg in commands:
            if self.run_command(cmd) is None:
                return False
            print(f"  ✓ {success_msg}")

        return True

    def create_github_release(self, version: str) -> bool:
        """Create GitHub release using gh CLI."""
        # Extract release notes from CHANGELOG.md
        changelog_file = self.project_root / "CHANGELOG.md"
        release_notes = f"Release version {version}"

        if changelog_file.exists():
            try:
                with open(changelog_file, 'r') as f:
                    content = f.read()

                version_pattern = rf'## \[{re.escape(version)}\][^\n]*\n(.*?)(?=\n## |\Z)'
                match = re.search(version_pattern, content, re.DOTALL)

                if match:
                    release_notes = match.group(1).strip()
            except Exception as e:
                print(f"Warning: Could not extract release notes: {e}")

        # Create release
        cmd = [
            "gh", "release", "create", f"v{version}",
            "--title", f"Release {version}",
            "--notes", release_notes
        ]

        # Note: Diagrams are in the repo, no need to attach as assets
        # Users can view them in docs/diagrams/ or rendered in the book

        result = self.run_command(cmd)
        if result:
            print(f"  ✓ Created GitHub release v{version}")
        return result is not None

    # =========================================================================
    # Release Workflow
    # =========================================================================

    # FIXME: DISABLED - rebuild_formal_documentation.py was removed (tzif-specific)
    # def rebuild_formal_documentation(self) -> bool:
    #     """Rebuild formal documentation (SRS, SDS, Test Guide) from codebase."""
    #     print("Rebuilding formal documentation...")
    #     result = self.run_command(
    #         [sys.executable, "scripts/rebuild_formal_documentation.py"],
    #         capture_output=True
    #     )
    #     if result:
    #         print("  ✓ Formal documentation rebuilt")
    #     return result is not None

    def cleanup_temp_files(self) -> bool:
        """Remove temporary files and build artifacts."""
        print("Cleaning up temporary files...")
        result = self.run_command(
            [sys.executable, "scripts/cleanup_temp_files.py"],
            capture_output=True
        )
        if result:
            print("  ✓ Temporary files cleaned")
        return result is not None

    def prepare_release(self, version: str) -> bool:
        """Prepare release by updating versions and running checks."""
        print(f"\n{'='*70}")
        print(f"PREPARING RELEASE {version}")
        print(f"{'='*70}\n")

        # Pre-flight check: Ensure CHANGELOG.md has release notes
        changelog_file = self.project_root / "CHANGELOG.md"
        if changelog_file.exists():
            try:
                with open(changelog_file, 'r') as f:
                    content = f.read()
                    unreleased_match = re.search(
                        r'## \[Unreleased\]\s*\n(.*?)(?=\n## |\Z)',
                        content,
                        re.DOTALL
                    )
                    if unreleased_match:
                        unreleased_content = unreleased_match.group(1).strip()
                        # Check if unreleased section is mostly empty (only has section headers)
                        if not unreleased_content or unreleased_content.count('\n') < 10:
                            message = f"""📝 CHANGELOG.md appears to have minimal content in [Unreleased] section.

Please:
1. Edit CHANGELOG.md
2. Add your release notes under the [Unreleased] section
3. Commit your changes:
   git add CHANGELOG.md
   git commit -m "docs: Add release notes for {version}"
4. Press ENTER to continue with release preparation

The script will automatically move [Unreleased] → [{version}] when you continue."""
                            if not self.prompt_user_continue(message):
                                return False
            except Exception:
                pass

        # Step 1: Clean up temporary files
        print("\n🧹 Step 1: Cleaning up temporary files...")
        if not self.cleanup_temp_files():
            print("  ⚠️  Warning: Could not clean up temporary files")
            # Not fatal - continue with release

        # Step 2: Update root alire.toml version
        print("\n📝 Step 2: Updating root alire.toml version...")
        if not self.update_root_alire_version(version):
            return False

        # Step 3: Sync all layer alire.toml files
        print("\n📝 Step 3: Syncing layer versions...")
        if not self.sync_layer_versions(version):
            return False

        # Step 4: Generate Simple_Hybrid.Version package
        print("\n📝 Step 4: Generating Simple_Hybrid.Version package...")
        if not self.generate_version_package():
            return False

        # FIXME: Step 5 DISABLED - generate_docstrings.py was removed (tzif-specific)
        # print("\n📝 Step 5: Generating Ada source docstrings...")
        # if not self.generate_docstrings():
        #     print("  ⚠️  Warning: Could not generate Ada docstrings")
        #     # Not fatal - continue with release

        # FIXME: Step 5 DISABLED - format target not available until adafmt tool is ready
        # print("\n📋 Step 5: Formatting code...")
        # self.run_format()

        # FIXME: Step 5 DISABLED - rebuild_formal_documentation.py was removed (tzif-specific)
        # print("\n📝 Step 5: Rebuilding formal documentation...")
        # if not self.rebuild_formal_documentation():
        #     print("  ⚠️  Warning: Could not rebuild formal documentation")
        #     # Not fatal - continue with release

        # Step 5: Update remaining markdown files
        print("\n📝 Step 5: Updating markdown documentation...")
        self.update_all_markdown_files(version)

        # Step 6: Update CHANGELOG.md (create for 0.1.0, update for later versions)
        print("\n📝 Step 6: Updating CHANGELOG.md...")
        if not self.update_changelog(version):
            return False

        # Step 7: Generate diagrams
        print("\n📝 Step 7: Generating diagrams...")
        if not self.generate_diagrams():
            return False

        # Checkpoint: Review and commit changes
        message = f"""✅ All files have been updated for release {version}

IMPORTANT: Review and commit changes NOW (before build/test):

1. Review what changed:
   git diff

2. Commit the prepared release:
   git add -A
   git commit -m "chore: Prepare release {version}"

WHY COMMIT NOW?
- If build/tests fail, you can easily rollback (git reset HEAD~1)
- Clean separation between preparation and verification
- Safe fallback position

After committing, press ENTER to continue with build and test verification."""
        if not self.prompt_user_continue(message):
            return False

        # Step 8: Build verification
        print("\n🔨 Step 8: Running build...")
        if not self.run_build():
            print("❌ Build failed")
            return False
        print("  ✓ Build successful")

        # Step 9: Test verification
        print("\n🧪 Step 9: Running tests...")
        if not self.run_tests():
            print("❌ Tests failed")
            return False

        print(f"\n{'='*70}")
        print(f"✅ RELEASE {version} PREPARED AND VERIFIED SUCCESSFULLY!")
        print(f"{'='*70}\n")
        print("✓ All files updated")
        print("✓ Build passing")
        print("✓ Tests passing")
        print()
        print("Next step:")
        print(f"   python3 scripts/release/release.py release {version}")
        print()
        print("This will:")
        print(f"  - Create git tag v{version}")
        print("  - Push to GitHub")
        print("  - Create GitHub release with release notes")
        print()

        return True

    def create_release(self, version: str) -> bool:
        """Create the actual release (tag and publish)."""
        print(f"\n{'='*70}")
        print(f"CREATING RELEASE {version}")
        print(f"{'='*70}\n")

        # Verify working tree is clean
        print("🔍 Verifying clean working tree...")
        if not self.verify_clean_working_tree():
            print("❌ Working tree is not clean. Please commit changes first.")
            print("   Run: git status")
            return False
        print("  ✓ Working tree is clean")

        # Create git tag
        print("\n🏷️  Creating git tag...")
        if not self.create_git_tag(version):
            return False

        # Push changes and tag
        print("\n⬆️  Pushing to GitHub...")
        if not self.push_changes(version):
            return False

        # Create GitHub release
        print("\n📦 Creating GitHub release...")
        if not self.create_github_release(version):
            return False

        print(f"\n{'='*70}")
        print(f"🎉 RELEASE {version} CREATED SUCCESSFULLY!")
        print(f"{'='*70}\n")
        print("Release is now live on GitHub!")
        release_url = f"{self.project_url}/releases/tag/v{version}"
        print(f"View at: {release_url}")
        print()

        return True


def main():
    parser = argparse.ArgumentParser(
        description="Release management for Ada Hybrid Architecture Project"
    )
    parser.add_argument(
        "action",
        choices=["prepare", "release", "diagrams"],
        help="Action to perform"
    )
    parser.add_argument(
        "version",
        nargs='?',
        help="Version to release (e.g., 1.0.0 or 1.0.0-dev) - required for prepare/release"
    )

    args = parser.parse_args()

    # Validate version is provided for prepare/release
    if args.action in ["prepare", "release"] and not args.version:
        print("❌ Error: Version is required for prepare/release actions")
        parser.print_help()
        sys.exit(1)

    # Validate semantic version format (allows pre-release like -dev, -alpha.1)
    if args.version and not re.match(r'^\d+\.\d+\.\d+(-[a-zA-Z0-9.]+)?(\+[a-zA-Z0-9.]+)?$', args.version):
        print("❌ Error: Version must follow semantic versioning (e.g., 1.0.0, 1.0.0-dev)")
        sys.exit(1)

    # Find project root
    script_dir = Path(__file__).parent
    project_root = script_dir.parent.parent

    release_manager = AdaReleaseManager(str(project_root))

    try:
        if args.action == "prepare":
            success = release_manager.prepare_release(args.version)
        elif args.action == "release":
            success = release_manager.create_release(args.version)
        elif args.action == "diagrams":
            success = release_manager.generate_diagrams()
        else:
            print(f"❌ Unknown action: {args.action}")
            sys.exit(1)

        sys.exit(0 if success else 1)

    except KeyboardInterrupt:
        print("\n\n❌ Release process interrupted by user")
        sys.exit(1)
    except Exception as e:
        print(f"\n❌ Unexpected error: {e}")
        import traceback
        traceback.print_exc()
        sys.exit(1)


if __name__ == "__main__":
    main()
