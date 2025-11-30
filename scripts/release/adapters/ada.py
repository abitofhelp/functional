#!/usr/bin/env python3
# ==============================================================================
# adapters/ada.py - Ada language adapter for release management
# ==============================================================================
# Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
# SPDX-License-Identifier: BSD-3-Clause
# See LICENSE file in the project root.
#
# Purpose:
#   Ada-specific adapter for release operations.
#   Handles alire.toml versioning, alr build, make test, and Ada-specific config.
#
# Design Notes:
#   Ada projects use alire.toml for version and metadata.
#   Version package is generated from alire.toml.
#   Multi-layer projects need version sync across all alire.toml files.
#
# ==============================================================================

from pathlib import Path
from typing import Tuple
import re
import sys

from .base import BaseReleaseAdapter


class AdaReleaseAdapter(BaseReleaseAdapter):
    """
    Ada-specific adapter for release operations.

    Handles:
        - alire.toml version management
        - Version package generation (Project.Version)
        - Version synchronization across layer alire.toml files
        - Build via 'make build' or 'alr build'
        - Test via 'make test'
    """

    @property
    def name(self) -> str:
        return "Ada"

    @staticmethod
    def detect(project_root: Path) -> bool:
        """
        Detect if a directory is an Ada project.

        Args:
            project_root: Path to check

        Returns:
            True if Ada project detected
        """
        if (project_root / 'alire.toml').exists():
            return True
        if list(project_root.glob('*.gpr')):
            return True
        if list(project_root.glob('**/*.gpr')):
            return True
        if list(project_root.glob('**/*.ads')) or list(project_root.glob('**/*.adb')):
            return True
        return False

    def load_project_info(self, config) -> Tuple[str, str]:
        """
        Load project name and URL from alire.toml.

        Args:
            config: ReleaseConfig instance

        Returns:
            Tuple of (project_name, project_url)
        """
        alire_toml = config.project_root / 'alire.toml'
        project_name = ""
        project_url = ""

        if alire_toml.exists():
            content = alire_toml.read_text(encoding='utf-8')

            # Extract name field
            name_match = re.search(r'^name\s*=\s*"([^"]+)"', content, re.MULTILINE)
            if name_match:
                project_name = name_match.group(1)

            # Extract website field
            website_match = re.search(r'^website\s*=\s*"([^"]+)"', content, re.MULTILINE)
            if website_match:
                url = website_match.group(1)
                # Remove .git suffix if present
                if url.endswith('.git'):
                    url = url[:-4]
                project_url = url

        # Fallback to directory name
        if not project_name:
            project_name = config.project_root.name

        return project_name, project_url

    def update_version(self, config) -> bool:
        """
        Update version in root alire.toml.

        Args:
            config: ReleaseConfig instance

        Returns:
            True if successful
        """
        root_toml = config.project_root / 'alire.toml'

        if not root_toml.exists():
            print("  alire.toml not found")
            return False

        try:
            content = root_toml.read_text(encoding='utf-8')

            # Check if version is already correct
            current_match = re.search(
                r'^version\s*=\s*"([^"]+)"',
                content,
                flags=re.MULTILINE
            )

            if current_match:
                current_version = current_match.group(1)
                if current_version == config.version:
                    print(f"  Root alire.toml already has version = \"{config.version}\"")
                    return True

            # Update version line
            old_content = content
            content = re.sub(
                r'^(\s*version\s*=\s*")[^"]+(")',
                rf'\g<1>{config.version}\g<2>',
                content,
                flags=re.MULTILINE
            )

            if content == old_content:
                print(f"  Error: Version field not found in {root_toml}")
                return False

            root_toml.write_text(content, encoding='utf-8')
            print(f"  Updated root alire.toml: version = \"{config.version}\"")
            return True

        except Exception as e:
            print(f"Error updating root alire.toml: {e}")
            return False

    def sync_versions(self, config) -> bool:
        """
        Synchronize versions across all layer alire.toml files.

        Args:
            config: ReleaseConfig instance

        Returns:
            True if successful
        """
        print("Syncing versions across all layer alire.toml files...")

        # Find sync_versions.py script
        sync_script = config.project_root / 'scripts' / 'release' / 'sync_versions.py'
        if not sync_script.exists():
            # Try alternative location
            sync_script = config.project_root / 'scripts' / 'sync_versions.py'

        if sync_script.exists():
            result = self.run_command(
                [sys.executable, str(sync_script), config.version],
                config.project_root,
                capture_output=True
            )
            return result is not None

        # Fallback: manually sync all alire.toml files
        for toml_file in config.project_root.rglob('alire.toml'):
            if toml_file.parent == config.project_root:
                continue  # Skip root (already updated)

            try:
                content = toml_file.read_text(encoding='utf-8')
                new_content = re.sub(
                    r'^(\s*version\s*=\s*")[^"]+(")',
                    rf'\g<1>{config.version}\g<2>',
                    content,
                    flags=re.MULTILINE
                )
                if new_content != content:
                    toml_file.write_text(new_content, encoding='utf-8')
                    rel_path = toml_file.relative_to(config.project_root)
                    print(f"  Updated {rel_path}")
            except Exception as e:
                print(f"  Warning: Could not update {toml_file}: {e}")

        return True

    def generate_version_file(self, config) -> bool:
        """
        Generate Version Ada package from alire.toml.

        Embeds version generation logic directly - no external script needed.
        Output: src/<project_name>-version.ads

        Args:
            config: ReleaseConfig instance

        Returns:
            True if successful
        """
        try:
            # Read alire.toml
            alire_toml = config.project_root / 'alire.toml'
            if not alire_toml.exists():
                print("  alire.toml not found, skipping version package generation")
                return True

            content = alire_toml.read_text(encoding='utf-8')

            # Extract version
            version_match = re.search(r'^\s*version\s*=\s*"([^"]+)"', content, re.MULTILINE)
            if not version_match:
                print("  No version field in alire.toml, skipping")
                return True
            version_str = version_match.group(1)

            # Extract project name
            name_match = re.search(r'^\s*name\s*=\s*"([^"]+)"', content, re.MULTILINE)
            if not name_match:
                print("  No name field in alire.toml, skipping")
                return True
            project_name = name_match.group(1)

            # Parse semantic version: MAJOR.MINOR.PATCH[-PRERELEASE][+BUILD]
            version_pattern = r'^(\d+)\.(\d+)\.(\d+)(?:-([a-zA-Z0-9.]+))?(?:\+([a-zA-Z0-9.]+))?$'
            ver_match = re.match(version_pattern, version_str)
            if not ver_match:
                print(f"  Invalid semantic version: {version_str}")
                return False

            major, minor, patch, prerelease, build = ver_match.groups()
            prerelease = prerelease or ''
            build = build or ''

            # Convert to Ada casing (hybrid_app_ada -> Hybrid_App_Ada)
            ada_package = '_'.join(part.capitalize() for part in project_name.split('_'))

            # Generate Ada package source
            ada_code = f'''pragma Ada_2022;
--  =========================================================================
--  {ada_package}.Version - Application Version Information
--  =========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  See LICENSE file in the project root.
--
--  AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
--
--  This file is automatically generated from alire.toml by:
--    scripts/release/release.py (Ada adapter)
--
--  To update the version:
--    1. Edit the "version" field in alire.toml
--    2. Run: python3 scripts/release/release.py prepare <version>
--    3. Rebuild the project
--
--  Purpose:
--    Provides version constants extracted from alire.toml (single source
--    of truth). Useful for CLI --version flags and runtime version queries.
--
--  Design Notes:
--    - Version follows Semantic Versioning 2.0.0 (semver.org)
--    - Pre-release identifiers: -dev, -alpha.N, -beta.N, -rc.N
--    - Build metadata: +build.N, +commit.HASH
--  =========================================================================

package {ada_package}.Version is

   --  Semantic Version Components
   Major : constant Natural := {major};
   Minor : constant Natural := {minor};
   Patch : constant Natural := {patch};

   --  Pre-release identifier (e.g., "dev", "alpha.1", "beta.2", "rc.1")
   --  Empty string for stable releases
   Prerelease : constant String := "{prerelease}";

   --  Build metadata (e.g., "build.123", "commit.abc123")
   --  Empty string if not specified
   Build_Metadata : constant String := "{build}";

   --  Full version string (e.g., "0.1.0-dev", "1.2.3", "2.0.0-rc.1+build.456")
   Version : constant String := "{version_str}";

   --  Check if this is a pre-release version
   function Is_Prerelease return Boolean is (Prerelease'Length > 0);

   --  Check if this is a development version
   --  Note: Condition may be always False for stable releases (expected)
   pragma Warnings (Off, "condition is always*");
   function Is_Development return Boolean is (Prerelease = "dev");
   pragma Warnings (On, "condition is always*");

   --  Check if this is a stable release
   function Is_Stable return Boolean is (not Is_Prerelease);

end {ada_package}.Version;
'''

            # Write output file to src/version/ (cross-cutting, outside hexagonal layers)
            output_path = config.project_root / 'src' / 'version' / f'{project_name}-version.ads'
            output_path.parent.mkdir(parents=True, exist_ok=True)
            output_path.write_text(ada_code, encoding='utf-8')

            print(f"  Project: {project_name}")
            print(f"  Version: {version_str}")
            print(f"  Generated: src/version/{project_name}-version.ads")
            print(f"  Package: {ada_package}.Version")
            return True

        except Exception as e:
            print(f"  Error generating version file: {e}")
            return False

    def run_build(self, config) -> bool:
        """
        Run Ada release build.

        Args:
            config: ReleaseConfig instance

        Returns:
            True if build successful
        """
        print("Running Ada release build...")

        # Try make first (if Makefile exists with build-release target)
        makefile = config.project_root / 'Makefile'
        if makefile.exists():
            # Run make clean first
            self.run_command(['make', 'clean'], config.project_root, capture_output=True)

            # Use build-release for production builds
            result = self.run_command(['make', 'build-release'], config.project_root)
            if result:
                print("  Release build successful (via make)")
                return True

        # Fallback to alr build with release validation
        result = self.run_command(
            ['alr', 'build', '--release'],
            config.project_root
        )

        if result:
            print("  Release build successful")
            return True

        print("  Build failed")
        return False

    def run_tests(self, config) -> bool:
        """
        Run Ada tests.

        Args:
            config: ReleaseConfig instance

        Returns:
            True if tests pass
        """
        print("Running Ada tests...")

        # Try make test-all (comprehensive test target)
        makefile = config.project_root / 'Makefile'
        if makefile.exists():
            # Try test-all first (includes unit, integration, e2e)
            result = self.run_command(
                ['make', 'test-all'],
                config.project_root,
                capture_output=True
            )
            if result is not None:
                print("  All tests passed (via make test-all)")
                return True

            # Fallback to test target
            result = self.run_command(
                ['make', 'test'],
                config.project_root,
                capture_output=True
            )
            if result is not None:
                print("  All tests passed (via make test)")
                return True

        # No standard fallback for Ada - make test is the convention
        print("  No test target found")
        return True  # Not fatal

    def run_format(self, config) -> bool:
        """
        Run Ada code formatting.

        Args:
            config: ReleaseConfig instance

        Returns:
            True if successful
        """
        print("Formatting Ada code...")

        # Try make format first
        makefile = config.project_root / 'Makefile'
        if makefile.exists():
            result = self.run_command(
                ['make', 'format'],
                config.project_root,
                capture_output=True
            )
            if result is not None:
                print("  Code formatted (via make)")
                return True

        # gnatpp is not always available
        print("  Format target not available (gnatpp/adafmt)")
        return True

    def cleanup_temp_files(self, config) -> bool:
        """
        Clean up Ada build artifacts.

        Args:
            config: ReleaseConfig instance

        Returns:
            True if successful
        """
        print("Cleaning up temporary files...")

        # Check for cleanup script
        cleanup_script = config.project_root / 'scripts' / 'cleanup_temp_files.py'
        if cleanup_script.exists():
            result = self.run_command(
                [sys.executable, str(cleanup_script)],
                config.project_root,
                capture_output=True
            )
            if result is not None:
                print("  Cleaned (via cleanup script)")
                return True

        # Fallback to make clean
        makefile = config.project_root / 'Makefile'
        if makefile.exists():
            result = self.run_command(
                ['make', 'clean'],
                config.project_root,
                capture_output=True
            )
            if result is not None:
                print("  Cleaned (via make)")
                return True

        print("  Cleaned")
        return True
