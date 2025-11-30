#!/usr/bin/env python3
# ==============================================================================
# adapters/go.py - Go language adapter for release management
# ==============================================================================
# Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
# SPDX-License-Identifier: BSD-3-Clause
# See LICENSE file in the project root.
#
# Purpose:
#   Go-specific adapter for release operations.
#   Handles go.mod versioning, go build, go test, and Go-specific config.
#
# Design Notes:
#   Go projects use go.mod for module information.
#   Version package is generated from release version argument.
#   Tests use 'go test ./...' for all packages.
#
# ==============================================================================

from pathlib import Path
from typing import Tuple
import re

from .base import BaseReleaseAdapter


class GoReleaseAdapter(BaseReleaseAdapter):
    """
    Go-specific adapter for release operations.

    Handles:
        - go.mod module information extraction
        - Version package generation:
          - Apps: internal/version/version.go (unexported)
          - Libraries: version/version.go (exported for consumers)
        - Build via 'make build' or 'go build'
        - Test via 'make test' or 'go test'
    """

    @property
    def name(self) -> str:
        return "Go"

    @staticmethod
    def detect(project_root: Path) -> bool:
        """
        Detect if a directory is a Go project.

        Args:
            project_root: Path to check

        Returns:
            True if Go project detected
        """
        if (project_root / 'go.mod').exists():
            return True
        if (project_root / 'go.work').exists():
            return True
        if list(project_root.glob('**/*.go')):
            return True
        return False

    def load_project_info(self, config) -> Tuple[str, str]:
        """
        Load project name and URL from go.mod.

        Args:
            config: ReleaseConfig instance

        Returns:
            Tuple of (project_name, project_url)
        """
        go_mod = config.project_root / 'go.mod'
        project_name = ""
        project_url = ""

        if go_mod.exists():
            content = go_mod.read_text(encoding='utf-8')

            # Extract module path (e.g., github.com/user/project)
            match = re.search(r'^module\s+(\S+)', content, re.MULTILINE)
            if match:
                module_path = match.group(1)
                # Extract project name from last path component
                project_name = module_path.split('/')[-1]
                # Convert module path to HTTPS URL
                if module_path.startswith('github.com/'):
                    project_url = f"https://{module_path}"

        # Fallback to directory name
        if not project_name:
            project_name = config.project_root.name

        return project_name, project_url

    def update_version(self, config) -> bool:
        """
        Update version for Go project.

        Go doesn't have version in go.mod, so we generate a version package.

        Args:
            config: ReleaseConfig instance

        Returns:
            True if successful
        """
        # Go projects use generated version package instead of ldflags
        # This provides a single source of truth for both apps and libraries
        print(f"  Version will be set in generated version package")
        return True

    def _is_library_project(self, config) -> bool:
        """
        Detect if project is a library (no main package) vs an application.

        Libraries export version in version/ for consumer access.
        Applications keep version internal in internal/version/.

        Args:
            config: ReleaseConfig instance

        Returns:
            True if library, False if application
        """
        # Check for cmd/ directory (common app structure)
        if (config.project_root / 'cmd').exists():
            return False

        # Check for main.go in root
        if (config.project_root / 'main.go').exists():
            return False

        # Check for bin/ directory with executables expected
        if (config.project_root / 'bin').exists():
            # Check if Makefile builds to bin/
            makefile = config.project_root / 'Makefile'
            if makefile.exists():
                content = makefile.read_text(encoding='utf-8')
                if 'bin/' in content and 'go build' in content:
                    return False

        # Check project name heuristics
        project_name = config.project_root.name.lower()
        if project_name.endswith('_app') or project_name.startswith('app_'):
            return False
        if project_name.endswith('_lib') or project_name.startswith('lib_'):
            return True

        # Check module name for 'lib' indicator
        go_mod = config.project_root / 'go.mod'
        if go_mod.exists():
            content = go_mod.read_text(encoding='utf-8')
            match = re.search(r'^module\s+(\S+)', content, re.MULTILINE)
            if match:
                module_name = match.group(1).lower()
                if '_lib' in module_name or '/lib' in module_name:
                    return True

        # Default to application (internal version)
        return False

    def generate_version_file(self, config) -> bool:
        """
        Generate Version Go package from release version.

        Embeds version generation logic directly - no external script needed.
        Output path depends on project type:
          - Applications: internal/version/version.go (unexported)
          - Libraries: version/version.go (exported for consumers)

        Args:
            config: ReleaseConfig instance

        Returns:
            True if successful
        """
        try:
            version_str = config.version
            if not version_str:
                print("  No version specified, skipping version package generation")
                return True

            # Parse semantic version: MAJOR.MINOR.PATCH[-PRERELEASE][+BUILD]
            version_pattern = r'^(\d+)\.(\d+)\.(\d+)(?:-([a-zA-Z0-9.]+))?(?:\+([a-zA-Z0-9.]+))?$'
            ver_match = re.match(version_pattern, version_str)
            if not ver_match:
                print(f"  Invalid semantic version: {version_str}")
                return False

            major, minor, patch, prerelease, build = ver_match.groups()
            prerelease = prerelease or ''
            build = build or ''

            # Determine if library or application
            is_library = self._is_library_project(config)
            project_type = "library" if is_library else "application"

            # Get module path from go.mod for proper import path
            go_mod = config.project_root / 'go.mod'
            module_path = ""
            if go_mod.exists():
                content = go_mod.read_text(encoding='utf-8')
                match = re.search(r'^module\s+(\S+)', content, re.MULTILINE)
                if match:
                    module_path = match.group(1)

            # Generate Go package source
            go_code = f'''// Package version provides version information for the {project_type}.
//
// AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
//
// This file is automatically generated from the release version by:
//
//	scripts/release/release.py (Go adapter)
//
// To update the version:
//  1. Run: python3 scripts/release/release.py prepare <version>
//  2. Rebuild the project
//
// Purpose:
//
//	Provides version constants for CLI --version flags and runtime queries.
//	Single source of truth for version information.
//
// Design Notes:
//   - Version follows Semantic Versioning 2.0.0 (semver.org)
//   - Pre-release identifiers: -dev, -alpha.N, -beta.N, -rc.N
//   - Build metadata: +build.N, +commit.HASH
package version

// Semantic version components
const (
	Major = {major}
	Minor = {minor}
	Patch = {patch}
)

// Prerelease identifier (e.g., "dev", "alpha.1", "beta.2", "rc.1")
// Empty string for stable releases
const Prerelease = "{prerelease}"

// BuildMetadata (e.g., "build.123", "commit.abc123")
// Empty string if not specified
const BuildMetadata = "{build}"

// Version is the full version string (e.g., "0.1.0-dev", "1.2.3")
const Version = "{version_str}"

// IsPrerelease returns true if this is a pre-release version
func IsPrerelease() bool {{ return Prerelease != "" }}

// IsDevelopment returns true if this is a development version
func IsDevelopment() bool {{ return Prerelease == "dev" }}

// IsStable returns true if this is a stable release
func IsStable() bool {{ return !IsPrerelease() }}
'''

            # Determine output path based on project type
            if is_library:
                # Libraries: exported version/ for consumer access
                output_path = config.project_root / 'version' / 'version.go'
                output_rel = 'version/version.go'
            else:
                # Applications: internal/version/ (unexported)
                output_path = config.project_root / 'internal' / 'version' / 'version.go'
                output_rel = 'internal/version/version.go'

            output_path.parent.mkdir(parents=True, exist_ok=True)
            output_path.write_text(go_code, encoding='utf-8')

            print(f"  Version: {version_str}")
            print(f"  Project type: {project_type}")
            print(f"  Generated: {output_rel}")
            print(f"  Package: version")
            return True

        except Exception as e:
            print(f"  Error generating version file: {e}")
            return False

    def run_build(self, config) -> bool:
        """
        Run Go release build.

        Args:
            config: ReleaseConfig instance

        Returns:
            True if build successful
        """
        print("Running Go release build...")

        # Try make first (if Makefile exists with build-release target)
        makefile = config.project_root / 'Makefile'
        if makefile.exists():
            # Run make clean first
            self.run_command(['make', 'clean'], config.project_root, capture_output=True)

            # Use build-release for optimized binary (-ldflags="-s -w")
            result = self.run_command(['make', 'build-release'], config.project_root)
            if result:
                print("  Release build successful (via make)")
                return True

        # Fallback to direct go build with release flags
        result = self.run_command(
            ['go', 'build', '-ldflags=-s -w', './...'],
            config.project_root
        )

        if result:
            print("  Release build successful")
            return True

        print("  Build failed")
        return False

    def run_tests(self, config) -> bool:
        """
        Run Go tests.

        Args:
            config: ReleaseConfig instance

        Returns:
            True if tests pass
        """
        print("Running Go tests...")

        # Try make first (if Makefile exists with test target)
        makefile = config.project_root / 'Makefile'
        if makefile.exists():
            result = self.run_command(
                ['make', 'test'],
                config.project_root,
                capture_output=True
            )
            if result is not None:
                print("  All tests passed (via make)")
                return True

        # Fallback to direct go test
        # Use GOWORK=off to avoid workspace issues during release
        import os
        env = os.environ.copy()
        env['GOWORK'] = 'off'

        result = self.run_command(
            ['go', 'test', './...'],
            config.project_root,
            capture_output=True
        )

        if result is not None:
            print("  All tests passed")
            return True

        print("  Tests failed")
        return False

    def run_format(self, config) -> bool:
        """
        Run Go code formatting.

        Args:
            config: ReleaseConfig instance

        Returns:
            True if successful
        """
        print("Formatting Go code...")

        if getattr(config, 'dry_run', False):
            print("  [DRY-RUN] Would format Go code")
            return True

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

        # Fallback to gofmt
        result = self.run_command(
            ['gofmt', '-w', '.'],
            config.project_root,
            capture_output=True
        )

        if result is not None:
            print("  Code formatted")
            return True

        return True  # Not fatal if formatting fails

    def cleanup_temp_files(self, config) -> bool:
        """
        Clean up Go build artifacts.

        Args:
            config: ReleaseConfig instance

        Returns:
            True if successful
        """
        print("Cleaning up temporary files...")

        if getattr(config, 'dry_run', False):
            print("  [DRY-RUN] Would clean build artifacts")
            return True

        # Try make clean first
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

        # Fallback to go clean
        self.run_command(
            ['go', 'clean', '-cache'],
            config.project_root,
            capture_output=True
        )

        print("  Cleaned")
        return True

    def validate_makefile(self, config) -> bool:
        """
        Validate all key Makefile targets for Go projects.

        Runs each target to ensure they all execute without errors.

        Args:
            config: ReleaseConfig instance

        Returns:
            True if all targets work
        """
        makefile = config.project_root / 'Makefile'
        if not makefile.exists():
            print("  No Makefile found, skipping validation")
            return True

        # Key targets to validate for Go projects
        # Ordered to avoid dependencies issues (clean first, then build-dependent)
        targets = [
            'help',           # Display help
            'clean',          # Clean build artifacts
            'build',          # Development build
            'test',           # Run all tests
            'vet',            # Go vet
            'format',         # Code formatting
            'check-arch',     # Architecture validation
            'stats',          # Project statistics
        ]

        print("Validating Makefile targets...")
        failed_targets = []

        for target in targets:
            result = self.run_command(
                ['make', target],
                config.project_root,
                capture_output=True,
                check=False
            )
            if result is None:
                print(f"  ✗ make {target}")
                failed_targets.append(target)
            else:
                print(f"  ✓ make {target}")

        # Clean up after validation
        self.run_command(['make', 'clean'], config.project_root, capture_output=True, check=False)

        if failed_targets:
            print(f"\n  Failed targets: {', '.join(failed_targets)}")
            return False

        print("  All Makefile targets validated successfully")
        return True
