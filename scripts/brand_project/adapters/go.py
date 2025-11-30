#!/usr/bin/env python3
# ==============================================================================
# adapters/go.py - Go language adapter for brand_project
# ==============================================================================
# Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
# SPDX-License-Identifier: BSD-3-Clause
# See LICENSE file in the project root.
#
# Purpose:
#   Go-specific adapter for project branding operations.
#   Handles go.mod, go.work, and Go source file updates.
#
# ==============================================================================

from pathlib import Path
from typing import List, Set, Tuple
import re

from .base import BaseAdapter

import sys
sys.path.insert(0, str(Path(__file__).parent.parent.parent))
from common import print_success, print_info, print_warning


class GoAdapter(BaseAdapter):
    """
    Go-specific adapter for project branding.

    Handles:
        - go.mod module path updates
        - go.work workspace updates
        - Go source file import path updates
        - Go-specific file extensions
    """

    GO_EXCLUDED_DIRS: Set[str] = {
        'vendor',
        'bin',
        'coverage',  # Generated coverage reports
    }

    GO_TEXT_EXTENSIONS: Set[str] = {
        '.go',
        '.mod',
        '.work',
        '.sum',
    }

    @property
    def excluded_dirs(self) -> Set[str]:
        return self.COMMON_EXCLUDED_DIRS | self.GO_EXCLUDED_DIRS

    @property
    def excluded_patterns(self) -> Set[str]:
        return self.COMMON_EXCLUDED_PATTERNS

    @property
    def text_file_extensions(self) -> Set[str]:
        return self.COMMON_TEXT_EXTENSIONS | self.GO_TEXT_EXTENSIONS

    def get_replacement_pairs(self, config) -> List[Tuple[str, str]]:
        """
        Get text replacement pairs for Go projects.

        Order matters - replace longer/more specific patterns first.
        """
        # Get the old module path from the source go.mod
        old_module_path = self._detect_module_path(config.source_dir)
        new_module_path = config.new_repo.module_path

        pairs = []

        # Module path replacement (most important for Go)
        if old_module_path:
            pairs.append((old_module_path, new_module_path))

        # Project name variations (order: longest first)
        pairs.extend([
            (config.old_name_ada_pascal, config.new_name_ada_pascal),  # Hybrid_App_Go
            (config.old_name_pascal, config.new_name_pascal),          # HybridAppGo
            (config.old_name_upper, config.new_name_upper),            # HYBRID_APP_GO
            (config.old_name, config.new_name),                        # hybrid_app_go
        ])

        return pairs

    def _detect_module_path(self, source_dir: Path) -> str:
        """Detect the Go module path from go.mod."""
        go_mod = source_dir / 'go.mod'
        if go_mod.exists():
            content = go_mod.read_text(encoding='utf-8')
            match = re.search(r'^module\s+(\S+)', content, re.MULTILINE)
            if match:
                return match.group(1)
        return ""

    def update_config_files(self, config) -> List[str]:
        """
        Update Go-specific configuration files.

        Updates:
            - All go.mod files (module path)
            - go.work file (use paths)
        """
        updated = []

        # Update all go.mod files
        for go_mod in config.target_dir.rglob('go.mod'):
            if self._update_go_mod(go_mod, config):
                updated.append(str(go_mod.relative_to(config.target_dir)))

        # Update go.work if present
        go_work = config.target_dir / 'go.work'
        if go_work.exists():
            if self._update_go_work(go_work, config):
                updated.append('go.work')

        return updated

    def _update_go_mod(self, go_mod: Path, config) -> bool:
        """Update a go.mod file with new module path."""
        if config.dry_run:
            print_info(f"  [DRY RUN] Would update: {go_mod.name}")
            return True

        try:
            content = go_mod.read_text(encoding='utf-8')
            original = content

            old_module_path = self._detect_module_path(config.source_dir)
            new_module_path = config.new_repo.module_path

            if old_module_path:
                content = content.replace(old_module_path, new_module_path)

            if content != original:
                go_mod.write_text(content, encoding='utf-8')
                return True

        except Exception as e:
            print_warning(f"Error updating {go_mod}: {e}")

        return False

    def _update_go_work(self, go_work: Path, config) -> bool:
        """Update go.work file."""
        if config.dry_run:
            print_info(f"  [DRY RUN] Would update: go.work")
            return True

        try:
            content = go_work.read_text(encoding='utf-8')
            original = content

            old_module_path = self._detect_module_path(config.source_dir)
            new_module_path = config.new_repo.module_path

            if old_module_path:
                content = content.replace(old_module_path, new_module_path)

            if content != original:
                go_work.write_text(content, encoding='utf-8')
                return True

        except Exception as e:
            print_warning(f"Error updating go.work: {e}")

        return False

    @staticmethod
    def detect(project_root: Path) -> bool:
        """
        Detect if a directory is a Go project.

        Args:
            project_root: Path to check

        Returns:
            True if Go project detected
        """
        # Check for go.mod or go.work
        if (project_root / 'go.mod').exists():
            return True
        if (project_root / 'go.work').exists():
            return True
        # Check for .go files
        if list(project_root.glob('**/*.go')):
            return True
        return False
