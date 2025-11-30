#!/usr/bin/env python3
# ==============================================================================
# adapters/ada.py - Ada language adapter for brand_project
# ==============================================================================
# Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
# SPDX-License-Identifier: BSD-3-Clause
# See LICENSE file in the project root.
#
# Purpose:
#   Ada-specific adapter for project branding operations.
#   Handles alire.toml, .gpr files, and Ada source file updates.
#
# ==============================================================================

from pathlib import Path
from typing import List, Set, Tuple
import re

from .base import BaseAdapter

import sys
sys.path.insert(0, str(Path(__file__).parent.parent.parent))
from common import print_success, print_info, print_warning


class AdaAdapter(BaseAdapter):
    """
    Ada-specific adapter for project branding.

    Handles:
        - alire.toml name and website updates
        - .gpr project file updates
        - Ada source file (.ads, .adb) updates
        - Ada-specific naming conventions (Underscore_Pascal_Case)
    """

    ADA_EXCLUDED_DIRS: Set[str] = {
        'alire',
        '.alire',
        'obj',
        'bin',
        'lib',
    }

    ADA_TEXT_EXTENSIONS: Set[str] = {
        '.ads',
        '.adb',
        '.gpr',
    }

    @property
    def excluded_dirs(self) -> Set[str]:
        return self.COMMON_EXCLUDED_DIRS | self.ADA_EXCLUDED_DIRS

    @property
    def excluded_patterns(self) -> Set[str]:
        return self.COMMON_EXCLUDED_PATTERNS | {'*.ali', '*.o'}

    @property
    def text_file_extensions(self) -> Set[str]:
        return self.COMMON_TEXT_EXTENSIONS | self.ADA_TEXT_EXTENSIONS

    def get_replacement_pairs(self, config) -> List[Tuple[str, str]]:
        """
        Get text replacement pairs for Ada projects.

        Order matters - replace longer/more specific patterns first.
        Ada uses Underscore_Pascal_Case for packages (e.g., Hybrid_App_Ada).
        """
        pairs = []

        # Project name variations (order: longest first)
        pairs.extend([
            (config.old_name_ada_pascal, config.new_name_ada_pascal),  # Hybrid_App_Ada
            (config.old_name_pascal, config.new_name_pascal),          # HybridAppAda
            (config.old_name_upper, config.new_name_upper),            # HYBRID_APP_ADA
            (config.old_name, config.new_name),                        # hybrid_app_ada
        ])

        return pairs

    def update_config_files(self, config) -> List[str]:
        """
        Update Ada-specific configuration files.

        Updates:
            - alire.toml (name, website)
            - All .gpr files (project name)
        """
        updated = []

        # Update alire.toml if present
        alire_toml = config.target_dir / 'alire.toml'
        if alire_toml.exists():
            if self._update_alire_toml(alire_toml, config):
                updated.append('alire.toml')

        # Update all .gpr files
        for gpr_file in config.target_dir.rglob('*.gpr'):
            if self._update_gpr_file(gpr_file, config):
                updated.append(str(gpr_file.relative_to(config.target_dir)))

        return updated

    def _update_alire_toml(self, alire_toml: Path, config) -> bool:
        """Update alire.toml with new project name and website."""
        if config.dry_run:
            print_info(f"  [DRY RUN] Would update: alire.toml")
            return True

        try:
            content = alire_toml.read_text(encoding='utf-8')
            original = content

            # Update name field
            content = re.sub(
                r'^(name\s*=\s*")[^"]*(")',
                f'\\g<1>{config.new_name}\\g<2>',
                content,
                flags=re.MULTILINE
            )

            # Update website field
            content = re.sub(
                r'^(website\s*=\s*")[^"]*(")',
                f'\\g<1>{config.new_repo.https_url}\\g<2>',
                content,
                flags=re.MULTILINE
            )

            # Also replace old project name references in other fields
            for old_text, new_text in self.get_replacement_pairs(config):
                content = content.replace(old_text, new_text)

            if content != original:
                alire_toml.write_text(content, encoding='utf-8')
                return True

        except Exception as e:
            print_warning(f"Error updating alire.toml: {e}")

        return False

    def _update_gpr_file(self, gpr_file: Path, config) -> bool:
        """Update a .gpr project file."""
        if config.dry_run:
            print_info(f"  [DRY RUN] Would update: {gpr_file.name}")
            return True

        try:
            content = gpr_file.read_text(encoding='utf-8')
            original = content

            # Replace all name variations
            for old_text, new_text in self.get_replacement_pairs(config):
                content = content.replace(old_text, new_text)

            if content != original:
                gpr_file.write_text(content, encoding='utf-8')
                return True

        except Exception as e:
            print_warning(f"Error updating {gpr_file}: {e}")

        return False

    @staticmethod
    def detect(project_root: Path) -> bool:
        """
        Detect if a directory is an Ada project.

        Args:
            project_root: Path to check

        Returns:
            True if Ada project detected
        """
        # Check for alire.toml
        if (project_root / 'alire.toml').exists():
            return True
        # Check for .gpr files
        if list(project_root.glob('*.gpr')):
            return True
        if list(project_root.glob('**/*.gpr')):
            return True
        # Check for Ada source files
        if list(project_root.glob('**/*.ads')) or list(project_root.glob('**/*.adb')):
            return True
        return False
