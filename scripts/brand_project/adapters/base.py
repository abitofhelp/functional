#!/usr/bin/env python3
# ==============================================================================
# adapters/base.py - Base adapter for brand_project
# ==============================================================================
# Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
# SPDX-License-Identifier: BSD-3-Clause
# See LICENSE file in the project root.
#
# Purpose:
#   Abstract base class for language-specific project branding adapters.
#   Provides common functionality for file operations and text replacement.
#
# ==============================================================================

from abc import ABC, abstractmethod
from pathlib import Path
from typing import List, Set, Tuple
import shutil
import re

import sys
sys.path.insert(0, str(Path(__file__).parent.parent.parent))
from common import print_success, print_error, print_warning, print_info


class BaseAdapter(ABC):
    """
    Abstract base class for language-specific branding adapters.

    Subclasses must implement:
        - excluded_dirs: Directories to skip during copy
        - excluded_patterns: File patterns to skip
        - text_file_extensions: Extensions to process for text replacement
        - update_config_files(): Language-specific config updates
    """

    # Common directories to exclude (all languages)
    COMMON_EXCLUDED_DIRS: Set[str] = {
        '.git',
        '__pycache__',
        '.pytest_cache',
        '.mypy_cache',
        '.venv',
        'venv',
        '.idea',
        '.vscode',
    }

    # Common file patterns to exclude
    COMMON_EXCLUDED_PATTERNS: Set[str] = {
        '*.gz',
        '*.zip',
        '*.tar',
        '*.pyc',
        '*.pyo',
        '.DS_Store',
        'Thumbs.db',
    }

    # Common text file extensions
    COMMON_TEXT_EXTENSIONS: Set[str] = {
        '.md',
        '.txt',
        '.yml',
        '.yaml',
        '.json',
        '.toml',
        '.py',
        '.sh',
        '.bash',
        '.puml',  # PlantUML diagrams
        '.svg',   # Generated diagrams (contain text)
        '.html',  # HTML files (coverage reports, etc.)
        '.xml',   # XML config files
    }

    # Files without extensions that should be processed
    COMMON_TEXT_FILENAMES: Set[str] = {
        'Makefile',
        'Dockerfile',
        'LICENSE',
    }

    @property
    @abstractmethod
    def excluded_dirs(self) -> Set[str]:
        """Directories to exclude from copy (language-specific + common)."""
        pass

    @property
    @abstractmethod
    def excluded_patterns(self) -> Set[str]:
        """File patterns to exclude from copy."""
        pass

    @property
    @abstractmethod
    def text_file_extensions(self) -> Set[str]:
        """File extensions to process for text replacement."""
        pass

    @abstractmethod
    def update_config_files(self, config) -> List[str]:
        """
        Update language-specific configuration files.

        Args:
            config: ProjectConfig instance

        Returns:
            List of updated file paths (relative to target_dir)
        """
        pass

    @abstractmethod
    def get_replacement_pairs(self, config) -> List[Tuple[str, str]]:
        """
        Get text replacement pairs for this language.

        Args:
            config: ProjectConfig instance

        Returns:
            List of (old_text, new_text) tuples in order of replacement
        """
        pass

    def should_exclude_dir(self, dir_name: str) -> bool:
        """Check if a directory should be excluded."""
        return dir_name in self.excluded_dirs

    def should_exclude_file(self, file_path: Path) -> bool:
        """Check if a file should be excluded."""
        for pattern in self.excluded_patterns:
            if file_path.match(pattern):
                return True
        return False

    def is_text_file(self, file_path: Path) -> bool:
        """Check if a file should be processed for text replacement."""
        # Check by extension
        if file_path.suffix.lower() in self.text_file_extensions:
            return True
        # Check by filename (for files without extensions like Makefile)
        if file_path.name in self.COMMON_TEXT_FILENAMES:
            return True
        return False

    def copy_template(self, config, verbose: bool = False) -> int:
        """
        Copy template directory to target, excluding build artifacts.

        Args:
            config: ProjectConfig instance
            verbose: Print detailed progress

        Returns:
            Number of files copied
        """
        if config.target_dir.exists():
            print_error(f"Target directory already exists: {config.target_dir}")
            return 0

        file_count = 0

        def copy_tree(src: Path, dst: Path):
            nonlocal file_count

            if not config.dry_run:
                dst.mkdir(parents=True, exist_ok=True)

            for item in src.iterdir():
                if item.is_dir():
                    if self.should_exclude_dir(item.name):
                        if verbose:
                            print_info(f"  Skipping directory: {item.name}/")
                        continue
                    copy_tree(item, dst / item.name)
                else:
                    if self.should_exclude_file(item):
                        if verbose:
                            print_info(f"  Skipping file: {item.name}")
                        continue

                    if config.dry_run:
                        if verbose:
                            print_info(f"  Would copy: {item.name}")
                    else:
                        shutil.copy2(item, dst / item.name)
                        if verbose:
                            print_info(f"  Copied: {item.name}")
                    file_count += 1

        if config.dry_run:
            print_info(f"[DRY RUN] Would copy template to: {config.target_dir}")
        else:
            print_info(f"Copying template to: {config.target_dir}")

        copy_tree(config.source_dir, config.target_dir)
        return file_count

    def rename_files(self, config, verbose: bool = False) -> List[str]:
        """
        Rename files containing the old project name.

        Args:
            config: ProjectConfig instance
            verbose: Print detailed progress

        Returns:
            List of renamed files (new paths)
        """
        renamed = []
        replacements = self.get_replacement_pairs(config)

        # Collect all files first, then rename (to avoid issues during iteration)
        files_to_rename: List[Tuple[Path, Path]] = []

        for file_path in config.target_dir.rglob('*'):
            if file_path.is_dir():
                continue

            new_name = file_path.name
            for old_text, new_text in replacements:
                if old_text in new_name:
                    new_name = new_name.replace(old_text, new_text)

            if new_name != file_path.name:
                new_path = file_path.parent / new_name
                files_to_rename.append((file_path, new_path))

        # Perform renames
        for old_path, new_path in files_to_rename:
            if config.dry_run:
                if verbose:
                    print_info(f"  Would rename: {old_path.name} -> {new_path.name}")
            else:
                old_path.rename(new_path)
                if verbose:
                    print_info(f"  Renamed: {old_path.name} -> {new_path.name}")
            renamed.append(str(new_path.relative_to(config.target_dir)))

        return renamed

    def replace_in_files(self, config, verbose: bool = False) -> int:
        """
        Replace old project name with new name in file contents.

        Args:
            config: ProjectConfig instance
            verbose: Print detailed progress

        Returns:
            Number of files modified
        """
        modified_count = 0
        replacements = self.get_replacement_pairs(config)

        for file_path in config.target_dir.rglob('*'):
            if file_path.is_dir():
                continue

            if not self.is_text_file(file_path):
                continue

            try:
                content = file_path.read_text(encoding='utf-8')
                original_content = content

                for old_text, new_text in replacements:
                    content = content.replace(old_text, new_text)

                if content != original_content:
                    if config.dry_run:
                        if verbose:
                            rel_path = file_path.relative_to(config.target_dir)
                            print_info(f"  Would modify: {rel_path}")
                    else:
                        file_path.write_text(content, encoding='utf-8')
                        if verbose:
                            rel_path = file_path.relative_to(config.target_dir)
                            print_info(f"  Modified: {rel_path}")
                    modified_count += 1

            except UnicodeDecodeError:
                # Skip binary files
                continue
            except Exception as e:
                print_warning(f"Error processing {file_path}: {e}")

        return modified_count

    def verify_no_old_references(self, config) -> List[str]:
        """
        Verify no old project name references remain.

        Args:
            config: ProjectConfig instance

        Returns:
            List of files still containing old references
        """
        files_with_old_refs = []
        old_patterns = [
            config.old_name,
            config.old_name_pascal,
            config.old_name_ada_pascal,
        ]

        for file_path in config.target_dir.rglob('*'):
            if file_path.is_dir():
                continue

            # Check filename
            for pattern in old_patterns:
                if pattern in file_path.name:
                    files_with_old_refs.append(
                        f"{file_path.relative_to(config.target_dir)} (filename)"
                    )
                    break

            # Check content of text files
            if self.is_text_file(file_path):
                try:
                    content = file_path.read_text(encoding='utf-8')
                    for pattern in old_patterns:
                        if pattern in content:
                            files_with_old_refs.append(
                                f"{file_path.relative_to(config.target_dir)} (content: {pattern})"
                            )
                            break
                except (UnicodeDecodeError, Exception):
                    continue

        return files_with_old_refs
