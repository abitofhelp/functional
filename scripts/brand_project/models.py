#!/usr/bin/env python3
# ==============================================================================
# models.py - Data models for brand_project
# ==============================================================================
# Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
# SPDX-License-Identifier: BSD-3-Clause
# See LICENSE file in the project root.
#
# Purpose:
#   Data models for project branding operations.
#   Provides GitRepoUrl parsing and ProjectConfig for template instantiation.
#
# ==============================================================================

from dataclasses import dataclass
from enum import Enum
from pathlib import Path
from typing import Optional
from urllib.parse import urlparse
import re


class Language(Enum):
    """Supported template languages."""
    GO = "go"
    ADA = "ada"
    RUST = "rust"


@dataclass
class GitRepoUrl:
    """
    Parsed Git repository URL.

    Parses URLs like:
        https://github.com/abitofhelp/my_awesome_app.git
        github.com/abitofhelp/my_awesome_app
        git@github.com:abitofhelp/my_awesome_app.git

    Attributes:
        host: e.g., "github.com"
        account: e.g., "abitofhelp"
        project_name: e.g., "my_awesome_app"
    """
    host: str
    account: str
    project_name: str

    @classmethod
    def parse(cls, url: str) -> 'GitRepoUrl':
        """
        Parse a git repository URL.

        Args:
            url: Git repository URL in various formats

        Returns:
            GitRepoUrl instance

        Raises:
            ValueError: If URL cannot be parsed
        """
        original_url = url

        # Normalize the URL
        url = url.strip()

        # Remove .git suffix if present
        if url.endswith('.git'):
            url = url[:-4]

        # Handle SSH format: git@github.com:account/repo
        ssh_match = re.match(r'^git@([^:]+):([^/]+)/(.+)$', url)
        if ssh_match:
            return cls(
                host=ssh_match.group(1),
                account=ssh_match.group(2),
                project_name=ssh_match.group(3)
            )

        # Add https:// if no scheme present
        if not url.startswith(('http://', 'https://')):
            url = 'https://' + url

        # Parse as URL
        parsed = urlparse(url)

        if not parsed.netloc:
            raise ValueError(f"Cannot parse git URL: {original_url}")

        # Extract path components
        path_parts = parsed.path.strip('/').split('/')

        if len(path_parts) < 2:
            raise ValueError(
                f"URL must have at least account/project: {original_url}"
            )

        return cls(
            host=parsed.netloc,
            account=path_parts[0],
            project_name=path_parts[1]
        )

    @property
    def https_url(self) -> str:
        """Full HTTPS URL without .git suffix."""
        return f"https://{self.host}/{self.account}/{self.project_name}"

    @property
    def clone_url(self) -> str:
        """Full HTTPS URL with .git suffix for cloning."""
        return f"{self.https_url}.git"

    @property
    def module_path(self) -> str:
        """Go module path (host/account/project)."""
        return f"{self.host}/{self.account}/{self.project_name}"


@dataclass
class ProjectConfig:
    """
    Configuration for project branding operation.

    Attributes:
        source_dir: Path to template directory
        target_dir: Path to create new project
        old_name: Template project name (snake_case)
        new_repo: Parsed git repository URL for new project
        language: Detected or specified language
        dry_run: If True, don't make changes, just report
    """
    source_dir: Path
    target_dir: Path
    old_name: str
    new_repo: GitRepoUrl
    language: Language
    dry_run: bool = False

    @property
    def new_name(self) -> str:
        """New project name in snake_case."""
        return self.new_repo.project_name

    @property
    def old_name_pascal(self) -> str:
        """Old project name in PascalCase (e.g., HybridAppGo)."""
        return to_pascal_case(self.old_name)

    @property
    def new_name_pascal(self) -> str:
        """New project name in PascalCase."""
        return to_pascal_case(self.new_name)

    @property
    def old_name_ada_pascal(self) -> str:
        """Old project name in Ada PascalCase (e.g., Hybrid_App_Go)."""
        return to_ada_pascal_case(self.old_name)

    @property
    def new_name_ada_pascal(self) -> str:
        """New project name in Ada PascalCase."""
        return to_ada_pascal_case(self.new_name)

    @property
    def old_name_upper(self) -> str:
        """Old project name in UPPER_CASE."""
        return self.old_name.upper()

    @property
    def new_name_upper(self) -> str:
        """New project name in UPPER_CASE."""
        return self.new_name.upper()


def to_pascal_case(snake_case: str) -> str:
    """
    Convert snake_case to PascalCase.

    Args:
        snake_case: e.g., "my_awesome_app"

    Returns:
        PascalCase: e.g., "MyAwesomeApp"
    """
    return ''.join(word.capitalize() for word in snake_case.split('_'))


def to_ada_pascal_case(snake_case: str) -> str:
    """
    Convert snake_case to Ada PascalCase (preserves underscores).

    Args:
        snake_case: e.g., "my_awesome_app"

    Returns:
        Ada PascalCase: e.g., "My_Awesome_App"
    """
    return '_'.join(word.capitalize() for word in snake_case.split('_'))


def to_snake_case(name: str) -> str:
    """
    Convert various formats to snake_case.

    Args:
        name: e.g., "MyAwesomeApp" or "My_Awesome_App" or "my-awesome-app"

    Returns:
        snake_case: e.g., "my_awesome_app"
    """
    # Replace hyphens with underscores
    name = name.replace('-', '_')

    # Handle Ada Pascal_Case (already has underscores)
    if '_' in name:
        return name.lower()

    # Handle PascalCase - insert underscore before uppercase letters
    result = re.sub(r'([A-Z])', r'_\1', name)
    return result.strip('_').lower()
