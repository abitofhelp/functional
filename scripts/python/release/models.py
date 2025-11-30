#!/usr/bin/env python3
# ==============================================================================
# models.py - Data models for release management
# ==============================================================================
# Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
# SPDX-License-Identifier: BSD-3-Clause
# See LICENSE file in the project root.
#
# Purpose:
#   Data classes and enums for the unified release management system.
#   Provides ReleaseConfig and Language enum for multi-language support.
#
# Usage:
#   from models import ReleaseConfig, Language
#
#   config = ReleaseConfig(
#       project_root=Path('/path/to/project'),
#       version='1.0.0',
#       language=Language.GO,
#   )
#
# ==============================================================================

from dataclasses import dataclass
from datetime import datetime
from enum import Enum
from pathlib import Path
from typing import Optional


class Language(Enum):
    """Supported programming languages."""
    GO = 'go'
    ADA = 'ada'
    RUST = 'rust'  # Future support


class ReleaseAction(Enum):
    """Release workflow actions."""
    PREPARE = 'prepare'
    RELEASE = 'release'
    DIAGRAMS = 'diagrams'
    VALIDATE = 'validate'


@dataclass
class ReleaseConfig:
    """Configuration for release operations."""
    project_root: Path
    version: str
    language: Language
    dry_run: bool = False

    # Computed fields (set in __post_init__)
    date_str: str = ""
    year: int = 0
    project_name: str = ""
    project_url: str = ""

    def __post_init__(self):
        """Initialize computed fields."""
        self.date_str = datetime.now().strftime("%B %d, %Y")
        self.year = datetime.now().year

    @property
    def is_prerelease(self) -> bool:
        """Check if this is a pre-release version (contains - or +)."""
        return '-' in self.version

    @property
    def is_initial_release(self) -> bool:
        """Check if this is an initial release (<= 1.0.0)."""
        try:
            from packaging import version as pkg_version
            return pkg_version.parse(self.version) <= pkg_version.parse("1.0.0")
        except ImportError:
            # Fallback without packaging library
            return self.version in ["0.1.0", "1.0.0"] or self.version.startswith("0.")

    @property
    def tag_name(self) -> str:
        """Get the git tag name for this version."""
        return f"v{self.version}"
