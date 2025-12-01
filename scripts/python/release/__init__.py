# ==============================================================================
# release/__init__.py - Unified Release Management Tool
# ==============================================================================
# Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
# SPDX-License-Identifier: BSD-3-Clause
# ==============================================================================

"""
release - Unified release management for Go and Ada projects.

Usage:
    # From project root (recommended):
    python3 scripts/python/release/release.py prepare 1.0.0
    python3 scripts/python/release/release.py release 1.0.0
    python3 scripts/python/release/release.py diagrams

    # Or from scripts directory:
    cd scripts/python && python3 -m release prepare 1.0.0

Supported languages:
    - Go (go.mod)
    - Ada (alire.toml)
"""

from .models import ReleaseConfig, Language, ReleaseAction
from .release import prepare_release, create_release, detect_language

__all__ = [
    'ReleaseConfig',
    'Language',
    'ReleaseAction',
    'prepare_release',
    'create_release',
    'detect_language',
]

__version__ = '1.0.0'
