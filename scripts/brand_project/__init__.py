# ==============================================================================
# brand_project/__init__.py - Unified Project Branding Tool
# ==============================================================================
# Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
# SPDX-License-Identifier: BSD-3-Clause
# ==============================================================================

"""
brand_project - Instantiate new projects from hybrid_app/hybrid_lib templates.

Usage:
    # From project root (recommended):
    python3 scripts/brand_project/brand_project.py --git-repo github.com/user/my_app
    python3 scripts/brand_project/brand_project.py --git-repo github.com/user/my_app --dry-run

    # Or from scripts directory:
    cd scripts && python3 -m brand_project --git-repo github.com/user/my_app

Supported languages:
    - Go (go.mod, go.work)
    - Ada (alire.toml, .gpr)
    - Rust (Cargo.toml) - future
"""

from .models import GitRepoUrl, ProjectConfig, Language
from .brand_project import brand_project, detect_language

__all__ = [
    'GitRepoUrl',
    'ProjectConfig',
    'Language',
    'brand_project',
    'detect_language',
]

__version__ = '1.0.0'
