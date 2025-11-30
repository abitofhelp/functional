#!/usr/bin/env python3
# ==============================================================================
# brand_project/__main__.py - Module entry point
# ==============================================================================
# Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
# SPDX-License-Identifier: BSD-3-Clause
# ==============================================================================

"""
Entry point for running brand_project as a module.

Usage (from scripts directory):
    cd scripts && python3 -m brand_project --git-repo github.com/user/my_app

Or run directly from project root (recommended):
    python3 scripts/brand_project/brand_project.py --git-repo github.com/user/my_app
"""

from .brand_project import main

if __name__ == '__main__':
    main()
