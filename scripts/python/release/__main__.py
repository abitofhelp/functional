#!/usr/bin/env python3
# ==============================================================================
# release/__main__.py - Module entry point
# ==============================================================================
# Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
# SPDX-License-Identifier: BSD-3-Clause
# ==============================================================================

"""
Entry point for running release as a module.

Usage (from scripts directory):
    cd scripts && python3 -m release prepare 1.0.0
    cd scripts && python3 -m release release 1.0.0

Or run directly from project root (recommended):
    python3 scripts/release/release.py prepare 1.0.0
"""

from .release import main

if __name__ == '__main__':
    main()
