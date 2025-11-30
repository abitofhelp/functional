# SPDX-License-Identifier: BSD-3-Clause
# Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
"""
Entry point for running arch_guard as a module.

Usage (from scripts directory):
    cd scripts && python3 -m arch_guard

Or run directly from project root (recommended):
    python3 scripts/arch_guard/arch_guard.py
"""

import sys
from .arch_guard import main

if __name__ == '__main__':
    sys.exit(main())
