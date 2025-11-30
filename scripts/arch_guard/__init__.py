# SPDX-License-Identifier: BSD-3-Clause
# Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
"""
Unified Architecture Guard for Multi-Language Projects.

Validates layer dependencies in hybrid DDD/Clean/Hexagonal architecture
across Go, Ada, and Rust projects.

Usage:
    # From project root (recommended):
    python3 scripts/arch_guard/arch_guard.py
    python3 scripts/arch_guard/arch_guard.py --project-root /path/to/project

    # Or from scripts directory:
    cd scripts && python3 -m arch_guard
"""

from .arch_guard import ArchitectureGuard, main
from .models import ArchitectureViolation

__all__ = ['ArchitectureGuard', 'ArchitectureViolation', 'main']
