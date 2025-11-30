# SPDX-License-Identifier: BSD-3-Clause
# Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
"""
Data models for architecture validation.
"""

from dataclasses import dataclass


@dataclass
class ArchitectureViolation:
    """Represents a single architecture rule violation."""
    file_path: str
    line_number: int
    violation_type: str
    details: str
