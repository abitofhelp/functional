# SPDX-License-Identifier: BSD-3-Clause
# Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
"""
Language-specific adapters for architecture validation.
"""

from .base import LanguageAdapter
from .ada import AdaAdapter
from .go import GoAdapter

__all__ = ['LanguageAdapter', 'AdaAdapter', 'GoAdapter']
