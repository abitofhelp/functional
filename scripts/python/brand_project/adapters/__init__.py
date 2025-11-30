# ==============================================================================
# adapters/__init__.py - Language adapters for brand_project
# ==============================================================================
# Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
# SPDX-License-Identifier: BSD-3-Clause
# ==============================================================================

from .base import BaseAdapter
from .go import GoAdapter
from .ada import AdaAdapter

__all__ = ['BaseAdapter', 'GoAdapter', 'AdaAdapter']
