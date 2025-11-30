# ==============================================================================
# adapters/__init__.py - Language adapters for release management
# ==============================================================================
# Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
# SPDX-License-Identifier: BSD-3-Clause
# ==============================================================================

from .base import BaseReleaseAdapter
from .go import GoReleaseAdapter
from .ada import AdaReleaseAdapter

__all__ = ['BaseReleaseAdapter', 'GoReleaseAdapter', 'AdaReleaseAdapter']
