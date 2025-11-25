# SPDX-License-Identifier: BSD-3-Clause
# Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
"""Entry point for running arch_guard as a module: python -m arch_guard"""

import sys
from .arch_guard import main

if __name__ == '__main__':
    sys.exit(main())
