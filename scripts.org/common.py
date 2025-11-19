#!/usr/bin/env python3
# ==============================================================================
# common.py
# ==============================================================================
# Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
# SPDX-License-Identifier: BSD-3-Clause
# See LICENSE file in the project root.
#
# Purpose:
#   Shared utilities for project automation scripts.
#       Provides OS detection, terminal colors, command execution helpers,
#       and common operations used across all scripts.
#
# Usage:
#   Import utilities in other scripts:
#          from common import print_success, command_exists, is_macos
#
#          if command_exists('gcovr'):
#              print_success("gcovr is installed")
#
# Design Notes:
#   Design as pure utility module - no side effects
#       All functions are stateless and reusable
#       Terminal colors use ANSI escape codes for cross-platform support
#
# See Also:
#   install_tools.py - uses OS detection and command execution
#       run_coverage.py - uses command execution and print functions
#       Python os and shutil modules for platform operations
# ==============================================================================

import platform
import shutil
import subprocess
import sys
from typing import Optional


# ANSI color codes for terminal output
class Colors:
    """Terminal color codes for formatted output."""
    RED = '\033[0;31m'
    GREEN = '\033[0;32m'
    YELLOW = '\033[1;33m'
    BLUE = '\033[0;34m'
    CYAN = '\033[0;36m'
    ORANGE = '\033[0;33m'
    BOLD = '\033[1m'
    NC = '\033[0m'  # No Color


def print_success(message: str) -> None:
    """Print a success message in green."""
    print(f"{Colors.GREEN}✓ {message}{Colors.NC}")


def print_error(message: str) -> None:
    """Print an error message in red."""
    print(f"{Colors.RED}✗ {message}{Colors.NC}", file=sys.stderr)


def print_warning(message: str) -> None:
    """Print a warning message in yellow."""
    print(f"{Colors.YELLOW}⚠ {message}{Colors.NC}")


def print_info(message: str) -> None:
    """Print an info message in cyan."""
    print(f"{Colors.CYAN}{message}{Colors.NC}")


def print_section(message: str) -> None:
    """Print a section header in blue."""
    print(f"{Colors.BLUE}{message}{Colors.NC}")


def command_exists(command: str) -> bool:
    """Check if a command exists in PATH."""
    return shutil.which(command) is not None


def run_command(cmd: list[str], check: bool = True, capture: bool = False) -> Optional[subprocess.CompletedProcess]:
    """
    Run a shell command.

    Args:
        cmd: Command as list of strings
        check: Raise exception on non-zero exit
        capture: Capture stdout/stderr

    Returns:
        CompletedProcess if capture=True, None otherwise
    """
    try:
        if capture:
            return subprocess.run(cmd, check=check, capture_output=True, text=True)
        else:
            subprocess.run(cmd, check=check)
            return None
    except subprocess.CalledProcessError as e:
        if check:
            print_error(f"Command failed: {' '.join(cmd)}")
            raise
        return None


def get_os_type() -> str:
    """
    Get the operating system type.

    Returns:
        'Darwin' for macOS, 'Linux' for Linux, 'Windows' for Windows
    """
    return platform.system()


def is_macos() -> bool:
    """Check if running on macOS."""
    return get_os_type() == 'Darwin'


def is_linux() -> bool:
    """Check if running on Linux."""
    return get_os_type() == 'Linux'


def is_windows() -> bool:
    """Check if running on Windows."""
    return get_os_type() == 'Windows'


def detect_package_manager() -> Optional[str]:
    """
    Detect the system package manager on Linux.

    Returns:
        'apt', 'yum', 'dnf', etc., or None if not detected
    """
    if not is_linux():
        return None

    managers = ['apt-get', 'yum', 'dnf', 'pacman', 'zypper']
    for manager in managers:
        if command_exists(manager):
            return manager

    return None
