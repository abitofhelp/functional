#!/usr/bin/env python3
# ==============================================================================
# install_tools.py
# ==============================================================================
# Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
# SPDX-License-Identifier: BSD-3-Clause
# See LICENSE file in the project root.
#
# Purpose:
#   Automated installation of development dependencies.
#       Installs GMP library, gcovr, and gnatformat based on detected OS.
#       Verifies installations and provides helpful error messages.
#
# Usage:
#   Run directly or via Makefile:
#          python3 scripts/makefile/install_tools.py
#          make install-tools
#
#          Checks existing installations before installing
#          Provides OS-specific installation commands
#
# Design Notes:
#   Uses common.py for OS detection and command execution
#       Implements graceful degradation (warns on optional tool failures)
#       Provides detailed installation verification
#
# See Also:
#   common.py - OS detection and utilities
#       Makefile - calls this script for install-tools target
#       README.md - installation documentation
# ==============================================================================

import sys
from pathlib import Path

# Add scripts directory to path for imports
sys.path.insert(0, str(Path(__file__).parent.parent))

from common import (
    Colors,
    command_exists,
    detect_package_manager,
    get_os_type,
    is_linux,
    is_macos,
    print_error,
    print_info,
    print_section,
    print_success,
    print_warning,
    run_command,
)


def check_gmp_installed() -> bool:
    """Check if GMP library is installed."""
    if is_macos():
        # Check via Homebrew
        result = run_command(['brew', 'list', 'gmp'], check=False, capture=True)
        return result is not None and result.returncode == 0
    elif is_linux():
        # Check for libgmp-dev via dpkg or rpm
        dpkg_result = run_command(['dpkg', '-l', 'libgmp-dev'], check=False, capture=True)
        if dpkg_result and dpkg_result.returncode == 0 and 'ii' in dpkg_result.stdout:
            return True
        rpm_result = run_command(['rpm', '-qa'], check=False, capture=True)
        if rpm_result and 'gmp-devel' in rpm_result.stdout:
            return True
        return False
    return False


def install_gmp() -> bool:
    """Install GMP library based on the operating system."""
    if check_gmp_installed():
        print_success("GMP already installed")
        return True

    print_section("Installing GMP library...")

    try:
        if is_macos():
            if not command_exists('brew'):
                print_error("Homebrew not found. Please install Homebrew first:")
                print("  Visit: https://brew.sh")
                return False
            run_command(['brew', 'install', 'gmp'])
            print_success("GMP installed via Homebrew")
            return True

        elif is_linux():
            pkg_manager = detect_package_manager()
            if pkg_manager == 'apt-get':
                print_info("Installing via apt-get...")
                run_command(['sudo', 'apt-get', 'update'])
                run_command(['sudo', 'apt-get', 'install', '-y', 'libgmp-dev'])
                print_success("GMP installed via apt-get")
                return True
            elif pkg_manager in ['yum', 'dnf']:
                print_info(f"Installing via {pkg_manager}...")
                run_command(['sudo', pkg_manager, 'install', '-y', 'gmp-devel'])
                print_success(f"GMP installed via {pkg_manager}")
                return True
            else:
                print_error("Unable to detect package manager")
                print_warning("Please install libgmp-dev manually")
                return False
        else:
            print_warning(f"Unknown OS: {get_os_type()} - skipping GMP installation")
            return False

    except Exception as e:
        print_error(f"Failed to install GMP: {e}")
        return False


def install_gcovr() -> bool:
    """Install gcovr for coverage analysis."""
    if command_exists('gcovr'):
        print_success("gcovr already installed")
        return True

    print_section("Installing gcovr...")

    try:
        if not command_exists('pip3'):
            print_error("pip3 not found. Please install Python 3 and pip first")
            return False

        run_command(['pip3', 'install', 'gcovr'])
        print_success("gcovr installed")
        return True

    except Exception as e:
        print_error(f"Failed to install gcovr: {e}")
        return False


def install_gnatformat() -> bool:
    """Install gnatformat for code formatting."""
    if command_exists('gnatformat'):
        print_success("gnatformat already installed")
        return True

    print_section("Installing gnatformat...")

    try:
        if not command_exists('alr'):
            print_error("alr (Alire) not found. Please install Alire first:")
            print("  Visit: https://alire.ada.dev")
            return False

        run_command(['alr', 'get', '--build', 'gnatformat'])
        print_success("gnatformat installed")
        print_warning("Note: Add gnatformat to PATH or use via 'alr exec -- gnatformat'")
        return True

    except Exception as e:
        print_error(f"Failed to install gnatformat: {e}")
        return False


def verify_installations() -> None:
    """Verify all required tools are available."""
    print_section("\nVerifying installations...")
    print()

    tools = [
        ('alr', 'Alire package manager', 'https://alire.ada.dev'),
        ('gcovr', 'Coverage report generator', 'pip3 install gcovr'),
        ('gnatformat', 'Ada code formatter', 'alr get --build gnatformat'),
        ('gnatdoc', 'Ada documentation generator', 'Part of GNAT toolchain'),
    ]

    for tool, description, install_hint in tools:
        if command_exists(tool):
            print_success(f"{tool} found - {description}")
        else:
            print_warning(f"{tool} not found - {description}")
            print(f"  Install: {install_hint}")


def main() -> int:
    """Main entry point."""
    print_info("Installing missing tools and dependencies...")
    print()

    success = True

    # Install GMP library (required for some GNAT libraries)
    if not install_gmp():
        success = False
    print()

    # Install gcovr (required for coverage reports)
    if not install_gcovr():
        success = False
    print()

    # Install gnatformat (optional, for code formatting)
    install_gnatformat()  # Non-critical, don't fail on this
    print()

    # Verify all installations
    verify_installations()

    if success:
        print()
        print_success("Tool installation complete!")
        return 0
    else:
        print()
        print_error("Some tools failed to install. Please review the errors above.")
        return 1


if __name__ == '__main__':
    sys.exit(main())
