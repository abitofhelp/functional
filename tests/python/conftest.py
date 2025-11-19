#!/usr/bin/env python3
# ==============================================================================
# conftest.py - Pytest Configuration and Shared Fixtures
# ==============================================================================
# Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
# SPDX-License-Identifier: BSD-3-Clause
# ==============================================================================
"""
Pytest configuration and shared fixtures for Python script testing.

Provides:
- Path configuration to import scripts under test
- Shared fixtures for temporary directories and files
- Common test utilities
"""

import sys
from pathlib import Path

import pytest

# Add scripts directory to Python path for importing modules under test
PROJECT_ROOT = Path(__file__).parent.parent.parent
SCRIPTS_DIR = PROJECT_ROOT / "scripts"
sys.path.insert(0, str(SCRIPTS_DIR))


@pytest.fixture
def project_root() -> Path:
    """Return the project root directory."""
    return PROJECT_ROOT


@pytest.fixture
def scripts_dir() -> Path:
    """Return the scripts directory."""
    return SCRIPTS_DIR


@pytest.fixture
def fixtures_dir() -> Path:
    """Return the test fixtures directory."""
    return Path(__file__).parent / "fixtures"


@pytest.fixture
def valid_fixtures_dir(fixtures_dir) -> Path:
    """Return the valid test fixtures directory."""
    return fixtures_dir / "valid"


@pytest.fixture
def invalid_fixtures_dir(fixtures_dir) -> Path:
    """Return the invalid test fixtures directory."""
    return fixtures_dir / "invalid"


@pytest.fixture
def temp_ada_project(tmp_path):
    """
    Create a temporary Ada project structure for testing.

    Returns a dictionary with paths:
        - root: Project root
        - src: Source directory
        - domain: Domain layer
        - application: Application layer
        - infrastructure: Infrastructure layer
        - presentation: Presentation layer
    """
    # Create directory structure
    root = tmp_path / "project"  # Changed from "test_project" to avoid 'test' in path
    src = root / "src"

    dirs = {
        "root": root,
        "src": src,
        "domain": src / "domain",
        "application": src / "application",
        "infrastructure": src / "infrastructure",
        "presentation": src / "presentation",
        "api": src / "api",
        "test": root / "test",
    }

    # Create all directories
    for directory in dirs.values():
        directory.mkdir(parents=True, exist_ok=True)

    return dirs


@pytest.fixture
def prod_ada_project(tmp_path):
    """
    Create a production Ada project structure WITHOUT 'test' in path.

    This is specifically for testing validations that skip test paths.
    Uses a temporary directory that doesn't contain 'test' in the path.
    """
    import tempfile
    import atexit
    import shutil

    # Create temp dir with 'prod' prefix to avoid 'test' in path
    temp_dir = Path(tempfile.mkdtemp(prefix="prod_"))

    # Ensure cleanup
    def cleanup():
        if temp_dir.exists():
            shutil.rmtree(temp_dir)
    atexit.register(cleanup)

    src = temp_dir / "src"

    dirs = {
        "root": temp_dir,
        "src": src,
        "domain": src / "domain",
        "application": src / "application",
        "infrastructure": src / "infrastructure",
        "presentation": src / "presentation",
        "api": src / "api",
    }

    # Create all directories
    for directory in dirs.values():
        directory.mkdir(parents=True, exist_ok=True)

    return dirs


def create_ada_file(path: Path, content: str) -> None:
    """Helper to create an Ada file with given content."""
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(content)


def create_gpr_file(path: Path, content: str) -> None:
    """Helper to create a GPR file with given content."""
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(content)


# Make helpers available to tests
pytest.create_ada_file = create_ada_file
pytest.create_gpr_file = create_gpr_file
