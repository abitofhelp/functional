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
- Shared fixtures for temporary Go project directories
- Common test utilities for Go architecture validation
"""

import sys
from pathlib import Path

import pytest

# Add scripts directory to Python path for importing modules under test
PROJECT_ROOT = Path(__file__).parent.parent.parent
SCRIPTS_DIR = PROJECT_ROOT / "scripts"
# Add scripts dir for arch_guard package (unified multi-language version)
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
def temp_go_project(tmp_path):
    """
    Create a temporary Go project structure for testing.

    Returns a dictionary with paths:
        - root: Project root
        - domain: Domain layer
        - application: Application layer
        - infrastructure: Infrastructure layer
        - presentation: Presentation layer
        - bootstrap: Bootstrap layer
    """
    root = tmp_path / "project"

    dirs = {
        "root": root,
        "domain": root / "domain",
        "application": root / "application",
        "infrastructure": root / "infrastructure",
        "presentation": root / "presentation",
        "bootstrap": root / "bootstrap",
        "cmd": root / "cmd" / "greeter",
    }

    # Create all directories
    for directory in dirs.values():
        directory.mkdir(parents=True, exist_ok=True)

    # Create root go.mod
    (root / "go.mod").write_text("module github.com/test/project\n\ngo 1.23\n")

    # Create layer go.mod files
    for layer in ["domain", "application", "infrastructure", "presentation", "bootstrap"]:
        layer_gomod = dirs[layer] / "go.mod"
        layer_gomod.write_text(f"module github.com/test/project/{layer}\n\ngo 1.23\n")

    return dirs


@pytest.fixture
def prod_go_project(tmp_path):
    """
    Create a production Go project structure WITHOUT 'test' in path.

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

    dirs = {
        "root": temp_dir,
        "domain": temp_dir / "domain",
        "application": temp_dir / "application",
        "infrastructure": temp_dir / "infrastructure",
        "presentation": temp_dir / "presentation",
        "bootstrap": temp_dir / "bootstrap",
    }

    # Create all directories
    for directory in dirs.values():
        directory.mkdir(parents=True, exist_ok=True)

    # Create root go.mod
    (temp_dir / "go.mod").write_text("module github.com/test/project\n\ngo 1.23\n")

    # Create layer go.mod files
    for layer in ["domain", "application", "infrastructure", "presentation", "bootstrap"]:
        layer_gomod = dirs[layer] / "go.mod"
        layer_gomod.write_text(f"module github.com/test/project/{layer}\n\ngo 1.23\n")

    return dirs


def create_go_file(path: Path, content: str) -> None:
    """Helper to create a Go file with given content."""
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(content)


def create_gomod_file(path: Path, module: str, requires: list = None) -> None:
    """Helper to create a go.mod file with given module and requirements."""
    path.parent.mkdir(parents=True, exist_ok=True)
    content = f"module {module}\n\ngo 1.23\n"
    if requires:
        content += "\nrequire (\n"
        for req in requires:
            content += f"\t{req}\n"
        content += ")\n"
    path.write_text(content)


# Make helpers available to tests
pytest.create_go_file = create_go_file
pytest.create_gomod_file = create_gomod_file
