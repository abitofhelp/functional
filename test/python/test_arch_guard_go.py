#!/usr/bin/env python3
# ==============================================================================
# test_arch_guard_go.py - Table-Driven Tests for arch_guard Go Adapter
# ==============================================================================
# Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
# SPDX-License-Identifier: BSD-3-Clause
# ==============================================================================
"""
Table-driven test suite for arch_guard Go language adapter.

Uses pytest.mark.parametrize for comprehensive, maintainable tests covering:
- Layer dependency rules (what can import what)
- Forbidden dependencies (bootstrap, lateral, presentation→domain)
- Test code detection in production files
- Import parsing correctness
"""

from dataclasses import dataclass
from pathlib import Path
from typing import Optional

import pytest

from arch_guard import ArchitectureGuard, ArchitectureViolation
from arch_guard.adapters import GoAdapter


# =============================================================================
# Test Case Data Structures
# =============================================================================

@dataclass
class LayerDependencyCase:
    """Test case for layer dependency validation."""
    id: str
    from_layer: str
    to_layer: str
    should_violate: bool
    violation_type: Optional[str] = None
    description: str = ""


@dataclass
class ImportParseCase:
    """Test case for import parsing validation."""
    id: str
    go_code: str
    expected_imports: list[str]
    description: str = ""


# =============================================================================
# Test Case Tables
# =============================================================================

# Layer dependency rules - comprehensive coverage
LAYER_DEPENDENCY_CASES = [
    # Domain layer: ZERO dependencies allowed
    LayerDependencyCase(
        id="domain_cannot_import_application",
        from_layer="domain",
        to_layer="application",
        should_violate=True,
        violation_type="ILLEGAL_LAYER_DEPENDENCY",
        description="Domain is innermost - no outward dependencies",
    ),
    LayerDependencyCase(
        id="domain_cannot_import_infrastructure",
        from_layer="domain",
        to_layer="infrastructure",
        should_violate=True,
        violation_type="ILLEGAL_LAYER_DEPENDENCY",
    ),
    LayerDependencyCase(
        id="domain_cannot_import_presentation",
        from_layer="domain",
        to_layer="presentation",
        should_violate=True,
        violation_type="ILLEGAL_LAYER_DEPENDENCY",
    ),
    LayerDependencyCase(
        id="domain_cannot_import_bootstrap",
        from_layer="domain",
        to_layer="bootstrap",
        should_violate=True,
        violation_type="FORBIDDEN_BOOTSTRAP_DEPENDENCY",
    ),

    # Application layer: can only import Domain
    LayerDependencyCase(
        id="application_can_import_domain",
        from_layer="application",
        to_layer="domain",
        should_violate=False,
        description="Application depends on Domain (allowed)",
    ),
    LayerDependencyCase(
        id="application_cannot_import_infrastructure",
        from_layer="application",
        to_layer="infrastructure",
        should_violate=True,
        violation_type="ILLEGAL_LAYER_DEPENDENCY",
    ),
    LayerDependencyCase(
        id="application_cannot_import_presentation",
        from_layer="application",
        to_layer="presentation",
        should_violate=True,
        violation_type="ILLEGAL_LAYER_DEPENDENCY",
    ),
    LayerDependencyCase(
        id="application_cannot_import_bootstrap",
        from_layer="application",
        to_layer="bootstrap",
        should_violate=True,
        violation_type="FORBIDDEN_BOOTSTRAP_DEPENDENCY",
    ),

    # Infrastructure layer: can import Application + Domain
    LayerDependencyCase(
        id="infrastructure_can_import_domain",
        from_layer="infrastructure",
        to_layer="domain",
        should_violate=False,
    ),
    LayerDependencyCase(
        id="infrastructure_can_import_application",
        from_layer="infrastructure",
        to_layer="application",
        should_violate=False,
    ),
    LayerDependencyCase(
        id="infrastructure_cannot_import_presentation",
        from_layer="infrastructure",
        to_layer="presentation",
        should_violate=True,
        violation_type="FORBIDDEN_LATERAL_DEPENDENCY",
        description="No lateral dependencies allowed",
    ),
    LayerDependencyCase(
        id="infrastructure_cannot_import_bootstrap",
        from_layer="infrastructure",
        to_layer="bootstrap",
        should_violate=True,
        violation_type="FORBIDDEN_BOOTSTRAP_DEPENDENCY",
    ),

    # Presentation layer: can ONLY import Application (NOT Domain!)
    LayerDependencyCase(
        id="presentation_can_import_application",
        from_layer="presentation",
        to_layer="application",
        should_violate=False,
    ),
    LayerDependencyCase(
        id="presentation_cannot_import_domain",
        from_layer="presentation",
        to_layer="domain",
        should_violate=True,
        violation_type="PRESENTATION_IMPORTS_DOMAIN",
        description="CRITICAL: Presentation must use Application re-exports",
    ),
    LayerDependencyCase(
        id="presentation_cannot_import_infrastructure",
        from_layer="presentation",
        to_layer="infrastructure",
        should_violate=True,
        violation_type="FORBIDDEN_LATERAL_DEPENDENCY",
    ),
    LayerDependencyCase(
        id="presentation_cannot_import_bootstrap",
        from_layer="presentation",
        to_layer="bootstrap",
        should_violate=True,
        violation_type="FORBIDDEN_BOOTSTRAP_DEPENDENCY",
    ),

    # Bootstrap layer: can import ALL layers (composition root)
    LayerDependencyCase(
        id="bootstrap_can_import_domain",
        from_layer="bootstrap",
        to_layer="domain",
        should_violate=False,
    ),
    LayerDependencyCase(
        id="bootstrap_can_import_application",
        from_layer="bootstrap",
        to_layer="application",
        should_violate=False,
    ),
    LayerDependencyCase(
        id="bootstrap_can_import_infrastructure",
        from_layer="bootstrap",
        to_layer="infrastructure",
        should_violate=False,
    ),
    LayerDependencyCase(
        id="bootstrap_can_import_presentation",
        from_layer="bootstrap",
        to_layer="presentation",
        should_violate=False,
    ),
]

# Import parsing test cases
IMPORT_PARSE_CASES = [
    ImportParseCase(
        id="single_import",
        go_code='package main\n\nimport "fmt"\n',
        expected_imports=["fmt"],
    ),
    ImportParseCase(
        id="grouped_imports",
        go_code='package main\n\nimport (\n\t"context"\n\t"fmt"\n)\n',
        expected_imports=["context", "fmt"],
    ),
    ImportParseCase(
        id="aliased_import",
        go_code='package main\n\nimport (\n\tapperr "github.com/test/project/application/error"\n)\n',
        expected_imports=["github.com/test/project/application/error"],
    ),
    ImportParseCase(
        id="mixed_imports_with_comments",
        go_code='''package main

import (
    "context"  // standard lib
    "fmt"

    domerr "github.com/test/project/domain/error"  // domain errors
    "github.com/test/project/domain/valueobject"
)
''',
        expected_imports=[
            "context",
            "fmt",
            "github.com/test/project/domain/error",
            "github.com/test/project/domain/valueobject",
        ],
    ),
    ImportParseCase(
        id="dot_import",
        go_code='package main\n\nimport (\n\t. "github.com/test/project/domain"\n)\n',
        expected_imports=["github.com/test/project/domain"],
    ),
    ImportParseCase(
        id="blank_import",
        go_code='package main\n\nimport (\n\t_ "github.com/lib/pq"\n)\n',
        expected_imports=["github.com/lib/pq"],
    ),
]


# =============================================================================
# Fixtures
# =============================================================================

@pytest.fixture
def go_adapter():
    """Provide Go adapter instance."""
    return GoAdapter()


@pytest.fixture
def go_guard(temp_go_project, go_adapter):
    """Provide ArchitectureGuard configured for Go."""
    return ArchitectureGuard(temp_go_project["root"], go_adapter)


# =============================================================================
# Helper Functions
# =============================================================================

def generate_go_file(from_layer: str, to_layer: str) -> str:
    """Generate Go source that imports one layer from another."""
    return f'''package {from_layer}pkg

import (
    "github.com/test/project/{to_layer}/subpkg"
)

func Use() {{
    _ = subpkg.Something
}}
'''


def create_test_file(project: dict, layer: str, filename: str, content: str) -> Path:
    """Create a Go file in the specified layer."""
    file_path = project[layer] / filename
    file_path.parent.mkdir(parents=True, exist_ok=True)
    file_path.write_text(content)
    return file_path


# =============================================================================
# Layer Dependency Tests (Table-Driven)
# =============================================================================

class TestLayerDependencies:
    """Table-driven tests for layer dependency rules."""

    @pytest.mark.parametrize(
        "case",
        LAYER_DEPENDENCY_CASES,
        ids=lambda c: c.id,
    )
    def test_layer_dependency(self, temp_go_project, go_adapter, case: LayerDependencyCase):
        """Verify layer dependency rules are enforced correctly."""
        # Generate test file
        go_content = generate_go_file(case.from_layer, case.to_layer)
        go_path = create_test_file(
            temp_go_project,
            case.from_layer,
            "test_dep.go",
            go_content,
        )

        # Validate
        guard = ArchitectureGuard(temp_go_project["root"], go_adapter)
        guard.validate_file(go_path)

        # Assert
        if case.should_violate:
            assert len(guard.violations) > 0, (
                f"Expected violation for {case.from_layer} → {case.to_layer}"
            )
            if case.violation_type:
                violation_types = [v.violation_type for v in guard.violations]
                assert case.violation_type in violation_types, (
                    f"Expected {case.violation_type}, got {violation_types}"
                )
        else:
            assert len(guard.violations) == 0, (
                f"Unexpected violation for {case.from_layer} → {case.to_layer}: "
                f"{[v.violation_type for v in guard.violations]}"
            )


# =============================================================================
# Import Parsing Tests (Table-Driven)
# =============================================================================

class TestImportParsing:
    """Table-driven tests for Go import statement parsing."""

    @pytest.mark.parametrize(
        "case",
        IMPORT_PARSE_CASES,
        ids=lambda c: c.id,
    )
    def test_import_parsing(self, temp_go_project, go_adapter, case: ImportParseCase):
        """Verify import statements are parsed correctly."""
        # Create test file
        go_path = create_test_file(
            temp_go_project,
            "domain",
            "parse_test.go",
            case.go_code,
        )

        # Parse imports
        imports = go_adapter.extract_imports(go_path)
        import_paths = [imp[1] for imp in imports]

        # Assert all expected imports found
        for expected in case.expected_imports:
            assert expected in import_paths, (
                f"Expected import '{expected}' not found in {import_paths}"
            )

        # Assert count matches
        assert len(import_paths) == len(case.expected_imports), (
            f"Import count mismatch: expected {len(case.expected_imports)}, "
            f"got {len(import_paths)}"
        )


# =============================================================================
# Test Code Detection Tests
# =============================================================================

class TestTestCodeDetection:
    """Tests for detecting test code in production files."""

    @pytest.mark.parametrize(
        "filename,should_skip",
        [
            pytest.param("service.go", False, id="production_file"),
            pytest.param("service_test.go", True, id="test_file"),
        ],
    )
    def test_test_file_detection(self, go_adapter, filename: str, should_skip: bool):
        """Verify test files are correctly identified."""
        file_path = Path(f"/fake/path/{filename}")
        assert go_adapter.is_test_file(file_path) == should_skip

    def test_testing_import_in_production_flagged(self, prod_go_project, go_adapter):
        """Production code importing 'testing' package should be flagged."""
        go_content = '''package usecase

import (
    "testing"
)

func BadCode() {
    _ = testing.T{}
}
'''
        go_path = create_test_file(
            prod_go_project,
            "application",
            "bad.go",
            go_content,
        )

        guard = ArchitectureGuard(prod_go_project["root"], go_adapter)
        guard.validate_file(go_path)

        violations = [v for v in guard.violations
                      if v.violation_type == "TEST_CODE_IN_PRODUCTION"]
        assert len(violations) > 0, "Should flag testing import in production code"

    def test_testing_import_in_test_file_allowed(self, temp_go_project, go_adapter):
        """Test files can import testing package."""
        go_content = '''package usecase

import (
    "testing"
)

func TestSomething(t *testing.T) {
    // OK in test files
}
'''
        go_path = create_test_file(
            temp_go_project,
            "application",
            "usecase_test.go",
            go_content,
        )

        guard = ArchitectureGuard(temp_go_project["root"], go_adapter)
        guard.validate_file(go_path)

        violations = [v for v in guard.violations
                      if v.violation_type == "TEST_CODE_IN_PRODUCTION"]
        assert len(violations) == 0, "Test files should be allowed to import testing"


# =============================================================================
# Intra-Layer Dependency Tests
# =============================================================================

class TestIntraLayerDependencies:
    """Tests for dependencies within the same layer."""

    @pytest.mark.parametrize(
        "layer",
        ["domain", "application", "infrastructure", "presentation", "bootstrap"],
        ids=lambda l: f"intra_{l}",
    )
    def test_same_layer_imports_allowed(self, temp_go_project, go_adapter, layer: str):
        """Imports within the same layer should always be allowed."""
        go_content = f'''package subpkg

import (
    "github.com/test/project/{layer}/otherpkg"
)

func Use() {{
    _ = otherpkg.Something
}}
'''
        go_path = create_test_file(temp_go_project, layer, "subpkg/file.go", go_content)

        guard = ArchitectureGuard(temp_go_project["root"], go_adapter)
        guard.validate_file(go_path)

        assert len(guard.violations) == 0, (
            f"Intra-layer imports in {layer} should be allowed"
        )


# =============================================================================
# Integration Tests
# =============================================================================

class TestIntegration:
    """End-to-end integration tests."""

    def test_valid_hexagonal_architecture(self, temp_go_project, go_adapter):
        """Valid hexagonal architecture should pass all checks."""
        files = {
            "domain": '''package entity

type EntityID int64

func NewEntityID(value int64) EntityID {
    return EntityID(value)
}
''',
            "application": '''package usecase

import (
    "github.com/test/project/domain/entity"
)

func Process(id entity.EntityID) error {
    return nil
}
''',
            "infrastructure": '''package adapter

import (
    "github.com/test/project/domain/entity"
    "github.com/test/project/application/port"
)

type Repository struct{}

func (r *Repository) Save(id entity.EntityID) error {
    return nil
}
''',
            "presentation": '''package cli

import (
    "github.com/test/project/application/usecase"
)

func Run() int {
    usecase.Process(0)
    return 0
}
''',
        }

        # Create all files
        paths = []
        for layer, content in files.items():
            path = create_test_file(temp_go_project, layer, f"{layer}.go", content)
            paths.append(path)

        # Validate all
        guard = ArchitectureGuard(temp_go_project["root"], go_adapter)
        for path in paths:
            guard.validate_file(path)

        assert len(guard.violations) == 0, (
            f"Valid architecture should have no violations: "
            f"{[v.violation_type for v in guard.violations]}"
        )

    def test_multiple_violations_detected(self, temp_go_project, go_adapter):
        """Multiple violations in single file should all be detected."""
        go_content = '''package entity

import (
    "github.com/test/project/application/usecase"
    "github.com/test/project/infrastructure/adapter"
    "github.com/test/project/bootstrap/cli"
)

func Bad() {
    // Domain importing 3 forbidden layers
}
'''
        go_path = create_test_file(temp_go_project, "domain", "bad.go", go_content)

        guard = ArchitectureGuard(temp_go_project["root"], go_adapter)
        guard.validate_file(go_path)

        # Should have at least 3 violations (app, infra, bootstrap)
        assert len(guard.violations) >= 3, (
            f"Expected multiple violations, got {len(guard.violations)}"
        )


# =============================================================================
# Smoke Tests
# =============================================================================

@pytest.mark.smoke
def test_arch_guard_module_imports():
    """Verify arch_guard module can be imported."""
    import arch_guard
    assert hasattr(arch_guard, "ArchitectureGuard")
    assert hasattr(arch_guard, "ArchitectureViolation")


@pytest.mark.smoke
def test_go_adapter_instantiation():
    """Verify GoAdapter can be instantiated."""
    adapter = GoAdapter()
    assert adapter.name == "Go"
    assert ".go" in adapter.file_extensions


@pytest.mark.smoke
def test_architecture_guard_with_real_project(project_root):
    """Verify ArchitectureGuard works on real project."""
    from arch_guard.arch_guard import detect_language

    detected = detect_language(project_root)
    if detected != "go":
        pytest.skip(f"Skipping Go smoke test: detected language is '{detected}'")

    adapter = GoAdapter()
    guard = ArchitectureGuard(project_root, adapter)

    assert guard is not None
    assert "domain" in guard.layers_present
    assert "application" in guard.layers_present
    assert len(guard.violations) == 0
