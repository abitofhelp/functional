#!/usr/bin/env python3
# ==============================================================================
# test_arch_guard_ada.py - Table-Driven Tests for arch_guard Ada Adapter
# ==============================================================================
# Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
# SPDX-License-Identifier: BSD-3-Clause
# ==============================================================================
"""
Table-driven test suite for arch_guard Ada language adapter.

Uses pytest.mark.parametrize for comprehensive, maintainable tests covering:
- Layer dependency rules (shared with all languages)
- Ada-specific: pragma vs aspect validation
- Ada-specific: file naming conventions
- Ada-specific: bounded strings in error types
- With clause parsing
"""

from dataclasses import dataclass
from pathlib import Path
from typing import Optional

import pytest

from arch_guard import ArchitectureGuard, ArchitectureViolation
from arch_guard.adapters import AdaAdapter


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
class WithClauseParseCase:
    """Test case for Ada with clause parsing."""
    id: str
    ada_code: str
    expected_packages: list[str]
    description: str = ""


@dataclass
class PragmaAspectCase:
    """Test case for pragma vs aspect validation."""
    id: str
    ada_code: str
    should_violate: bool
    pragma_name: str
    description: str = ""


@dataclass
class FileNamingCase:
    """Test case for Ada file naming validation."""
    id: str
    filename: str
    package_name: str
    should_violate: bool
    description: str = ""


@dataclass
class BoundedStringCase:
    """Test case for bounded string validation in error types."""
    id: str
    ada_code: str
    should_violate: bool
    description: str = ""


# =============================================================================
# Shared Layer Dependency Cases (Same rules as Go)
# =============================================================================

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

    # API layer (libraries): can import Application AND Domain
    LayerDependencyCase(
        id="api_can_import_domain",
        from_layer="api",
        to_layer="domain",
        should_violate=False,
        description="API is library facade - CAN access Domain",
    ),
    LayerDependencyCase(
        id="api_can_import_application",
        from_layer="api",
        to_layer="application",
        should_violate=False,
    ),
    LayerDependencyCase(
        id="api_cannot_import_infrastructure",
        from_layer="api",
        to_layer="infrastructure",
        should_violate=True,
        violation_type="FORBIDDEN_LATERAL_DEPENDENCY",
    ),
    LayerDependencyCase(
        id="api_cannot_import_presentation",
        from_layer="api",
        to_layer="presentation",
        should_violate=True,
        violation_type="FORBIDDEN_LATERAL_DEPENDENCY",
    ),

    # Bootstrap layer: can import ALL layers
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


# =============================================================================
# Ada-Specific: With Clause Parsing Cases
# =============================================================================

WITH_CLAUSE_PARSE_CASES = [
    WithClauseParseCase(
        id="single_with",
        ada_code="with Ada.Text_IO;\npackage Test is\nend Test;",
        expected_packages=["Ada.Text_IO"],
    ),
    WithClauseParseCase(
        id="multiple_with_clauses",
        ada_code="with Ada.Text_IO;\nwith Ada.Strings;\npackage Test is\nend Test;",
        expected_packages=["Ada.Text_IO", "Ada.Strings"],
    ),
    WithClauseParseCase(
        id="with_use_clause",
        ada_code="with Ada.Text_IO; use Ada.Text_IO;\npackage Test is\nend Test;",
        expected_packages=["Ada.Text_IO"],
    ),
    WithClauseParseCase(
        id="multiple_packages_single_with",
        ada_code="with Ada.Text_IO, Ada.Strings, Ada.Containers;\npackage Test is\nend Test;",
        expected_packages=["Ada.Text_IO", "Ada.Strings", "Ada.Containers"],
    ),
    WithClauseParseCase(
        id="layer_packages",
        ada_code="""with Domain.Entity;
with Domain.Value_Object;
with Application.Use_Case;
package Infrastructure.Adapter is
end Infrastructure.Adapter;
""",
        expected_packages=["Domain.Entity", "Domain.Value_Object", "Application.Use_Case"],
    ),
]


# =============================================================================
# Ada-Specific: Pragma vs Aspect Cases
# =============================================================================

PRAGMA_ASPECT_CASES = [
    PragmaAspectCase(
        id="pragma_pure_violation",
        ada_code="pragma Pure;\npackage Test is\nend Test;",
        should_violate=True,
        pragma_name="Pure",
        description="Should use 'with Pure' aspect instead",
    ),
    PragmaAspectCase(
        id="pragma_preelaborate_violation",
        ada_code="pragma Preelaborate;\npackage Test is\nend Test;",
        should_violate=True,
        pragma_name="Preelaborate",
        description="Should use 'with Preelaborate' aspect instead",
    ),
    PragmaAspectCase(
        id="pragma_elaborate_body_violation",
        ada_code="pragma Elaborate_Body;\npackage Test is\nend Test;",
        should_violate=True,
        pragma_name="Elaborate_Body",
    ),
    PragmaAspectCase(
        id="aspect_pure_ok",
        ada_code="package Test with Pure is\nend Test;",
        should_violate=False,
        pragma_name="Pure",
        description="Aspect syntax is correct",
    ),
    PragmaAspectCase(
        id="pragma_in_comment_ok",
        ada_code="-- pragma Pure; this is a comment\npackage Test with Pure is\nend Test;",
        should_violate=False,
        pragma_name="Pure",
        description="Pragma in comment should be ignored",
    ),
]


# =============================================================================
# Ada-Specific: File Naming Cases
# =============================================================================

FILE_NAMING_CASES = [
    FileNamingCase(
        id="correct_simple_package",
        filename="domain.ads",
        package_name="Domain",
        should_violate=False,
    ),
    FileNamingCase(
        id="correct_nested_package",
        filename="domain-entity.ads",
        package_name="Domain.Entity",
        should_violate=False,
    ),
    FileNamingCase(
        id="correct_deeply_nested",
        filename="domain-value_object-person.ads",
        package_name="Domain.Value_Object.Person",
        should_violate=False,
    ),
    FileNamingCase(
        id="wrong_filename",
        filename="wrong_name.ads",
        package_name="Domain.Entity",
        should_violate=True,
        description="File should be named domain-entity.ads",
    ),
    FileNamingCase(
        id="missing_prefix",
        filename="entity.ads",
        package_name="Domain.Entity",
        should_violate=True,
        description="File should be named domain-entity.ads",
    ),
]


# =============================================================================
# Ada-Specific: Bounded String Cases (for embedded safety)
# =============================================================================

BOUNDED_STRING_CASES = [
    BoundedStringCase(
        id="unbounded_string_in_error",
        ada_code="""package Domain.Error is
   type Error_Type is record
      Message : String;
   end record;
end Domain.Error;
""",
        should_violate=True,
        description="Error message should use Bounded_String for embedded safety",
    ),
    BoundedStringCase(
        id="bounded_string_ok",
        ada_code="""with Ada.Strings.Bounded;
package Domain.Error is
   package Message_Strings is new Ada.Strings.Bounded.Generic_Bounded_Length(256);
   subtype Bounded_String is Message_Strings.Bounded_String;

   type Error_Type is record
      Message : Bounded_String;
   end record;
end Domain.Error;
""",
        should_violate=False,
        description="Bounded_String is correct for embedded safety",
    ),
]


# =============================================================================
# Fixtures
# =============================================================================

@pytest.fixture
def ada_adapter():
    """Provide Ada adapter instance."""
    return AdaAdapter()


@pytest.fixture
def temp_ada_project(tmp_path):
    """
    Create a temporary Ada project structure for testing.

    Ada projects have sources under src/ with layer subdirectories.
    """
    root = tmp_path / "ada_project"
    src = root / "src"

    dirs = {
        "root": root,
        "src": src,
        "domain": src / "domain",
        "application": src / "application",
        "infrastructure": src / "infrastructure",
        "presentation": src / "presentation",
        "api": src / "api",
        "bootstrap": src / "bootstrap",
    }

    # Create all directories
    for directory in dirs.values():
        directory.mkdir(parents=True, exist_ok=True)

    # Create a basic alire.toml (for language detection)
    (root / "alire.toml").write_text("""
name = "test_project"
version = "0.1.0"
""")

    return dirs


@pytest.fixture
def temp_ada_error_project(tmp_path):
    """Create Ada project specifically for error type testing."""
    root = tmp_path / "ada_error_project"
    src = root / "src"
    domain = src / "domain"
    error_dir = domain / "error"

    for d in [root, src, domain, error_dir]:
        d.mkdir(parents=True, exist_ok=True)

    (root / "alire.toml").write_text('name = "test"\nversion = "0.1.0"\n')

    return {
        "root": root,
        "src": src,
        "domain": domain,
        "error": error_dir,
    }


# =============================================================================
# Helper Functions
# =============================================================================

def generate_ada_spec(from_layer: str, to_layer: str) -> str:
    """Generate Ada package spec that withs another layer."""
    return f"""with {to_layer.capitalize()}.Subpackage;

package {from_layer.capitalize()}.Test_Package is
   -- Test dependency from {from_layer} to {to_layer}
end {from_layer.capitalize()}.Test_Package;
"""


def create_ada_file(project: dict, layer: str, filename: str, content: str) -> Path:
    """Create an Ada file in the specified layer."""
    file_path = project[layer] / filename
    file_path.parent.mkdir(parents=True, exist_ok=True)
    file_path.write_text(content)
    return file_path


# =============================================================================
# Layer Dependency Tests (Table-Driven) - Shared Architecture Rules
# =============================================================================

class TestLayerDependencies:
    """Table-driven tests for layer dependency rules (same rules as Go)."""

    @pytest.mark.parametrize(
        "case",
        LAYER_DEPENDENCY_CASES,
        ids=lambda c: c.id,
    )
    def test_layer_dependency(self, temp_ada_project, ada_adapter, case: LayerDependencyCase):
        """Verify layer dependency rules are enforced correctly."""
        # Skip if layer doesn't exist in test project
        if case.from_layer not in temp_ada_project:
            pytest.skip(f"Layer {case.from_layer} not in test fixture")

        # Generate test file
        ada_content = generate_ada_spec(case.from_layer, case.to_layer)
        ada_path = create_ada_file(
            temp_ada_project,
            case.from_layer,
            f"{case.from_layer}-test_package.ads",
            ada_content,
        )

        # Validate
        guard = ArchitectureGuard(temp_ada_project["root"], ada_adapter)
        guard.validate_file(ada_path)

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
# With Clause Parsing Tests (Table-Driven)
# =============================================================================

class TestWithClauseParsing:
    """Table-driven tests for Ada with clause parsing."""

    @pytest.mark.parametrize(
        "case",
        WITH_CLAUSE_PARSE_CASES,
        ids=lambda c: c.id,
    )
    def test_with_clause_parsing(self, temp_ada_project, ada_adapter, case: WithClauseParseCase):
        """Verify with clauses are parsed correctly."""
        ada_path = create_ada_file(
            temp_ada_project,
            "domain",
            "test_parse.ads",
            case.ada_code,
        )

        # Parse with clauses
        imports = ada_adapter.extract_imports(ada_path)
        import_packages = [imp[1] for imp in imports]

        # Assert all expected packages found
        for expected in case.expected_packages:
            assert expected in import_packages, (
                f"Expected package '{expected}' not found in {import_packages}"
            )

        # Assert count matches
        assert len(import_packages) == len(case.expected_packages), (
            f"Package count mismatch: expected {len(case.expected_packages)}, "
            f"got {len(import_packages)}"
        )


# =============================================================================
# Ada-Specific: Pragma vs Aspect Tests (Table-Driven)
# =============================================================================

class TestPragmaVsAspect:
    """Table-driven tests for Ada pragma vs aspect validation."""

    @pytest.mark.parametrize(
        "case",
        PRAGMA_ASPECT_CASES,
        ids=lambda c: c.id,
    )
    def test_pragma_aspect(self, temp_ada_project, ada_adapter, case: PragmaAspectCase):
        """Verify pragma usage is flagged when aspect should be used."""
        ada_path = create_ada_file(
            temp_ada_project,
            "domain",
            "test_pragma.ads",
            case.ada_code,
        )

        # Run language-specific validations
        violations = ada_adapter.language_specific_validations(ada_path)

        pragma_violations = [v for v in violations
                            if v.violation_type == "PRAGMA_INSTEAD_OF_ASPECT"]

        if case.should_violate:
            assert len(pragma_violations) > 0, (
                f"Expected pragma violation for '{case.pragma_name}'"
            )
        else:
            assert len(pragma_violations) == 0, (
                f"Unexpected pragma violation: {pragma_violations}"
            )


# =============================================================================
# Ada-Specific: File Naming Tests (Table-Driven)
# =============================================================================

class TestFileNaming:
    """Table-driven tests for Ada file naming conventions."""

    @pytest.mark.parametrize(
        "case",
        FILE_NAMING_CASES,
        ids=lambda c: c.id,
    )
    def test_file_naming(self, temp_ada_project, ada_adapter, case: FileNamingCase):
        """Verify file naming convention enforcement."""
        ada_content = f"""package {case.package_name} is
end {case.package_name};
"""
        ada_path = create_ada_file(
            temp_ada_project,
            "domain",
            case.filename,
            ada_content,
        )

        # Run language-specific validations
        violations = ada_adapter.language_specific_validations(ada_path)

        naming_violations = [v for v in violations
                           if v.violation_type == "INCONSISTENT_FILE_NAMING"]

        if case.should_violate:
            assert len(naming_violations) > 0, (
                f"Expected naming violation for file '{case.filename}' "
                f"with package '{case.package_name}'"
            )
        else:
            assert len(naming_violations) == 0, (
                f"Unexpected naming violation: {naming_violations}"
            )


# =============================================================================
# Ada-Specific: Bounded String Tests (Table-Driven)
# =============================================================================

class TestBoundedStrings:
    """Table-driven tests for bounded string validation in error types."""

    @pytest.mark.parametrize(
        "case",
        BOUNDED_STRING_CASES,
        ids=lambda c: c.id,
    )
    def test_bounded_string_in_error(self, temp_ada_error_project, ada_adapter, case: BoundedStringCase):
        """Verify bounded string requirement for error message fields."""
        ada_path = temp_ada_error_project["error"] / "domain-error.ads"
        ada_path.write_text(case.ada_code)

        # Run language-specific validations
        violations = ada_adapter.language_specific_validations(ada_path)

        bounded_violations = [v for v in violations
                            if v.violation_type == "UNBOUNDED_STRING_IN_ERROR"]

        if case.should_violate:
            assert len(bounded_violations) > 0, (
                f"Expected unbounded string violation"
            )
        else:
            assert len(bounded_violations) == 0, (
                f"Unexpected bounded string violation: {bounded_violations}"
            )


# =============================================================================
# Intra-Layer Dependency Tests
# =============================================================================

class TestIntraLayerDependencies:
    """Tests for dependencies within the same layer."""

    @pytest.mark.parametrize(
        "layer",
        ["domain", "application", "infrastructure", "presentation", "api", "bootstrap"],
        ids=lambda l: f"intra_{l}",
    )
    def test_same_layer_imports_allowed(self, temp_ada_project, ada_adapter, layer: str):
        """Imports within the same layer should always be allowed."""
        if layer not in temp_ada_project:
            pytest.skip(f"Layer {layer} not in test fixture")

        ada_content = f"""with {layer.capitalize()}.Other_Package;

package {layer.capitalize()}.Test_Package is
end {layer.capitalize()}.Test_Package;
"""
        ada_path = create_ada_file(
            temp_ada_project,
            layer,
            f"{layer}-test_package.ads",
            ada_content,
        )

        guard = ArchitectureGuard(temp_ada_project["root"], ada_adapter)
        guard.validate_file(ada_path)

        assert len(guard.violations) == 0, (
            f"Intra-layer imports in {layer} should be allowed"
        )


# =============================================================================
# Integration Tests
# =============================================================================

class TestIntegration:
    """End-to-end integration tests for Ada projects."""

    def test_valid_hexagonal_architecture(self, temp_ada_project, ada_adapter):
        """Valid hexagonal architecture should pass all checks."""
        files = {
            "domain": (
                "domain-entity.ads",
                """package Domain.Entity with Pure is
   type Entity_ID is new Integer;
end Domain.Entity;
"""
            ),
            "application": (
                "application-use_case.ads",
                """with Domain.Entity;

package Application.Use_Case is
   procedure Process (ID : Domain.Entity.Entity_ID);
end Application.Use_Case;
"""
            ),
            "infrastructure": (
                "infrastructure-adapter.ads",
                """with Domain.Entity;
with Application.Use_Case;

package Infrastructure.Adapter is
   procedure Save (ID : Domain.Entity.Entity_ID);
end Infrastructure.Adapter;
"""
            ),
            "presentation": (
                "presentation-cli.ads",
                """with Application.Use_Case;

package Presentation.CLI is
   procedure Run;
end Presentation.CLI;
"""
            ),
        }

        # Create all files
        paths = []
        for layer, (filename, content) in files.items():
            path = create_ada_file(temp_ada_project, layer, filename, content)
            paths.append(path)

        # Validate all
        guard = ArchitectureGuard(temp_ada_project["root"], ada_adapter)
        for path in paths:
            guard.validate_file(path)

        # Filter out pragma/aspect violations (we used 'with Pure' correctly)
        layer_violations = [v for v in guard.violations
                          if v.violation_type not in ("PRAGMA_INSTEAD_OF_ASPECT",
                                                      "INCONSISTENT_FILE_NAMING")]

        assert len(layer_violations) == 0, (
            f"Valid architecture should have no layer violations: "
            f"{[v.violation_type for v in layer_violations]}"
        )


# =============================================================================
# Smoke Tests
# =============================================================================

@pytest.mark.smoke
def test_ada_adapter_instantiation():
    """Verify AdaAdapter can be instantiated."""
    adapter = AdaAdapter()
    assert adapter.name == "Ada"
    assert ".ads" in adapter.file_extensions
    assert ".adb" in adapter.file_extensions


@pytest.mark.smoke
def test_ada_adapter_test_imports():
    """Verify Ada test framework imports are defined."""
    adapter = AdaAdapter()
    assert "aunit" in adapter.forbidden_test_imports
    assert "ahven" in adapter.forbidden_test_imports


@pytest.mark.smoke
def test_ada_source_root():
    """Verify Ada uses src/ as source root."""
    adapter = AdaAdapter()
    assert adapter.source_root_subdir == "src"
