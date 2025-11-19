#!/usr/bin/env python3
# ==============================================================================
# test_arch_guard.py - Comprehensive Test Suite for arch_guard.py
# ==============================================================================
# Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
# SPDX-License-Identifier: BSD-3-Clause
# ==============================================================================
"""
Comprehensive test suite for arch_guard.py architectural validation.

Tests all 11 architectural rules:
1. Layer dependency enforcement (Domain, Application, Infrastructure, etc.)
2. Library_Interface validation (prevents transitive dependencies)
3. Code quality rules (pragmas, naming, bounded strings, test imports)
4. False positive scenarios (comments, string literals)

Test Organization:
- TestLayerDependencies: Layer dependency rules
- TestLibraryInterface: Stand-alone library enforcement
- TestCodeQuality: Pragma, naming, string validation
- TestFalsePositives: Ensure valid code doesn't trigger violations
- TestIntegration: End-to-end validation on real project structure
"""

import sys
from pathlib import Path
from typing import List

import pytest

# Import the module under test
import arch_guard
from arch_guard import ArchitectureGuard, ArchitectureViolation


class TestLayerDependencies:
    """Test layer dependency rule enforcement."""

    def test_domain_cannot_depend_on_application(self, temp_ada_project):
        """Domain layer must not depend on any other layer."""
        ada_content = """
with Application.Service;  -- ❌ Domain cannot depend on Application

package Domain.Entity is
   type ID is range 1 .. 1_000_000;
end Domain.Entity;
"""
        ada_path = temp_ada_project["domain"] / "domain-entity.ads"
        pytest.create_ada_file(ada_path, ada_content)

        guard = ArchitectureGuard(temp_ada_project["src"])
        guard.validate_file(ada_path)

        # Should have violation: Domain depends on Application
        assert guard.has_violations()
        violations = guard.get_violations()
        assert any("application" in v.details.lower() for v in violations)

    def test_domain_with_no_dependencies_passes(self, temp_ada_project):
        """Domain layer with no dependencies should pass."""
        ada_content = """
package Domain.Entity
   with Pure
is
   type Entity_ID is range 1 .. 1_000_000;
end Domain.Entity;
"""
        ada_path = temp_ada_project["domain"] / "domain-entity.ads"
        pytest.create_ada_file(ada_path, ada_content)

        guard = ArchitectureGuard(temp_ada_project["src"])
        guard.validate_file(ada_path)

        # Should have no violations
        assert not guard.has_violations()

    def test_application_can_depend_on_domain(self, temp_ada_project):
        """Application layer can depend on Domain."""
        ada_content = """
with Domain.Entity;  -- ✅ Application can depend on Domain

package Application.Service is
   procedure Process (ID : Domain.Entity.Entity_ID);
end Application.Service;
"""
        ada_path = temp_ada_project["application"] / "application-service.ads"
        pytest.create_ada_file(ada_path, ada_content)

        guard = ArchitectureGuard(temp_ada_project["src"])
        guard.validate_file(ada_path)

        # Should have no violations
        assert not guard.has_violations()

    def test_application_cannot_depend_on_infrastructure(self, temp_ada_project):
        """Application layer must not depend on Infrastructure."""
        ada_content = """
with Domain.Entity;
with Infrastructure.Adapter;  -- ❌ Application cannot depend on Infrastructure

package Application.Service is
   procedure Execute;
end Application.Service;
"""
        ada_path = temp_ada_project["application"] / "application-service.ads"
        pytest.create_ada_file(ada_path, ada_content)

        guard = ArchitectureGuard(temp_ada_project["src"])
        guard.validate_file(ada_path)

        # Should have violation
        assert guard.has_violations()
        violations = guard.get_violations()
        assert any("infrastructure" in v.details.lower() for v in violations)

    def test_infrastructure_can_depend_on_application_and_domain(self, temp_ada_project):
        """Infrastructure layer can depend on Application and Domain."""
        ada_content = """
with Domain.Entity;
with Application.Port.Outbound.Writer;

package Infrastructure.Adapter.Console_Writer is
   procedure Write (Msg : String);
end Infrastructure.Adapter.Console_Writer;
"""
        ada_path = temp_ada_project["infrastructure"] / "adapter" / "infrastructure-adapter-console_writer.ads"
        pytest.create_ada_file(ada_path, ada_content)

        guard = ArchitectureGuard(temp_ada_project["src"])
        guard.validate_file(ada_path)

        # Should have no violations
        assert not guard.has_violations()

    def test_presentation_cannot_depend_on_domain(self, temp_ada_project):
        """Presentation layer must not directly depend on Domain."""
        ada_content = """
with Application.Port;
with Domain.Entity;  -- ❌ Presentation must not directly access Domain

package Presentation.View is
   procedure Display;
end Presentation.View;
"""
        ada_path = temp_ada_project["presentation"] / "presentation-view.ads"
        pytest.create_ada_file(ada_path, ada_content)

        guard = ArchitectureGuard(temp_ada_project["src"])
        guard.validate_file(ada_path)

        # Should have violation
        assert guard.has_violations()
        violations = guard.get_violations()
        assert any("domain" in v.details.lower() for v in violations)

    def test_presentation_cannot_depend_on_infrastructure(self, temp_ada_project):
        """Presentation and Infrastructure cannot have lateral dependencies."""
        ada_content = """
with Application.Port;
with Infrastructure.Adapter;  -- ❌ Lateral dependency forbidden

package Presentation.View is
   procedure Display;
end Presentation.View;
"""
        ada_path = temp_ada_project["presentation"] / "presentation-view.ads"
        pytest.create_ada_file(ada_path, ada_content)

        guard = ArchitectureGuard(temp_ada_project["src"])
        guard.validate_file(ada_path)

        # Should have violation
        assert guard.has_violations()
        violations = guard.get_violations()
        assert any("infrastructure" in v.details.lower() for v in violations)


class TestCodeQuality:
    """Test code quality rules: pragmas, naming, bounded strings, test imports."""

    def test_pragma_pure_detected(self, temp_ada_project):
        """Pragma Pure should be flagged (use aspect instead)."""
        ada_content = """
package Domain.Entity is
   pragma Pure;  -- ❌ Should use "with Pure" aspect

   type Entity_ID is range 1 .. 1_000_000;
end Domain.Entity;
"""
        ada_path = temp_ada_project["domain"] / "domain-entity.ads"
        pytest.create_ada_file(ada_path, ada_content)

        guard = ArchitectureGuard(temp_ada_project["src"])
        guard.validate_file(ada_path)

        # Should have violation: pragma Pure
        assert guard.has_violations()
        violations = guard.get_violations()
        assert any("pragma" in v.violation_type.lower() and "Pure" in v.details for v in violations)

    def test_pragma_preelaborate_detected(self, temp_ada_project):
        """Pragma Preelaborate should be flagged."""
        ada_content = """
package Domain.Types is
   pragma Preelaborate;  -- ❌ Should use "with Preelaborate"

   type Count is range 0 .. 1_000;
end Domain.Types;
"""
        ada_path = temp_ada_project["domain"] / "domain-types.ads"
        pytest.create_ada_file(ada_path, ada_content)

        guard = ArchitectureGuard(temp_ada_project["src"])
        guard.validate_file(ada_path)

        assert guard.has_violations()
        violations = guard.get_violations()
        assert any("Preelaborate" in v.details for v in violations)

    def test_aspect_usage_passes(self, temp_ada_project):
        """Using aspects instead of pragmas should pass."""
        ada_content = """
package Domain.Entity
   with Pure  -- ✅ Correct usage
is
   type Entity_ID is range 1 .. 1_000_000;
end Domain.Entity;
"""
        ada_path = temp_ada_project["domain"] / "domain-entity.ads"
        pytest.create_ada_file(ada_path, ada_content)

        guard = ArchitectureGuard(temp_ada_project["src"])
        guard.validate_file(ada_path)

        # Should have no pragma violations
        pragma_violations = [v for v in guard.get_violations() if "pragma" in v.violation_type.lower()]
        assert len(pragma_violations) == 0

    def test_test_framework_import_in_production_code(self, prod_ada_project):
        """Production code must not import test frameworks."""
        ada_content = """
with Test_Framework;  -- ❌ Production code importing test framework

package Application.Service is
   procedure Process;
end Application.Service;
"""
        ada_path = prod_ada_project["application"] / "application-service.ads"
        pytest.create_ada_file(ada_path, ada_content)

        guard = ArchitectureGuard(prod_ada_project["src"])
        guard.validate_file(ada_path)

        # Should have violation: test framework import
        assert guard.has_violations()
        violations = guard.get_violations()

        test_framework_violations = [v for v in violations
                                     if v.violation_type == 'TEST_CODE_IN_PRODUCTION']
        assert len(test_framework_violations) > 0
        assert "Test_Framework" in test_framework_violations[0].details

    def test_test_framework_import_in_test_code_allowed(self, temp_ada_project):
        """Test code CAN import test frameworks (path contains 'test')."""
        ada_content = """
with Test_Framework;  -- ✅ Test code can import test frameworks

package Test_Application_Service is
   procedure Run_Tests;
end Test_Application_Service;
"""
        # This path contains 'test' so validation should be skipped
        ada_path = temp_ada_project["application"] / "test-application-service.ads"
        pytest.create_ada_file(ada_path, ada_content)

        guard = ArchitectureGuard(temp_ada_project["src"])
        guard.validate_file(ada_path)

        # Should NOT have violation because path contains 'test'
        test_violations = [v for v in guard.get_violations()
                          if "test" in v.violation_type.lower()]
        assert len(test_violations) == 0

    def test_file_naming_mismatch(self, temp_ada_project):
        """File name must match package name."""
        ada_content = """
-- File is domain-entity.ads but package is Domain.Service (mismatch!)
package Domain.Service is
   procedure Execute;
end Domain.Service;
"""
        ada_path = temp_ada_project["domain"] / "domain-entity.ads"
        pytest.create_ada_file(ada_path, ada_content)

        guard = ArchitectureGuard(temp_ada_project["src"])
        guard.validate_file(ada_path)

        # Should have violation: naming mismatch
        assert guard.has_violations()
        violations = guard.get_violations()
        assert any("naming" in v.violation_type.lower() for v in violations)

        # Check details mention the mismatch
        naming_violations = [v for v in violations if "naming" in v.violation_type.lower()]
        assert len(naming_violations) > 0
        assert "domain-entity" in naming_violations[0].details.lower()
        assert "domain.service" in naming_violations[0].details.lower()

    def test_file_naming_correct(self, temp_ada_project):
        """Correct file naming should pass."""
        ada_content = """
-- File: domain-entity.ads, Package: Domain.Entity ✅
package Domain.Entity is
   type ID is range 1 .. 1_000_000;
end Domain.Entity;
"""
        ada_path = temp_ada_project["domain"] / "domain-entity.ads"
        pytest.create_ada_file(ada_path, ada_content)

        guard = ArchitectureGuard(temp_ada_project["src"])
        guard.validate_file(ada_path)

        # Should have no naming violations
        naming_violations = [v for v in guard.get_violations()
                           if "naming" in v.violation_type.lower()]
        assert len(naming_violations) == 0

    def test_error_type_with_unbounded_string(self, temp_ada_project):
        """Error types must use Bounded_String, not String."""
        ada_content = """
package Domain.Error is
   type Error_Code is (Success, Invalid_Input, Not_Found);

   type Domain_Error is record
      Code    : Error_Code;
      Message : String;  -- ❌ Should use Bounded_String
   end record;
end Domain.Error;
"""
        ada_path = temp_ada_project["domain"] / "error" / "domain-error.ads"
        pytest.create_ada_file(ada_path, ada_content)

        guard = ArchitectureGuard(temp_ada_project["src"])
        guard.validate_file(ada_path)

        # Should have violation: unbounded String in error type
        assert guard.has_violations()
        violations = guard.get_violations()

        # Check for UNBOUNDED_STRING_IN_ERROR violation
        string_violations = [v for v in violations
                           if v.violation_type == 'UNBOUNDED_STRING_IN_ERROR']
        assert len(string_violations) > 0
        assert "Bounded_String" in string_violations[0].details

    def test_error_type_with_bounded_string_passes(self, temp_ada_project):
        """Error types with Bounded_String should pass."""
        ada_content = """
with Ada.Strings.Bounded;

package Domain.Error is
   package Error_Strings is new Ada.Strings.Bounded.Generic_Bounded_Length (256);

   type Error_Code is (Success, Invalid_Input, Not_Found);

   type Domain_Error is record
      Code    : Error_Code;
      Message : Error_Strings.Bounded_String;  -- ✅ Correct
   end record;
end Domain.Error;
"""
        ada_path = temp_ada_project["domain"] / "error" / "domain-error.ads"
        pytest.create_ada_file(ada_path, ada_content)

        guard = ArchitectureGuard(temp_ada_project["src"])
        guard.validate_file(ada_path)

        # Should have no bounded string violations
        string_violations = [v for v in guard.get_violations()
                           if "bounded" in v.violation_type.lower()]
        assert len(string_violations) == 0


class TestPackageNamingFix:
    """Test package naming violation detection - distinguishes top-level from nested packages."""

    def test_package_instantiation_ignored(self, temp_ada_project):
        """Package instantiations (is new) should NOT require separate files."""
        ada_content = """
package Domain.Value_Object.Identifier
  with Preelaborate
is
   -- ✅ This is a package instantiation, NOT a declaration
   -- Should NOT trigger INCONSISTENT_FILE_NAMING
   package Identifier_Strings is new
     Ada.Strings.Bounded.Generic_Bounded_Length
       (Max => 64);

   type Identifier_Type is private;
private
   type Identifier_Type is record
      ID : Identifier_Strings.Bounded_String;
   end record;
end Domain.Value_Object.Identifier;
"""
        ada_path = temp_ada_project["domain"] / "value_object" / "domain-value_object-identifier.ads"
        pytest.create_ada_file(ada_path, ada_content)

        guard = ArchitectureGuard(temp_ada_project["src"])
        guard.validate_file(ada_path)

        # Should NOT have naming violation for Identifier_Strings
        naming_violations = [v for v in guard.get_violations()
                           if v.violation_type == 'INCONSISTENT_FILE_NAMING']

        # If there are any naming violations, none should mention Identifier_Strings
        for v in naming_violations:
            assert "Identifier_Strings" not in v.details, \
                "Package instantiation should not trigger naming violation"

    def test_nested_generic_package_ignored(self, temp_ada_project):
        """Generic packages nested inside parent should NOT require separate files."""
        ada_content = """
package Domain.Error.Result
  with Preelaborate
is
   -- ✅ This is a nested generic package declaration
   -- Should NOT trigger INCONSISTENT_FILE_NAMING
   generic
      type T is private;
   package Generic_Result is
      type Result is private;
      function Ok (Value : T) return Result;
   private
      type Result is null record;
   end Generic_Result;

end Domain.Error.Result;
"""
        ada_path = temp_ada_project["domain"] / "error" / "domain-error-result.ads"
        pytest.create_ada_file(ada_path, ada_content)

        guard = ArchitectureGuard(temp_ada_project["src"])
        guard.validate_file(ada_path)

        # Should NOT have naming violation for Generic_Result
        naming_violations = [v for v in guard.get_violations()
                           if v.violation_type == 'INCONSISTENT_FILE_NAMING']

        for v in naming_violations:
            assert "Generic_Result" not in v.details, \
                "Nested generic package should not trigger naming violation"

    def test_result_impl_instantiation_ignored(self, temp_ada_project):
        """Result Impl package instantiation should be ignored."""
        ada_content = """
with Domain.Error;
with Domain.Error.Result;

package Domain.Value_Object.Identifier.Result
  with Preelaborate
is
   -- ✅ This is a package instantiation (is new)
   -- Should NOT trigger INCONSISTENT_FILE_NAMING
   package Impl is new Domain.Error.Result.Generic_Result
     (T => Domain.Value_Object.Identifier.Identifier_Type);

   subtype Result is Impl.Result;

   function Ok (Value : Identifier_Type) return Result renames Impl.Ok;
end Domain.Value_Object.Identifier.Result;
"""
        ada_path = temp_ada_project["domain"] / "value_object" / "domain-value_object-identifier-result.ads"
        pytest.create_ada_file(ada_path, ada_content)

        guard = ArchitectureGuard(temp_ada_project["src"])
        guard.validate_file(ada_path)

        # Should NOT have naming violation for Impl
        naming_violations = [v for v in guard.get_violations()
                           if v.violation_type == 'INCONSISTENT_FILE_NAMING']

        for v in naming_violations:
            assert "Impl" not in v.details, \
                "Impl package instantiation should not trigger naming violation"

    def test_error_strings_instantiation_ignored(self, temp_ada_project):
        """Error_Strings package instantiation should be ignored."""
        ada_content = """
with Ada.Strings.Bounded;

package Domain.Error
  with Preelaborate
is
   -- ✅ This is a package instantiation
   -- Should NOT trigger INCONSISTENT_FILE_NAMING
   package Error_Strings is new
     Ada.Strings.Bounded.Generic_Bounded_Length (Max => 512);

   type Error_Kind is (Validation_Error, Infrastructure_Error);

   type Error_Type is record
      Kind    : Error_Kind;
      Message : Error_Strings.Bounded_String;
   end record;
end Domain.Error;
"""
        ada_path = temp_ada_project["domain"] / "error" / "domain-error.ads"
        pytest.create_ada_file(ada_path, ada_content)

        guard = ArchitectureGuard(temp_ada_project["src"])
        guard.validate_file(ada_path)

        # Should NOT have naming violation for Error_Strings
        naming_violations = [v for v in guard.get_violations()
                           if v.violation_type == 'INCONSISTENT_FILE_NAMING']

        for v in naming_violations:
            assert "Error_Strings" not in v.details, \
                "Error_Strings instantiation should not trigger naming violation"

    def test_indented_package_declaration_ignored(self, temp_ada_project):
        """Indented package declarations are nested and should be ignored."""
        ada_content = """
package Domain.Container is
   -- ✅ Indented package = nested, should NOT require separate file
   package Helpers is
      procedure Validate;
   end Helpers;

   type Container_Type is null record;
end Domain.Container;
"""
        ada_path = temp_ada_project["domain"] / "domain-container.ads"
        pytest.create_ada_file(ada_path, ada_content)

        guard = ArchitectureGuard(temp_ada_project["src"])
        guard.validate_file(ada_path)

        # Should NOT have naming violation for Helpers
        naming_violations = [v for v in guard.get_violations()
                           if v.violation_type == 'INCONSISTENT_FILE_NAMING']

        for v in naming_violations:
            assert "Helpers" not in v.details, \
                "Indented nested package should not trigger naming violation"

    def test_top_level_package_mismatch_still_flagged(self, temp_ada_project):
        """Top-level package naming mismatches should STILL be detected."""
        ada_content = """
-- File: domain-entity.ads
-- Package: Domain.Service (MISMATCH!)
package Domain.Service
  with Pure
is
   procedure Execute;
end Domain.Service;
"""
        ada_path = temp_ada_project["domain"] / "domain-entity.ads"
        pytest.create_ada_file(ada_path, ada_content)

        guard = ArchitectureGuard(temp_ada_project["src"])
        guard.validate_file(ada_path)

        # Should STILL have naming violation for top-level mismatch
        naming_violations = [v for v in guard.get_violations()
                           if v.violation_type == 'INCONSISTENT_FILE_NAMING']

        assert len(naming_violations) > 0, \
            "Top-level package naming mismatch should still be flagged"

        # Check it mentions the mismatch
        assert any("domain-entity" in v.details.lower() for v in naming_violations), \
            "Violation should mention the file name"
        assert any("domain.service" in v.details.lower() for v in naming_violations), \
            "Violation should mention the package name"

    def test_top_level_package_correct_naming_passes(self, temp_ada_project):
        """Correct top-level package naming should pass (no false positives)."""
        ada_content = """
-- File: domain-value_object-identifier.ads
-- Package: Domain.Value_Object.Identifier ✅ CORRECT
package Domain.Value_Object.Identifier
  with Preelaborate
is
   type Identifier_Type is private;
private
   type Identifier_Type is null record;
end Domain.Value_Object.Identifier;
"""
        ada_path = temp_ada_project["domain"] / "value_object" / "domain-value_object-identifier.ads"
        pytest.create_ada_file(ada_path, ada_content)

        guard = ArchitectureGuard(temp_ada_project["src"])
        guard.validate_file(ada_path)

        # Should NOT have naming violations
        naming_violations = [v for v in guard.get_violations()
                           if v.violation_type == 'INCONSISTENT_FILE_NAMING']

        assert len(naming_violations) == 0, \
            "Correctly named top-level package should not trigger violation"

    def test_multiple_instantiations_all_ignored(self, temp_ada_project):
        """Multiple package instantiations in same file should all be ignored."""
        ada_content = """
with Ada.Strings.Bounded;
with Ada.Containers.Vectors;

package Domain.Collections
  with Preelaborate
is
   -- ✅ All of these are instantiations - should be ignored
   package Name_Strings is new
     Ada.Strings.Bounded.Generic_Bounded_Length (Max => 100);

   package ID_Strings is new
     Ada.Strings.Bounded.Generic_Bounded_Length (Max => 64);

   type Item is null record;

   package Item_Vectors is new
     Ada.Containers.Vectors (Natural, Item);

   type Collection is null record;
end Domain.Collections;
"""
        ada_path = temp_ada_project["domain"] / "domain-collections.ads"
        pytest.create_ada_file(ada_path, ada_content)

        guard = ArchitectureGuard(temp_ada_project["src"])
        guard.validate_file(ada_path)

        # Should NOT have naming violations for any instantiations
        naming_violations = [v for v in guard.get_violations()
                           if v.violation_type == 'INCONSISTENT_FILE_NAMING']

        for v in naming_violations:
            assert "Name_Strings" not in v.details
            assert "ID_Strings" not in v.details
            assert "Item_Vectors" not in v.details

    def test_file_without_nested_packages_correct_naming(self, temp_ada_project):
        """File with ONLY top-level package, correctly named, should pass."""
        ada_content = """
-- File: domain-entity.ads
-- Package: Domain.Entity
-- No nested packages - pure top-level only
package Domain.Entity
  with Pure
is
   type Entity_ID is range 1 .. 1_000_000;

   function Make_ID (Value : Natural) return Entity_ID;
end Domain.Entity;
"""
        ada_path = temp_ada_project["domain"] / "domain-entity.ads"
        pytest.create_ada_file(ada_path, ada_content)

        guard = ArchitectureGuard(temp_ada_project["src"])
        guard.validate_file(ada_path)

        # Should have ZERO naming violations
        naming_violations = [v for v in guard.get_violations()
                           if v.violation_type == 'INCONSISTENT_FILE_NAMING']

        assert len(naming_violations) == 0, \
            "File with no nested packages and correct top-level naming should pass"

    def test_file_with_nested_packages_correct_toplevel_naming(self, temp_ada_project):
        """File with nested packages, top-level correctly named, should pass."""
        ada_content = """
-- File: domain-value_object-identifier.ads
-- Package: Domain.Value_Object.Identifier (CORRECT)
-- Has nested packages (instantiations) - should ignore them, check top-level only
package Domain.Value_Object.Identifier
  with Preelaborate
is
   -- Nested instantiation - should be ignored
   package Identifier_Strings is new
     Ada.Strings.Bounded.Generic_Bounded_Length (Max => 64);

   -- Nested helper package - should be ignored
   package Validators is
      function Is_Valid (S : String) return Boolean;
   end Validators;

   type Identifier_Type is private;
private
   type Identifier_Type is record
      ID : Identifier_Strings.Bounded_String;
   end record;
end Domain.Value_Object.Identifier;
"""
        ada_path = temp_ada_project["domain"] / "value_object" / "domain-value_object-identifier.ads"
        pytest.create_ada_file(ada_path, ada_content)

        guard = ArchitectureGuard(temp_ada_project["src"])
        guard.validate_file(ada_path)

        # Should have ZERO naming violations
        # Top-level is correctly named, nested packages are ignored
        naming_violations = [v for v in guard.get_violations()
                           if v.violation_type == 'INCONSISTENT_FILE_NAMING']

        assert len(naming_violations) == 0, \
            "File with nested packages but correct top-level naming should pass"

    def test_file_with_nested_packages_incorrect_toplevel_naming(self, temp_ada_project):
        """File with nested packages, top-level incorrectly named, should flag ONLY top-level."""
        ada_content = """
-- File: domain-value_object-identifier.ads (filename)
-- Package: Domain.Value_Object.Name (MISMATCH!)
-- Has nested packages - should ignore them, flag top-level only
package Domain.Value_Object.Name
  with Preelaborate
is
   -- Nested instantiation - should NOT be flagged
   package Name_Strings is new
     Ada.Strings.Bounded.Generic_Bounded_Length (Max => 64);

   -- Nested helper - should NOT be flagged
   package Helpers is
      procedure Validate;
   end Helpers;

   type Name_Type is private;
private
   type Name_Type is null record;
end Domain.Value_Object.Name;
"""
        ada_path = temp_ada_project["domain"] / "value_object" / "domain-value_object-identifier.ads"
        pytest.create_ada_file(ada_path, ada_content)

        guard = ArchitectureGuard(temp_ada_project["src"])
        guard.validate_file(ada_path)

        # Should have EXACTLY ONE naming violation for top-level mismatch
        naming_violations = [v for v in guard.get_violations()
                           if v.violation_type == 'INCONSISTENT_FILE_NAMING']

        assert len(naming_violations) == 1, \
            "Should have exactly 1 violation for top-level mismatch (not nested packages)"

        # Verify it's flagging the TOP-LEVEL mismatch
        violation = naming_violations[0]
        assert "domain-value_object-identifier" in violation.details.lower(), \
            "Should mention the filename"
        assert "domain.value_object.name" in violation.details.lower(), \
            "Should mention the top-level package name"

        # Verify it's NOT flagging the nested packages
        assert "Name_Strings" not in violation.details, \
            "Should NOT flag nested instantiation"
        assert "Helpers" not in violation.details, \
            "Should NOT flag nested helper package"


class TestFalsePositives:
    """Test that valid code doesn't trigger false violations."""

    def test_pragma_in_comment_allowed(self, temp_ada_project):
        """Pragma mentioned in comments should not trigger violation."""
        ada_content = """
-- Historical note: We used to use "pragma Pure" here,
-- but switched to aspects: "with Pure" for Ada 2012+ style.
package Domain.Entity
   with Pure
is
   type ID is range 1 .. 1_000_000;
end Domain.Entity;
"""
        ada_path = temp_ada_project["domain"] / "domain-entity.ads"
        pytest.create_ada_file(ada_path, ada_content)

        guard = ArchitectureGuard(temp_ada_project["src"])
        guard.validate_file(ada_path)

        # Should not flag pragma in comment
        pragma_violations = [v for v in guard.get_violations() if "pragma" in v.violation_type.lower()]
        assert len(pragma_violations) == 0

    def test_domain_in_string_literal_allowed(self, temp_ada_project):
        """Word 'Domain' in string literals should not trigger violation."""
        ada_content = """
package Application.Service is
   -- This is fine - "Domain" in a string literal
   Error_Msg : constant String := "Invalid domain entity detected";

   procedure Log_Message (Msg : String);
end Application.Service;
"""
        ada_path = temp_ada_project["application"] / "application-service.ads"
        pytest.create_ada_file(ada_path, ada_content)

        guard = ArchitectureGuard(temp_ada_project["src"])
        guard.validate_file(ada_path)

        # Should not flag "domain" in string literal as dependency violation
        assert not guard.has_violations()

    def test_intra_layer_dependencies_allowed(self, temp_ada_project):
        """Dependencies within same layer are allowed."""
        ada_content = """
with Domain.Entity;  -- ✅ Same layer dependency OK

package Domain.Service is
   procedure Execute (ID : Domain.Entity.Entity_ID);
end Domain.Service;
"""
        ada_path = temp_ada_project["domain"] / "domain-service.ads"
        pytest.create_ada_file(ada_path, ada_content)

        guard = ArchitectureGuard(temp_ada_project["src"])
        guard.validate_file(ada_path)

        # Intra-layer dependencies are OK
        assert not guard.has_violations()


class TestIntegration:
    """Integration tests using actual project structure."""

    def test_multiple_violations_in_single_file(self, temp_ada_project):
        """Test detecting multiple violations in a single file."""
        ada_content = """
with Test_Framework;  -- ❌ VIOLATION 1: test import in production (skipped in test paths)
with Application.Service;  -- ❌ VIOLATION 2: Domain depending on Application

package Domain.Service is
   pragma Pure;       -- ❌ VIOLATION 3: pragma instead of aspect
   pragma Inline;     -- ❌ VIOLATION 4: another pragma

   procedure Execute;
end Domain.Service;
"""
        ada_path = temp_ada_project["domain"] / "domain-service.ads"
        pytest.create_ada_file(ada_path, ada_content)

        guard = ArchitectureGuard(temp_ada_project["src"])
        guard.validate_file(ada_path)

        # Should detect multiple violations
        assert guard.has_violations()
        violations = guard.get_violations()

        # At least 3 violations (test framework check is skipped for test paths)
        assert len(violations) >= 3

        # Verify we have pragma and dependency violations
        violation_types = [v.violation_type for v in violations]
        assert 'PRAGMA_INSTEAD_OF_ASPECT' in violation_types
        assert 'ILLEGAL_LAYER_DEPENDENCY' in violation_types

    def test_valid_hexagonal_architecture(self, temp_ada_project):
        """Test that valid hexagonal architecture passes all checks."""
        # Domain - no dependencies
        domain_entity = """
package Domain.Entity
   with Pure
is
   type Entity_ID is range 1 .. 1_000_000;
end Domain.Entity;
"""
        pytest.create_ada_file(
            temp_ada_project["domain"] / "domain-entity.ads",
            domain_entity
        )

        # Application - depends on Domain
        app_service = """
with Domain.Entity;

package Application.Service is
   procedure Process (ID : Domain.Entity.Entity_ID);
end Application.Service;
"""
        pytest.create_ada_file(
            temp_ada_project["application"] / "application-service.ads",
            app_service
        )

        # Infrastructure - depends on Application + Domain
        infra_adapter = """
with Domain.Entity;
with Application.Service;

package Infrastructure.Adapter is
   procedure Execute;
end Infrastructure.Adapter;
"""
        pytest.create_ada_file(
            temp_ada_project["infrastructure"] / "infrastructure-adapter.ads",
            infra_adapter
        )

        # Presentation - depends only on Application
        pres_view = """
with Application.Service;

package Presentation.View is
   procedure Display;
end Presentation.View;
"""
        pytest.create_ada_file(
            temp_ada_project["presentation"] / "presentation-view.ads",
            pres_view
        )

        # Validate all files
        guard = ArchitectureGuard(temp_ada_project["src"])

        for ada_file in [
            temp_ada_project["domain"] / "domain-entity.ads",
            temp_ada_project["application"] / "application-service.ads",
            temp_ada_project["infrastructure"] / "infrastructure-adapter.ads",
            temp_ada_project["presentation"] / "presentation-view.ads",
        ]:
            guard.validate_file(ada_file)

        # All should pass - this is valid hexagonal architecture
        assert not guard.has_violations()


@pytest.mark.smoke
def test_arch_guard_module_imports():
    """Smoke test: Verify arch_guard module can be imported."""
    assert hasattr(arch_guard, 'ArchitectureGuard')
    assert hasattr(arch_guard, 'ArchitectureViolation')


@pytest.mark.smoke
def test_architecture_guard_instantiation(project_root):
    """Smoke test: Verify ArchitectureGuard can be instantiated."""
    guard = ArchitectureGuard(project_root / "src")
    assert guard is not None
    assert not guard.has_violations()  # Fresh instance has no violations
    assert guard.get_violation_count() == 0


@pytest.mark.smoke
def test_architecture_guard_helper_methods(project_root):
    """Smoke test: Verify helper methods work correctly."""
    guard = ArchitectureGuard(project_root / "src")

    # Initially no violations
    assert not guard.has_violations()
    assert guard.get_violation_count() == 0
    assert len(guard.get_violations()) == 0

    # Clear violations (should be no-op)
    guard.clear_violations()
    assert guard.get_violation_count() == 0
