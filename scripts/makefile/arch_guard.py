#!/usr/bin/env python3
"""
Hexagonal/Clean Architecture Guard for Ada Projects

Validates layer dependencies in a hybrid DDD/Clean/Hexagonal architecture:

Architecture Model (Concentric Spheres - Center-Seeking Dependencies):

  ┌──────────────────────────────────────────────────────────────┐
  │                  Bootstrap (outermost)                       │
  │        Bootstrap → All layers ✓ (can depend on any)          │
  │        All layers → Bootstrap ✗ (forbidden)                  │
  │                                                              │
  │  ┌───────────────────────────────────────────────────────┐  │
  │  │  Presentation        Infrastructure                   │  │
  │  │  (outer half)        (outer half)                     │  │
  │  │       ↓                    ↓         ↓                │  │
  │  │       │                    └─────────┘                │  │
  │  │       │                         ↓                     │  │
  │  │       │              Presentation ↔ Infrastructure ✗  │  │
  │  │       │                    (lateral forbidden)        │  │
  │  │       │                         ↓                     │  │
  │  │  ┌────┼─────────────────────────┼──────────────────┐  │  │
  │  │  │    ↓                         ↓                  │  │  │
  │  │  │              Application (middle sphere)        │  │  │
  │  │  │                       ↓                          │  │  │
  │  │  │         NO transitive Domain exposure           │  │  │
  │  │  │              to Presentation layer              │  │  │
  │  │  │                       ↓                          │  │  │
  │  │  │    ┌──────────────────────────────────┐         │  │  │
  │  │  │    │      Domain (inner core)         │         │  │  │
  │  │  │    │   ZERO dependencies outward      │         │  │  │
  │  │  │    │   (pure business logic)          │         │  │  │
  │  │  │    └──────────────────────────────────┘         │  │  │
  │  │  │                                                  │  │  │
  │  │  └──────────────────────────────────────────────────┘  │  │
  │  │                                                         │  │
  │  └─────────────────────────────────────────────────────────┘  │
  │                                                              │
  └──────────────────────────────────────────────────────────────┘

  Legend:
    ↓ = Allowed dependency (center-seeking)
    ✗ = Forbidden dependency
    ↔ = Lateral dependency (forbidden between Presentation ↔ Infrastructure)

Layer Rules (All dependencies flow INWARD toward Domain):
- Domain: ZERO dependencies (innermost core - pure business logic)
- Application: Depends ONLY on Domain (middle sphere)
  * MUST NOT expose Domain types in public interfaces (no transitive exposure)
- Infrastructure: Depends on Application + Domain (outer half-sphere, NOT Presentation)
- Presentation: Depends ONLY on Application (outer half-sphere, NOT Infrastructure, NOT Domain)
  * MUST NOT have transitive access to Domain through Application interfaces
- Bootstrap: Depends on all layers (outermost), but NO layer can depend on Bootstrap
- Fundamental Rule: ALL dependencies are center-seeking (toward Domain)

Important: This tool validates compile-time dependencies (with clauses) only.
Preventing transitive Domain exposure requires careful API design in Application layer.

GPRbuild Transitive Dependency Control:
------------------------------------------------------------------------------
✅ SOLUTION IMPLEMENTED: Stand-Alone Library with Library_Interface

GPRbuild provides a mechanism to prevent transitive dependency exposure through
the Stand-Alone Library feature with explicit Library_Interface declarations.

Strategy Implemented in This Project:

1. **Stand-Alone Library with Library_Interface** (✅ ACTIVE):
   - Application.gpr declares: for Library_Standalone use "standard"
   - Application.gpr lists explicit Library_Interface (Application.* packages only)
   - Domain.* packages are NOT listed in Library_Interface
   - GPRbuild enforces: Presentation can ONLY access packages in Library_Interface
   - Result: Presentation CANNOT "with Domain.*" even transitively (compile error)

2. **Limited/Private With** (Ada 2022):
   - Use "limited with" or "private with" in Application layer
   - Prevents automatic transitive visibility of Domain types
   - Requires explicit re-export if Domain types should be visible
   - Best practice for controlling transitive exposure

3. **Interface Segregation** (Design Pattern):
   - Application provides DTOs/view models instead of Domain entities
   - Presentation never sees Domain types directly
   - Requires mapping layer (Domain → DTO) in Application
   - Most verbose but strictest isolation

4. **This Architecture Guard Script**:
   - Validates that Presentation doesn't "with Domain.*" packages directly
   - Cannot detect indirect access through Application interfaces
   - Should be run in CI/CD pipeline for continuous validation

Recommendation for library (and other libraries):
- Use strategy #1: Separate GPR projects per layer
- Document in README that Application must not expose Domain types to Presentation
- Run this script in CI/CD to catch direct dependency violations
- Consider strategy #3 for executable projects with Presentation layers

Exit Codes:
  0: All architecture rules satisfied
  1: Architecture violations detected
  2: Script error (missing directories, etc.)
"""

import os
import sys
import re
from pathlib import Path
from typing import Set, Dict, List, Tuple
from dataclasses import dataclass


@dataclass
class ArchitectureViolation:
    """Represents a single architecture rule violation"""
    file_path: str
    line_number: int
    violation_type: str
    details: str


class ArchitectureGuard:
    """Validates hexagonal architecture layer dependencies"""

    # Layer dependency rules (layer -> allowed dependencies)
    # All dependencies flow INWARD toward the domain core
    LAYER_RULES = {
        'domain': set(),  # ZERO dependencies (innermost core)
        'application': {'domain'},  # Middle sphere - depends on core only
        'infrastructure': {'application', 'domain'},  # Outer half-sphere - center-seeking
        'api': {'application', 'domain'},  # API/Facade layer - can use Application and Domain
        'presentation': {'application'},  # Outer half-sphere - depends ONLY on Application (NOT Domain)
        'bootstrap': {'domain', 'application', 'infrastructure', 'presentation', 'api'}  # Outermost
    }

    # Forbidden test framework imports in production code
    FORBIDDEN_TEST_IMPORTS = ['test_framework', 'aunit', 'ahven', 'gnattest']

    # Pragmas that should be aspects instead
    PRAGMA_TO_ASPECT = {
        'Pure': 'with Pure',
        'Preelaborate': 'with Preelaborate',
        'Elaborate_Body': 'with Elaborate_Body',
        'Pack': 'with Pack',
        'Inline': 'with Inline',
        'Volatile': 'with Volatile',
        'Atomic': 'with Atomic',
    }

    # No layer can depend on bootstrap
    FORBIDDEN_DEPENDENCY = 'bootstrap'

    def __init__(self, src_root: Path):
        """
        Initialize architecture guard

        Args:
            src_root: Root source directory containing layer subdirectories
        """
        self.src_root = src_root
        self.violations: List[ArchitectureViolation] = []
        self.layers_present = self._detect_layers()
        self.gpr_config_valid = False

    def _detect_layers(self) -> Set[str]:
        """Detect which layers exist in the project"""
        layers = set()
        if not self.src_root.exists():
            print(f"Warning: Source root {self.src_root} does not exist")
            return layers

        print("Layer Detection:")
        for layer in sorted(self.LAYER_RULES.keys()):
            layer_dir = self.src_root / layer
            if layer_dir.exists() and layer_dir.is_dir():
                layers.add(layer)
                print(f"  ✓ {layer:15} - present")
            else:
                print(f"  ○ {layer:15} - not present (skipped)")

        return layers

    def _extract_with_clauses(self, file_path: Path) -> List[Tuple[int, str]]:
        """
        Extract all 'with' clauses from an Ada file

        Returns:
            List of (line_number, package_name) tuples
        """
        with_clauses = []

        try:
            with open(file_path, 'r', encoding='utf-8') as f:
                for line_num, line in enumerate(f, start=1):
                    # Match: with Package.Name;
                    # Handle multiple packages: with A, B, C;
                    match = re.match(r'^\s*with\s+([^;]+);', line, re.IGNORECASE)
                    if match:
                        packages_str = match.group(1)
                        # Split by comma for multiple packages in one with clause
                        packages = [pkg.strip() for pkg in packages_str.split(',')]
                        for pkg in packages:
                            with_clauses.append((line_num, pkg))
        except Exception as e:
            print(f"Warning: Could not read {file_path}: {e}")

        return with_clauses

    def _get_layer_from_package(self, package_name: str) -> str | None:
        """
        Determine which layer a package belongs to based on naming convention

        Args:
            package_name: Ada package name (e.g., "Domain.Error.Result")

        Returns:
            Layer name or None if not a layered package
        """
        # Normalize to lowercase for comparison
        normalized = package_name.lower()

        # Check each layer (longest match first to handle nested packages)
        for layer in sorted(self.LAYER_RULES.keys(), key=len, reverse=True):
            if normalized.startswith(layer + '.') or normalized == layer:
                return layer

        return None

    def _get_file_layer(self, file_path: Path) -> str | None:
        """Determine which layer a file belongs to based on its path"""
        try:
            relative_path = file_path.relative_to(self.src_root)
            parts = relative_path.parts
            if parts and parts[0] in self.LAYER_RULES:
                return parts[0]
        except ValueError:
            pass
        return None

    def _validate_application_gpr_config(self) -> bool:
        """
        Validate that Application layer GPR is configured as stand-alone library
        with Library_Interface that excludes Domain packages.

        This enforces the rule: "Application MUST NOT transitively expose Domain to Presentation"

        Returns:
            True if configuration is valid, False otherwise
        """
        if 'application' not in self.layers_present:
            # No application layer, nothing to check
            return True

        app_gpr_path = self.src_root / 'application' / 'application.gpr'
        if not app_gpr_path.exists():
            print(f"\n❌ ERROR: Application GPR file not found: {app_gpr_path}")
            print("   Application layer must have application.gpr file")
            return False

        has_standalone = False
        has_interface = False
        interface_packages = []

        try:
            with open(app_gpr_path, 'r', encoding='utf-8') as f:
                content = f.read()

                # Check for Library_Standalone (must not be commented out)
                if re.search(r'^\s*for\s+Library_Standalone\s+use\s+"standard"\s*;', content, re.MULTILINE | re.IGNORECASE):
                    has_standalone = True

                # Extract Library_Interface packages
                interface_match = re.search(
                    r'for\s+Library_Interface\s+use\s*\((.*?)\);',
                    content,
                    re.DOTALL | re.IGNORECASE
                )
                if interface_match:
                    has_interface = True
                    # Parse package names from the list
                    packages_str = interface_match.group(1)
                    # Extract strings like "Package.Name"
                    package_matches = re.findall(r'"([^"]+)"', packages_str)
                    interface_packages = [pkg.strip() for pkg in package_matches]

        except Exception as e:
            print(f"\n❌ ERROR: Could not read Application GPR file: {e}")
            return False

        # Validation Results
        valid = True

        if not has_standalone:
            print(f"\n❌ ERROR: Application layer GPR missing stand-alone library configuration")
            print(f"   File: {app_gpr_path}")
            print(f"   Required: for Library_Standalone use \"standard\";")
            print(f"   WHY: Prevents transitive Domain exposure to Presentation layer")
            valid = False

        if not has_interface:
            print(f"\n❌ ERROR: Application layer GPR missing Library_Interface declaration")
            print(f"   File: {app_gpr_path}")
            print(f"   Required: for Library_Interface use (...);")
            print(f"   WHY: Explicitly defines public API, preventing Domain package visibility")
            valid = False
        elif interface_packages:
            # Check that no Domain.* packages are in the interface
            domain_packages = [pkg for pkg in interface_packages if pkg.lower().startswith('domain.')]
            if domain_packages:
                print(f"\n❌ ERROR: Application Library_Interface contains Domain packages!")
                print(f"   File: {app_gpr_path}")
                print(f"   Forbidden packages in Library_Interface:")
                for pkg in domain_packages:
                    print(f"      - {pkg}")
                print(f"   WHY: Domain packages MUST NOT be exposed to Presentation layer")
                valid = False
            else:
                print(f"\n✅ Application GPR configuration valid:")
                print(f"   - Stand-alone library: enabled")
                print(f"   - Library_Interface: defined ({len(interface_packages)} packages)")
                print(f"   - Domain packages: correctly excluded")

        return valid

    def _validate_no_test_imports(self, file_path: Path) -> None:
        """
        Ensure production code doesn't import test frameworks

        Test code should stay in test/ directory only
        """
        # Skip if this is a test file
        if 'test' in str(file_path).lower():
            return

        with_clauses = self._extract_with_clauses(file_path)

        for line_num, package_name in with_clauses:
            pkg_lower = package_name.lower()
            for forbidden in self.FORBIDDEN_TEST_IMPORTS:
                if forbidden in pkg_lower:
                    self.violations.append(ArchitectureViolation(
                        file_path=str(file_path),
                        line_number=line_num,
                        violation_type='TEST_CODE_IN_PRODUCTION',
                        details=f"Production code cannot import test framework: {package_name}"
                    ))

    def _validate_pragma_usage(self, file_path: Path) -> None:
        """
        Check that aspects are used instead of pragmas where applicable

        Ada 2012+ style prefers aspects over pragmas
        """
        try:
            with open(file_path, 'r', encoding='utf-8') as f:
                content = f.read()

            for line_num, line in enumerate(content.split('\n'), start=1):
                # Check for pragma usage
                for pragma, aspect in self.PRAGMA_TO_ASPECT.items():
                    # Match pragma statement (not in comments)
                    if re.match(rf'^\s*pragma\s+{pragma}\s*[;\(]', line, re.IGNORECASE):
                        # Skip if it's in a comment
                        if '--' in line and line.index('--') < line.lower().index('pragma'):
                            continue

                        self.violations.append(ArchitectureViolation(
                            file_path=str(file_path),
                            line_number=line_num,
                            violation_type='PRAGMA_INSTEAD_OF_ASPECT',
                            details=f"Use {aspect} instead of pragma {pragma}"
                        ))
        except Exception as e:
            print(f"Warning: Could not validate pragma usage in {file_path}: {e}")

    def _validate_file_naming(self, file_path: Path) -> None:
        """
        Validate file naming conventions for TOP-LEVEL packages only

        Checks:
        1. File name should match top-level package name
        2. Consistent namespace prefixes across project

        IGNORES (not violations):
        - Package instantiations (package X is new Y)
        - Nested package declarations (indented or inside parent)
        - Generic packages declared inside parent packages

        Only top-level package declarations need their own files.
        """
        try:
            with open(file_path, 'r', encoding='utf-8') as f:
                content = f.read()
                lines = content.split('\n')

                nesting_level = 0
                in_generic_declaration = False

                for line_num, line in enumerate(lines, start=1):
                    stripped = line.strip()

                    # Track generic declarations (nested packages inside generics are not top-level)
                    if stripped.startswith('generic'):
                        in_generic_declaration = True
                        continue

                    # Check for package declaration
                    if stripped.startswith('package'):
                        # SKIP: Package instantiations (is new) - these don't need separate files
                        if ' is new ' in line or '\tis new ' in line:
                            continue

                        # SKIP: Nested packages (indented) - not top-level declarations
                        if line != line.lstrip() and 'package' in line:
                            continue

                        # Track nesting level
                        if 'is new' not in stripped:
                            nesting_level += 1

                            # ONLY CHECK top-level packages (nesting_level == 1)
                            # AND not inside a generic declaration
                            if nesting_level == 1 and not in_generic_declaration:
                                # Match package declaration (handles aspects like "with Pure")
                                match = re.match(r'package\s+(body\s+)?([A-Za-z0-9_.]+)', stripped, re.IGNORECASE)
                                if match:
                                    package_name = match.group(2)

                                    # Convert package name to expected file name
                                    # Package.Name -> package-name.ads
                                    expected_filename = package_name.lower().replace('.', '-')
                                    actual_filename = file_path.stem.lower()

                                    # Check if filename matches top-level package name
                                    if actual_filename != expected_filename:
                                        self.violations.append(ArchitectureViolation(
                                            file_path=str(file_path),
                                            line_number=line_num,
                                            violation_type='INCONSISTENT_FILE_NAMING',
                                            details=f"File name '{file_path.name}' doesn't match package '{package_name}' (expected: {expected_filename}{file_path.suffix})"
                                        ))

                            # Reset generic flag after processing the package declaration
                            if in_generic_declaration:
                                in_generic_declaration = False

                    # Track end of packages to maintain correct nesting level
                    if stripped.startswith('end') and ';' in stripped:
                        nesting_level = max(0, nesting_level - 1)

        except Exception as e:
            print(f"Warning: Could not validate file naming for {file_path}: {e}")

    def _validate_bounded_strings_for_errors(self, file_path: Path) -> None:
        """
        Ensure error types use bounded strings, not unbounded String

        For embedded safety and predictable memory usage
        """
        # Only check error-related files
        if 'error' not in str(file_path).lower():
            return

        try:
            with open(file_path, 'r', encoding='utf-8') as f:
                content = f.read()

            # Look for record types with Message : String field
            # This is a heuristic - might need refinement
            for line_num, line in enumerate(content.split('\n'), start=1):
                # Skip comments
                code_part = line.split('--')[0] if '--' in line else line

                # Match: Message : String (but not Bounded_String in the code part)
                if re.search(r'\bMessage\s*:\s*String\s*;', code_part) and 'Bounded_String' not in code_part:
                    self.violations.append(ArchitectureViolation(
                        file_path=str(file_path),
                        line_number=line_num,
                        violation_type='UNBOUNDED_STRING_IN_ERROR',
                        details="Error message field should use Bounded_String, not String (for embedded safety)"
                    ))
        except Exception as e:
            print(f"Warning: Could not validate bounded strings in {file_path}: {e}")

    def validate_file(self, file_path: Path) -> None:
        """
        Validate a single Ada file against architecture rules

        Args:
            file_path: Path to .ads or .adb file
        """
        # Run all validation checks
        self._validate_no_test_imports(file_path)
        self._validate_pragma_usage(file_path)
        self._validate_file_naming(file_path)
        self._validate_bounded_strings_for_errors(file_path)

        current_layer = self._get_file_layer(file_path)
        if not current_layer:
            # File not in a known layer, skip dependency checks
            return

        if current_layer not in self.layers_present:
            # Layer not present in project, skip dependency checks
            return

        allowed_deps = self.LAYER_RULES[current_layer]
        with_clauses = self._extract_with_clauses(file_path)

        for line_num, package_name in with_clauses:
            dependency_layer = self._get_layer_from_package(package_name)

            if not dependency_layer:
                # Not a layered package (e.g., Ada standard library), OK
                continue

            if dependency_layer not in self.layers_present:
                # Dependency layer doesn't exist in project, skip
                continue

            # Skip intra-layer dependencies (same layer is always OK)
            if dependency_layer == current_layer:
                continue

            # ═══════════════════════════════════════════════════════════════
            # CENTER-SEEKING VALIDATION (All dependencies flow toward Domain)
            # ═══════════════════════════════════════════════════════════════

            # Check for bootstrap dependency violation
            if dependency_layer == self.FORBIDDEN_DEPENDENCY:
                self.violations.append(ArchitectureViolation(
                    file_path=str(file_path),
                    line_number=line_num,
                    violation_type='FORBIDDEN_BOOTSTRAP_DEPENDENCY',
                    details=f"Layer '{current_layer}' cannot depend on '{self.FORBIDDEN_DEPENDENCY}'"
                ))
                continue

            # Check for forbidden lateral dependencies (Presentation ↔ Infrastructure)
            if current_layer == 'presentation' and dependency_layer == 'infrastructure':
                self.violations.append(ArchitectureViolation(
                    file_path=str(file_path),
                    line_number=line_num,
                    violation_type='FORBIDDEN_LATERAL_DEPENDENCY',
                    details=f"Presentation cannot depend on Infrastructure (package: {package_name})"
                ))
                continue

            if current_layer == 'infrastructure' and dependency_layer == 'presentation':
                self.violations.append(ArchitectureViolation(
                    file_path=str(file_path),
                    line_number=line_num,
                    violation_type='FORBIDDEN_LATERAL_DEPENDENCY',
                    details=f"Infrastructure cannot depend on Presentation (package: {package_name})"
                ))
                continue

            # Check if inter-layer dependency is allowed
            if dependency_layer not in allowed_deps:
                self.violations.append(ArchitectureViolation(
                    file_path=str(file_path),
                    line_number=line_num,
                    violation_type='ILLEGAL_LAYER_DEPENDENCY',
                    details=f"Layer '{current_layer}' cannot depend on '{dependency_layer}' (package: {package_name})"
                ))

    def validate_all(self) -> bool:
        """
        Validate all Ada files in the source tree

        Returns:
            True if all files pass validation, False if violations found
        """
        if not self.layers_present:
            print("⚠ No architecture layers detected - skipping validation")
            return True

        print(f"\nValidating architecture rules for layers: {', '.join(sorted(self.layers_present))}\n")

        # Step 1: Validate GPR configuration (transitive dependency prevention)
        print("=" * 70)
        print("Step 1: Validate GPR Configuration (Transitive Dependency Prevention)")
        print("=" * 70)
        self.gpr_config_valid = self._validate_application_gpr_config()
        print()

        # Step 2: Validate Ada source file dependencies
        print("=" * 70)
        print("Step 2: Validate Ada Source File Dependencies")
        print("=" * 70)

        # Find all .ads and .adb files
        ada_files = []
        for layer in self.layers_present:
            layer_path = self.src_root / layer
            ada_files.extend(layer_path.rglob('*.ads'))
            ada_files.extend(layer_path.rglob('*.adb'))

        print(f"Scanning {len(ada_files)} Ada files...\n")

        for ada_file in ada_files:
            self.validate_file(ada_file)

        return self.gpr_config_valid and len(self.violations) == 0

    def has_violations(self) -> bool:
        """
        Check if any violations were detected.

        Returns:
            True if violations exist, False otherwise
        """
        return len(self.violations) > 0

    def get_violations(self) -> List[ArchitectureViolation]:
        """
        Get list of detected violations.

        Returns:
            List of ArchitectureViolation objects
        """
        return self.violations.copy()

    def get_violation_count(self) -> int:
        """
        Get count of detected violations.

        Returns:
            Number of violations
        """
        return len(self.violations)

    def clear_violations(self) -> None:
        """Clear all recorded violations (useful for testing)."""
        self.violations.clear()

    def report_violations(self) -> None:
        """Print violation report to stdout"""
        print("\n" + "=" * 70)
        print("FINAL RESULTS")
        print("=" * 70)

        # Report GPR configuration status
        if self.gpr_config_valid:
            print("✅ GPR Configuration: VALID")
        else:
            print("❌ GPR Configuration: INVALID (see errors above)")

        # Report source file dependency violations
        if not self.violations:
            print("✅ Source File Dependencies: VALID")
        else:
            print(f"❌ Source File Dependencies: {len(self.violations)} violation(s)")

        # Overall status
        if self.gpr_config_valid and not self.violations:
            print("\n✅ Architecture validation PASSED - All rules satisfied!")
            return

        print(f"\n❌ Architecture validation FAILED")

        if self.violations:
            print(f"\nSource Dependency Violations ({len(self.violations)}):\n")

        # Group violations by type
        by_type: Dict[str, List[ArchitectureViolation]] = {}
        for v in self.violations:
            by_type.setdefault(v.violation_type, []).append(v)

        for violation_type, violations in sorted(by_type.items()):
            print(f"  [{violation_type}] ({len(violations)} violations)")
            for v in violations:
                print(f"    {v.file_path}:{v.line_number}")
                print(f"      → {v.details}")
            print()


def main():
    """Main entry point"""
    # Determine project root (script is in <root>/scripts/makefile/)
    script_dir = Path(__file__).parent
    project_root = script_dir.parent.parent
    src_root = project_root / 'src'

    print("=" * 70)
    print("Hexagonal Architecture Guard")
    print("=" * 70)
    print(f"Project root: {project_root}")
    print(f"Source root: {src_root}")
    print()

    if not src_root.exists():
        print(f"ERROR: Source directory not found: {src_root}")
        return 2

    # Run validation
    guard = ArchitectureGuard(src_root)

    if not guard.layers_present:
        print("No architecture layers to validate - exiting")
        return 2

    is_valid = guard.validate_all()
    guard.report_violations()

    return 0 if is_valid else 1


if __name__ == '__main__':
    sys.exit(main())
