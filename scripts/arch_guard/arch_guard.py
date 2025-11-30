#!/usr/bin/env python3
# SPDX-License-Identifier: BSD-3-Clause
# Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
# ==============================================================================
# arch_guard.py - Unified Architecture Guard for Multi-Language Projects
# ==============================================================================
#
# Purpose:
#   Validates layer dependencies in a hybrid DDD/Clean/Hexagonal architecture
#   for Go, Ada, and Rust projects. Enforces strict architectural boundaries
#   between layers to maintain clean separation of concerns and dependency
#   inversion.
#
# Usage:
#   python3 -m scripts.arch_guard [--language go|ada|rust]
#   make check-arch  # via Makefile
#
#   Exit Codes:
#     0: All architecture rules satisfied
#     1: Architecture violations detected
#     2: Script error
#
# Architecture Model (Concentric Spheres - Center-Seeking Dependencies):
#   - Domain: ZERO dependencies (innermost core)
#   - Application: Depends ONLY on Domain (middle sphere)
#   - Infrastructure: Depends on Application + Domain (outer half-sphere)
#   - API: Depends on Application + Domain (library public facade - optional)
#   - Presentation: Depends ONLY on Application (NOT Domain directly!)
#   - Bootstrap: Depends on all layers (outermost)
#
#   Critical Rules:
#   - Presentation MUST NOT import Domain directly (use Application re-exports)
#   - API CAN import Domain directly (it's the library's public facade)
#   - No lateral dependencies (Presentation ↔ Infrastructure)
#
# See Also:
#   Architecture Standards agent for full documentation
# ==============================================================================
"""
Unified Hexagonal/Clean Architecture Guard for Multi-Language Projects.

Validates layer dependencies in hybrid DDD/Clean/Hexagonal architecture
across Go, Ada, and Rust projects.
"""

import argparse
import sys
from pathlib import Path
from typing import Set, Dict, List

from .models import ArchitectureViolation
from .adapters.base import LanguageAdapter


class ArchitectureGuard:
    """
    Language-agnostic architecture validation engine.

    Uses language-specific adapters for parsing while enforcing
    common architectural rules across all supported languages.
    """

    # Layer dependency rules (layer -> allowed dependencies)
    # These rules are language-agnostic and defined by Architecture Standards
    LAYER_RULES = {
        'api': {'application', 'domain'},  # Library public facade - CAN access Domain
        'application': {'domain'},
        'bootstrap': {'domain', 'application', 'infrastructure', 'api', 'presentation'},
        'domain': set(),  # ZERO dependencies (innermost core)
        'infrastructure': {'application', 'domain'},
        'presentation': {'application'},  # CANNOT access Domain directly!
    }

    # No layer can depend on bootstrap (it's the outermost layer)
    FORBIDDEN_DEPENDENCY = 'bootstrap'

    def __init__(self, project_root: Path, adapter: LanguageAdapter):
        """
        Initialize architecture guard.

        Args:
            project_root: Root directory of the project
            adapter: Language-specific adapter for parsing
        """
        self.project_root = project_root
        self.adapter = adapter
        self.violations: List[ArchitectureViolation] = []
        self.config_valid = False

        # Determine source root (some languages use src/ subdirectory)
        if adapter.source_root_subdir:
            self.source_root = project_root / adapter.source_root_subdir
            if not self.source_root.exists():
                self.source_root = project_root
        else:
            self.source_root = project_root

        self.layers_present = self._detect_layers()

    def _detect_layers(self) -> Set[str]:
        """Detect which layers exist in the project."""
        layers = set()
        if not self.source_root.exists():
            print(f"Warning: Source root {self.source_root} does not exist")
            return layers

        print("Layer Detection:")
        for layer in sorted(self.LAYER_RULES.keys()):
            layer_dir = self.source_root / layer
            if layer_dir.exists() and layer_dir.is_dir():
                layers.add(layer)
                print(f"  ✓ {layer:15} - present")
            else:
                print(f"  ○ {layer:15} - not present (skipped)")

        return layers

    def _get_file_layer(self, file_path: Path) -> str | None:
        """Determine which layer a file belongs to based on its path."""
        try:
            relative_path = file_path.relative_to(self.source_root)
            parts = relative_path.parts
            if parts and parts[0] in self.LAYER_RULES:
                return parts[0]
        except ValueError:
            pass
        return None

    def _is_api_composition_root(self, file_path: Path) -> bool:
        """
        Check if file is in an API composition root directory.

        For LIBRARIES (not applications), api/desktop/ and api/adapter/desktop/
        serve as composition roots and are allowed to import Infrastructure.
        This follows the 4-layer library architecture where API sub-packages
        fill the Bootstrap role.
        """
        try:
            relative_path = file_path.relative_to(self.source_root)
            parts = relative_path.parts
            # Check for api/desktop/ or api/adapter/desktop/
            if len(parts) >= 2 and parts[0] == 'api':
                if parts[1] == 'desktop':
                    return True
                if len(parts) >= 3 and parts[1] == 'adapter' and parts[2] == 'desktop':
                    return True
        except ValueError:
            pass
        return False

    def _validate_no_test_imports(self, file_path: Path) -> None:
        """Ensure production code doesn't import test frameworks."""
        # Skip test files
        if self.adapter.is_test_file(file_path):
            return

        # Skip test support files (e.g., domain/test/test_framework.go)
        if self.adapter.is_test_support_file(file_path, self.source_root, self.LAYER_RULES):
            return

        imports = self.adapter.extract_imports(file_path)

        for line_num, import_path in imports:
            import_lower = import_path.lower()
            for forbidden in self.adapter.forbidden_test_imports:
                if forbidden in import_lower:
                    self.violations.append(ArchitectureViolation(
                        file_path=str(file_path),
                        line_number=line_num,
                        violation_type='TEST_CODE_IN_PRODUCTION',
                        details=f"Production code cannot import test framework: {import_path}"
                    ))

    def validate_file(self, file_path: Path) -> None:
        """Validate a single source file against architecture rules."""
        # Run test import check
        self._validate_no_test_imports(file_path)

        # Run language-specific validations
        self.violations.extend(self.adapter.language_specific_validations(file_path))

        # Get current file's layer
        current_layer = self._get_file_layer(file_path)
        if not current_layer or current_layer not in self.layers_present:
            return

        allowed_deps = self.LAYER_RULES[current_layer]
        imports = self.adapter.extract_imports(file_path)

        for line_num, import_path in imports:
            dependency_layer = self.adapter.get_layer_from_import(import_path, self.source_root)

            if not dependency_layer or dependency_layer not in self.layers_present:
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
                    details=f"Layer '{current_layer}' cannot depend on '{self.FORBIDDEN_DEPENDENCY}' (import: {import_path})"
                ))
                continue

            # Check for forbidden lateral dependencies (Presentation ↔ Infrastructure)
            if current_layer == 'presentation' and dependency_layer == 'infrastructure':
                self.violations.append(ArchitectureViolation(
                    file_path=str(file_path),
                    line_number=line_num,
                    violation_type='FORBIDDEN_LATERAL_DEPENDENCY',
                    details=f"Presentation cannot depend on Infrastructure (import: {import_path})"
                ))
                continue

            if current_layer == 'infrastructure' and dependency_layer == 'presentation':
                self.violations.append(ArchitectureViolation(
                    file_path=str(file_path),
                    line_number=line_num,
                    violation_type='FORBIDDEN_LATERAL_DEPENDENCY',
                    details=f"Infrastructure cannot depend on Presentation (import: {import_path})"
                ))
                continue

            # Lateral: API ↔ Infrastructure forbidden
            # EXCEPTION: api/desktop/ is a composition root for libraries
            if current_layer == 'api' and dependency_layer == 'infrastructure':
                if not self._is_api_composition_root(file_path):
                    self.violations.append(ArchitectureViolation(
                        file_path=str(file_path),
                        line_number=line_num,
                        violation_type='FORBIDDEN_LATERAL_DEPENDENCY',
                        details=f"API cannot depend on Infrastructure (import: {import_path})"
                    ))
                continue

            if current_layer == 'infrastructure' and dependency_layer == 'api':
                self.violations.append(ArchitectureViolation(
                    file_path=str(file_path),
                    line_number=line_num,
                    violation_type='FORBIDDEN_LATERAL_DEPENDENCY',
                    details=f"Infrastructure cannot depend on API (import: {import_path})"
                ))
                continue

            # Lateral: API ↔ Presentation forbidden
            if current_layer == 'api' and dependency_layer == 'presentation':
                self.violations.append(ArchitectureViolation(
                    file_path=str(file_path),
                    line_number=line_num,
                    violation_type='FORBIDDEN_LATERAL_DEPENDENCY',
                    details=f"API cannot depend on Presentation (import: {import_path})"
                ))
                continue

            if current_layer == 'presentation' and dependency_layer == 'api':
                self.violations.append(ArchitectureViolation(
                    file_path=str(file_path),
                    line_number=line_num,
                    violation_type='FORBIDDEN_LATERAL_DEPENDENCY',
                    details=f"Presentation cannot depend on API (import: {import_path})"
                ))
                continue

            # CRITICAL: Presentation cannot import Domain directly
            if current_layer == 'presentation' and dependency_layer == 'domain':
                self.violations.append(ArchitectureViolation(
                    file_path=str(file_path),
                    line_number=line_num,
                    violation_type='PRESENTATION_IMPORTS_DOMAIN',
                    details=f"Presentation MUST NOT import Domain directly (import: {import_path})\n" +
                            f"      → Use application/error re-exports instead"
                ))
                continue

            # Check if inter-layer dependency is allowed
            if dependency_layer not in allowed_deps:
                self.violations.append(ArchitectureViolation(
                    file_path=str(file_path),
                    line_number=line_num,
                    violation_type='ILLEGAL_LAYER_DEPENDENCY',
                    details=f"Layer '{current_layer}' cannot depend on '{dependency_layer}' (import: {import_path})"
                ))

    def validate_all(self) -> bool:
        """
        Validate all source files in the project.

        Returns:
            True if all files pass validation, False if violations found
        """
        if not self.layers_present:
            print("⚠ No architecture layers detected - skipping validation")
            return True

        print(f"\nValidating architecture rules for layers: {', '.join(sorted(self.layers_present))}\n")

        # Step 1: Validate configuration
        print("=" * 70)
        print(f"Step 1: Validate {self.adapter.get_config_step_name()}")
        print("=" * 70)
        self.config_valid, config_messages = self.adapter.validate_config(
            self.project_root, self.layers_present
        )
        for msg in config_messages:
            print(msg)
        print()

        # Step 2: Validate source file dependencies
        print("=" * 70)
        print(f"Step 2: Validate {self.adapter.name} Source File Dependencies")
        print("=" * 70)

        # Find all source files
        source_files = []
        for layer in self.layers_present:
            layer_path = self.source_root / layer
            for ext in self.adapter.file_extensions:
                source_files.extend(layer_path.rglob(f'*{ext}'))

        # Filter out vendor directories
        source_files = [f for f in source_files if 'vendor' not in f.parts]

        print(f"Scanning {len(source_files)} {self.adapter.name} files...\n")

        for source_file in source_files:
            self.validate_file(source_file)

        return self.config_valid and len(self.violations) == 0

    def report_violations(self) -> None:
        """Print violation report to stdout."""
        print("\n" + "=" * 70)
        print("FINAL RESULTS")
        print("=" * 70)

        # Report configuration status
        if self.config_valid:
            print(f"✅ {self.adapter.get_config_step_name()}: VALID")
        else:
            print(f"❌ {self.adapter.get_config_step_name()}: INVALID")

        # Report source file dependency violations
        if not self.violations:
            print("✅ Source File Dependencies: VALID")
        else:
            print(f"❌ Source File Dependencies: {len(self.violations)} violation(s)")

        # Overall status
        if self.config_valid and not self.violations:
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


def detect_language(project_root: Path) -> str | None:
    """
    Auto-detect project language based on configuration files.

    Returns:
        'go', 'ada', 'rust', or None if unknown
    """
    # Check for Go
    if (project_root / 'go.mod').exists() or (project_root / 'go.work').exists():
        return 'go'

    # Check for Ada (GPR files or alire.toml)
    if (project_root / 'alire.toml').exists():
        return 'ada'
    if list(project_root.glob('*.gpr')):
        return 'ada'
    if (project_root / 'src').exists() and list((project_root / 'src').glob('**/*.gpr')):
        return 'ada'

    # Check for Rust
    if (project_root / 'Cargo.toml').exists():
        return 'rust'

    return None


def get_adapter(language: str) -> LanguageAdapter:
    """
    Get the appropriate language adapter.

    Args:
        language: 'go', 'ada', or 'rust'

    Returns:
        Language adapter instance

    Raises:
        ValueError: If language is not supported
    """
    from .adapters import GoAdapter, AdaAdapter

    adapters = {
        'go': GoAdapter,
        'ada': AdaAdapter,
        # 'rust': RustAdapter,  # Future
    }

    if language not in adapters:
        supported = ', '.join(adapters.keys())
        raise ValueError(f"Unsupported language: {language}. Supported: {supported}")

    return adapters[language]()


def main() -> int:
    """Main entry point."""
    parser = argparse.ArgumentParser(
        description='Unified Architecture Guard for Multi-Language Projects'
    )
    parser.add_argument(
        '--language', '-l',
        choices=['go', 'ada', 'rust'],
        help='Project language (auto-detected if not specified)'
    )
    parser.add_argument(
        '--project-root', '-p',
        type=Path,
        help='Project root directory (default: auto-detect from script location)'
    )

    args = parser.parse_args()

    # Determine project root
    if args.project_root:
        project_root = args.project_root.resolve()
    else:
        # Script is in <root>/scripts/arch_guard/
        script_dir = Path(__file__).parent
        project_root = script_dir.parent.parent

    print("=" * 70)
    print("Hexagonal Architecture Guard (Unified)")
    print("=" * 70)
    print(f"Project root: {project_root}")

    if not project_root.exists():
        print(f"ERROR: Project directory not found: {project_root}")
        return 2

    # Determine language
    language = args.language or detect_language(project_root)
    if not language:
        print("ERROR: Could not auto-detect project language.")
        print("       Use --language to specify: go, ada, or rust")
        return 2

    print(f"Language: {language.upper()}")
    print()

    # Get adapter and run validation
    try:
        adapter = get_adapter(language)
    except ValueError as e:
        print(f"ERROR: {e}")
        return 2

    guard = ArchitectureGuard(project_root, adapter)

    if not guard.layers_present:
        print("No architecture layers to validate - exiting")
        return 2

    is_valid = guard.validate_all()
    guard.report_violations()

    return 0 if is_valid else 1


if __name__ == '__main__':
    sys.exit(main())
