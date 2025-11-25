# SPDX-License-Identifier: BSD-3-Clause
# Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
"""
Ada language adapter for architecture validation.
"""

import re
from pathlib import Path
from typing import List, Tuple, Set

from .base import LanguageAdapter
from ..models import ArchitectureViolation


class AdaAdapter(LanguageAdapter):
    """
    Ada-specific architecture validation adapter.

    Handles:
    - .ads/.adb file extensions
    - with clause parsing
    - GPR project file validation
    - Ada-specific validations (pragma vs aspect, file naming, etc.)
    """

    # Pragmas that should be aspects instead (Ada 2012+)
    PRAGMA_TO_ASPECT = {
        'Pure': 'with Pure',
        'Preelaborate': 'with Preelaborate',
        'Elaborate_Body': 'with Elaborate_Body',
        'Pack': 'with Pack',
        'Inline': 'with Inline',
        'Volatile': 'with Volatile',
        'Atomic': 'with Atomic',
    }

    @property
    def name(self) -> str:
        return "Ada"

    @property
    def file_extensions(self) -> List[str]:
        return ['.ads', '.adb']

    @property
    def forbidden_test_imports(self) -> List[str]:
        return ['test_framework', 'aunit', 'ahven', 'gnattest']

    @property
    def source_root_subdir(self) -> str | None:
        """Ada projects typically have sources under src/."""
        return 'src'

    def extract_imports(self, file_path: Path) -> List[Tuple[int, str]]:
        """Extract all 'with' clauses from an Ada file."""
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

    def get_layer_from_import(self, import_path: str, project_root: Path) -> str | None:
        """Determine which layer an Ada package belongs to based on naming convention."""
        # Normalize to lowercase for comparison
        normalized = import_path.lower()

        # Layer names
        layer_names = {'domain', 'application', 'infrastructure', 'api', 'presentation', 'bootstrap'}

        # Check each layer (longest match first to handle nested packages)
        for layer in sorted(layer_names, key=len, reverse=True):
            if normalized.startswith(layer + '.') or normalized == layer:
                return layer

        return None

    def validate_config(self, project_root: Path, layers_present: Set[str]) -> Tuple[bool, List[str]]:
        """
        Validate GPR configuration for Ada projects.

        Checks that Application layer is configured as stand-alone library
        with Library_Interface that excludes Domain packages.
        """
        messages = []

        if 'application' not in layers_present:
            messages.append("  ✓ No application layer - GPR validation skipped")
            return True, messages

        # Determine source root
        src_root = project_root / 'src' if (project_root / 'src').exists() else project_root
        app_gpr_path = src_root / 'application' / 'application.gpr'

        if not app_gpr_path.exists():
            messages.append(f"  ❌ Application GPR file not found: {app_gpr_path}")
            messages.append("     Application layer must have application.gpr file")
            return False, messages

        has_standalone = False
        has_interface = False
        interface_packages = []

        try:
            with open(app_gpr_path, 'r', encoding='utf-8') as f:
                content = f.read()

                # Check for Library_Standalone (must not be commented out)
                if re.search(r'^\s*for\s+Library_Standalone\s+use\s+"standard"\s*;',
                           content, re.MULTILINE | re.IGNORECASE):
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
            messages.append(f"  ❌ Could not read Application GPR file: {e}")
            return False, messages

        # Validation Results
        valid = True

        if not has_standalone:
            messages.append(f"  ❌ Application layer GPR missing stand-alone library configuration")
            messages.append(f"     File: {app_gpr_path}")
            messages.append(f"     Required: for Library_Standalone use \"standard\";")
            messages.append(f"     WHY: Prevents transitive Domain exposure to Presentation layer")
            valid = False

        if not has_interface:
            messages.append(f"  ❌ Application layer GPR missing Library_Interface declaration")
            messages.append(f"     File: {app_gpr_path}")
            messages.append(f"     Required: for Library_Interface use (...);")
            messages.append(f"     WHY: Explicitly defines public API, preventing Domain package visibility")
            valid = False
        elif interface_packages:
            # Check that no Domain.* packages are in the interface
            domain_packages = [pkg for pkg in interface_packages
                             if pkg.lower().startswith('domain.')]
            if domain_packages:
                messages.append(f"  ❌ Application Library_Interface contains Domain packages!")
                messages.append(f"     File: {app_gpr_path}")
                messages.append(f"     Forbidden packages in Library_Interface:")
                for pkg in domain_packages:
                    messages.append(f"        - {pkg}")
                messages.append(f"     WHY: Domain packages MUST NOT be exposed to Presentation layer")
                valid = False
            else:
                messages.append(f"  ✓ Application GPR configuration valid:")
                messages.append(f"     - Stand-alone library: enabled")
                messages.append(f"     - Library_Interface: defined ({len(interface_packages)} packages)")
                messages.append(f"     - Domain packages: correctly excluded")

        return valid, messages

    def is_test_file(self, file_path: Path) -> bool:
        """Check if this is an Ada test file."""
        # Ada test files are typically:
        # 1. In a top-level tests/ or test/ directory
        # 2. Have _test or -test in the filename
        filename_lower = file_path.name.lower()
        if '_test' in filename_lower or '-test' in filename_lower:
            return True

        # Check if in a tests/ directory (but not pytest temp paths)
        parts = file_path.parts
        for i, part in enumerate(parts):
            if part.lower() in ('tests', 'test') and i > 0:
                # Make sure it's a project directory, not pytest temp
                prev_part = parts[i-1].lower()
                if not prev_part.startswith('pytest') and not prev_part.startswith('tmp'):
                    return True

        return False

    def language_specific_validations(self, file_path: Path) -> List[ArchitectureViolation]:
        """Perform Ada-specific validations."""
        violations = []

        # Only validate production code
        if self.is_test_file(file_path):
            return violations

        violations.extend(self._validate_pragma_usage(file_path))
        violations.extend(self._validate_file_naming(file_path))
        violations.extend(self._validate_bounded_strings_for_errors(file_path))

        return violations

    def _validate_pragma_usage(self, file_path: Path) -> List[ArchitectureViolation]:
        """Check that aspects are used instead of pragmas where applicable (Ada 2012+)."""
        violations = []

        try:
            with open(file_path, 'r', encoding='utf-8') as f:
                content = f.read()

            for line_num, line in enumerate(content.split('\n'), start=1):
                for pragma, aspect in self.PRAGMA_TO_ASPECT.items():
                    # Match pragma statement (not in comments)
                    if re.match(rf'^\s*pragma\s+{pragma}\s*[;\(]', line, re.IGNORECASE):
                        # Skip if it's in a comment
                        if '--' in line and line.index('--') < line.lower().index('pragma'):
                            continue

                        violations.append(ArchitectureViolation(
                            file_path=str(file_path),
                            line_number=line_num,
                            violation_type='PRAGMA_INSTEAD_OF_ASPECT',
                            details=f"Use {aspect} instead of pragma {pragma}"
                        ))
        except Exception as e:
            print(f"Warning: Could not validate pragma usage in {file_path}: {e}")

        return violations

    def _validate_file_naming(self, file_path: Path) -> List[ArchitectureViolation]:
        """Validate file naming conventions for TOP-LEVEL packages only."""
        violations = []

        try:
            with open(file_path, 'r', encoding='utf-8') as f:
                content = f.read()
                lines = content.split('\n')

                nesting_level = 0
                in_generic_declaration = False

                for line_num, line in enumerate(lines, start=1):
                    stripped = line.strip()

                    # Track generic declarations
                    if stripped.startswith('generic'):
                        in_generic_declaration = True
                        continue

                    # Check for package declaration
                    if stripped.startswith('package'):
                        # SKIP: Package instantiations (is new)
                        if ' is new ' in line or '\tis new ' in line:
                            continue

                        # SKIP: Nested packages (indented)
                        if line != line.lstrip() and 'package' in line:
                            continue

                        # Track nesting level
                        if 'is new' not in stripped:
                            nesting_level += 1

                            # ONLY CHECK top-level packages
                            if nesting_level == 1 and not in_generic_declaration:
                                match = re.match(r'package\s+(body\s+)?([A-Za-z0-9_.]+)',
                                               stripped, re.IGNORECASE)
                                if match:
                                    package_name = match.group(2)
                                    expected_filename = package_name.lower().replace('.', '-')
                                    actual_filename = file_path.stem.lower()

                                    if actual_filename != expected_filename:
                                        violations.append(ArchitectureViolation(
                                            file_path=str(file_path),
                                            line_number=line_num,
                                            violation_type='INCONSISTENT_FILE_NAMING',
                                            details=f"File name '{file_path.name}' doesn't match package '{package_name}' (expected: {expected_filename}{file_path.suffix})"
                                        ))

                            if in_generic_declaration:
                                in_generic_declaration = False

                    # Track end of packages
                    if stripped.startswith('end') and ';' in stripped:
                        nesting_level = max(0, nesting_level - 1)

        except Exception as e:
            print(f"Warning: Could not validate file naming for {file_path}: {e}")

        return violations

    def _validate_bounded_strings_for_errors(self, file_path: Path) -> List[ArchitectureViolation]:
        """Ensure error types use bounded strings, not unbounded String."""
        violations = []

        # Only check error-related files
        if 'error' not in str(file_path).lower():
            return violations

        try:
            with open(file_path, 'r', encoding='utf-8') as f:
                content = f.read()

            for line_num, line in enumerate(content.split('\n'), start=1):
                # Skip comments
                code_part = line.split('--')[0] if '--' in line else line

                # Match: Message : String (but not Bounded_String)
                if re.search(r'\bMessage\s*:\s*String\s*;', code_part) and 'Bounded_String' not in code_part:
                    violations.append(ArchitectureViolation(
                        file_path=str(file_path),
                        line_number=line_num,
                        violation_type='UNBOUNDED_STRING_IN_ERROR',
                        details="Error message field should use Bounded_String, not String (for embedded safety)"
                    ))
        except Exception as e:
            print(f"Warning: Could not validate bounded strings in {file_path}: {e}")

        return violations

    def get_config_step_name(self) -> str:
        return "GPR Configuration (Transitive Dependency Prevention)"
