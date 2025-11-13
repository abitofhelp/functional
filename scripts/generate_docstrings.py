#!/usr/bin/env python3
# ==============================================================================
# generate_docstrings.py - Generate Ada source code docstrings
# ==============================================================================
# Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
# SPDX-License-Identifier: BSD-3-Clause
# See LICENSE file in the project root.
# ==============================================================================
"""
Generate comprehensive docstring headers for all Ada source and test files.

This script generates internal documentation (docstrings) for source code:
1. Reads each Ada file to understand its actual content
2. Analyzes packages, types, functions, procedures
3. Generates detailed, accurate docstring headers
4. Updates copyright year
5. Follows Ada documentation best practices

This is internal documentation embedded in source files (.ads/.adb), not
external user documentation (.md files).

Usage:
    python3 scripts/generate_docstrings.py [--dry-run] [--verbose]

Options:
    --dry-run    Show what would be changed without modifying files
    --verbose    Show detailed information about each file
"""

import sys
import re
import argparse
from pathlib import Path
from typing import List, Dict, Tuple, Optional
from datetime import datetime


class AdaDocGenerator:
    def __init__(self, project_root: Path, dry_run: bool = False, verbose: bool = False):
        self.project_root = project_root
        self.dry_run = dry_run
        self.verbose = verbose
        self.year = datetime.now().year

    def find_ada_files(self) -> List[Path]:
        """Find all Ada source and test files (.ads, .adb)."""
        ada_files = []

        # Source files
        src_dir = self.project_root / "src"
        if src_dir.exists():
            ada_files.extend(src_dir.rglob("*.ads"))
            ada_files.extend(src_dir.rglob("*.adb"))

        # Test files
        test_dir = self.project_root / "test"
        if test_dir.exists():
            ada_files.extend(test_dir.rglob("*.ads"))
            ada_files.extend(test_dir.rglob("*.adb"))

        return sorted(ada_files)

    def extract_package_name(self, file_path: Path) -> str:
        """Extract package name from file name."""
        file_stem = file_path.stem
        parts = file_stem.split("-")
        package_parts = [p.replace("_", " ").title().replace(" ", "_") for p in parts]
        return ".".join(package_parts)

    def analyze_ada_file(self, file_path: Path, code_content: str) -> Dict[str, any]:
        """Analyze Ada file to extract key information."""
        info = {
            'types': [],
            'functions': [],
            'procedures': [],
            'constants': [],
            'generics': [],
            'instantiations': [],
            'with_clauses': [],
            'comments': []
        }

        lines = code_content.split('\n')

        for line in lines:
            stripped = line.strip()

            # Extract with clauses
            if stripped.startswith('with '):
                pkg = stripped.replace('with ', '').replace(';', '').strip()
                info['with_clauses'].append(pkg)

            # Extract type declarations
            elif 'type ' in stripped and ('is' in stripped or ';' in stripped):
                match = re.search(r'type\s+(\w+)', stripped)
                if match:
                    info['types'].append(match.group(1))

            # Extract subtype declarations
            elif 'subtype ' in stripped:
                match = re.search(r'subtype\s+(\w+)', stripped)
                if match:
                    info['types'].append(match.group(1))

            # Extract function declarations
            elif stripped.startswith('function '):
                match = re.search(r'function\s+(\w+)', stripped)
                if match:
                    info['functions'].append(match.group(1))

            # Extract procedure declarations
            elif stripped.startswith('procedure '):
                match = re.search(r'procedure\s+(\w+)', stripped)
                if match:
                    info['procedures'].append(match.group(1))

            # Extract constants
            elif ':' in stripped and 'constant' in stripped:
                match = re.search(r'(\w+)\s*:', stripped)
                if match:
                    info['constants'].append(match.group(1))

            # Extract generic instantiations
            elif 'is new' in stripped:
                match = re.search(r'(\w+)\s+is\s+new', stripped)
                if match:
                    info['instantiations'].append(match.group(1))

            # Extract generic package declarations
            elif stripped.startswith('generic'):
                info['generics'].append('generic package')

        return info

    def generate_comprehensive_header(self, file_path: Path, package_name: str,
                                     is_spec: bool, code_content: str) -> str:
        """Generate comprehensive documentation header based on actual code."""

        # Analyze the file
        analysis = self.analyze_ada_file(file_path, code_content)

        # Determine the purpose based on file location and content
        purpose_lines = self.generate_purpose_section(
            file_path, package_name, is_spec, analysis
        )

        header = f"""pragma Ada_2022;
--  ===========================================================================
--  {package_name}
--  ===========================================================================
--  Copyright (c) {self.year} Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
{purpose_lines}--  ===========================================================================

"""
        return header

    def generate_purpose_section(self, file_path: Path, package_name: str,
                                is_spec: bool, analysis: Dict) -> str:
        """Generate detailed Purpose section based on analysis."""

        path_str = str(file_path).lower()
        lines = []

        # Add main purpose
        lines.append("--  Purpose:")

        # Determine context
        if "test" in path_str:
            if file_path.stem.endswith("_runner"):
                lines.append("--    Test runner for executing test suites.")
                lines.append("--")
            elif file_path.stem.startswith("test_"):
                subject = file_path.stem.replace("test_", "").replace("_", " ").title()
                lines.append(f"--    Unit tests for {subject} functionality.")
                lines.append("--")
            elif "_spy" in file_path.stem:
                lines.append("--    Test spy for monitoring and verifying interactions.")
                lines.append("--")
            else:
                lines.append("--    Test support utilities.")
                lines.append("--")

        elif "value_object" in path_str:
            obj_name = package_name.split(".")[-1].replace("_", " ")
            lines.append(f"--    {obj_name} value object - immutable domain data.")
            lines.append("--")
            if is_spec:
                lines.append("--  Responsibilities:")
                if analysis['types']:
                    lines.append(f"--    - Define {obj_name} type and operations")
                if analysis['functions']:
                    lines.append("--    - Provide constructors and accessors")
                if 'Validation' in ' '.join(analysis['types']) or 'Result' in ' '.join(analysis['types']):
                    lines.append("--    - Validate domain constraints")
                lines.append("--")

        elif "entity" in path_str:
            entity_name = package_name.split(".")[-1]
            lines.append(f"--    {entity_name} domain entity with identity.")
            lines.append("--")
            if is_spec and analysis['types']:
                lines.append("--  Key Types:")
                for t in analysis['types'][:3]:  # First 3 types
                    lines.append(f"--    - {t}")
                lines.append("--")

        elif "service" in path_str:
            service_name = package_name.split(".")[-1].replace("_", " ")
            lines.append(f"--    {service_name} - domain logic and operations.")
            lines.append("--")

        elif "port" in path_str and "inbound" in path_str:
            usecase = package_name.split(".")[-1].replace("_", " ").title()
            lines.append(f"--    Inbound port for {usecase} use case.")
            lines.append("--")
            lines.append("--  Architecture:")
            lines.append("--    Application layer port (hexagonal architecture).")
            lines.append("--    Defines interface for external actors to trigger use cases.")
            lines.append("--")

        elif "port" in path_str and "outbound" in path_str:
            adapter = package_name.split(".")[-1].replace("_", " ").title()
            lines.append(f"--    Outbound port for {adapter}.")
            lines.append("--")
            lines.append("--  Architecture:")
            lines.append("--    Application layer port (hexagonal architecture).")
            lines.append("--    Defines interface for infrastructure adapters.")
            lines.append("--")

        elif "usecase" in path_str:
            usecase = package_name.split(".")[-1].replace("_", " ").title()
            lines.append(f"--    {usecase} use case implementation.")
            lines.append("--")
            lines.append("--  Architecture:")
            lines.append("--    Application layer use case (hexagonal architecture).")
            lines.append("--    Orchestrates domain logic via ports.")
            lines.append("--")

        elif "adapter" in path_str:
            adapter = package_name.split(".")[-1].replace("_", " ").title()
            lines.append(f"--    {adapter} infrastructure adapter.")
            lines.append("--")
            lines.append("--  Architecture:")
            lines.append("--    Infrastructure layer adapter (hexagonal architecture).")
            lines.append("--    Implements outbound ports for external systems.")
            lines.append("--")

        elif "repository" in path_str:
            lines.append("--    Repository for timezone data persistence and retrieval.")
            lines.append("--")
            if is_spec:
                lines.append("--  Operations:")
                if analysis['functions']:
                    for func in analysis['functions'][:5]:
                        lines.append(f"--    - {func}")
                lines.append("--")

        elif "cache" in path_str:
            cache_type = package_name.split(".")[-1].replace("_", " ").title()
            lines.append(f"--    {cache_type} for performance optimization.")
            lines.append("--")

        elif "parser" in path_str:
            lines.append("--    TZif binary format parser implementation.")
            lines.append("--")
            lines.append("--  Supported Versions:")
            lines.append("--    - TZif version 1 (legacy)")
            lines.append("--    - TZif version 2 (64-bit)")
            lines.append("--    - TZif version 3 (with extensions)")
            lines.append("--")

        elif "platform" in path_str:
            if "posix" in path_str:
                lines.append("--    POSIX platform-specific operations.")
                lines.append("--")
                lines.append("--  Platforms:")
                lines.append("--    - Linux (all distributions)")
                lines.append("--    - macOS (all versions)")
                lines.append("--    - BSD variants (FreeBSD, OpenBSD, NetBSD)")
                lines.append("--")
            elif "windows" in path_str:
                lines.append("--    Windows platform-specific operations.")
                lines.append("--")
            else:
                lines.append("--    Platform abstraction layer.")
                lines.append("--")

        else:
            # Generic documentation
            component = package_name.split(".")[-1].replace("_", " ")
            if is_spec:
                lines.append(f"--    {component} interface and type definitions.")
            else:
                lines.append(f"--    {component} implementation.")
            lines.append("--")

        # Add key types section for specs
        if is_spec and analysis['types'] and len(analysis['types']) > 0:
            lines.append("--  Key Types:")
            for t in analysis['types'][:5]:  # First 5 types
                lines.append(f"--    {t}")
            if len(analysis['types']) > 5:
                lines.append(f"--    ... and {len(analysis['types']) - 5} more")
            lines.append("--")

        # Add dependencies if significant
        if is_spec and len(analysis['with_clauses']) > 0:
            external_deps = [w for w in analysis['with_clauses']
                           if not w.startswith('Ada.') and
                              not w.startswith('Domain.') and
                              not w.startswith('Application.') and
                              not w.startswith('Infrastructure.')]
            if external_deps:
                lines.append("--  Dependencies:")
                for dep in external_deps[:3]:
                    lines.append(f"--    {dep}")
                lines.append("--")

        return '\n'.join(lines) + '\n'

    def extract_code_content(self, content: str) -> str:
        """Extract actual Ada code (everything after the header)."""
        lines = content.split('\n')

        # Find the last occurrence of the header separator
        last_separator_idx = -1
        for idx, line in enumerate(lines):
            if re.match(r'^--\s*={70,}', line):
                last_separator_idx = idx

        # If we found a separator, start after it
        if last_separator_idx >= 0:
            code_start_idx = last_separator_idx + 1
            # Skip any blank lines after the separator
            while code_start_idx < len(lines) and not lines[code_start_idx].strip():
                code_start_idx += 1
        else:
            # No separator found, look for first actual code line
            code_start_idx = 0
            for idx, line in enumerate(lines):
                stripped = line.lstrip()
                if stripped and not stripped.startswith('--') and not stripped.startswith('pragma'):
                    code_start_idx = idx
                    break

        return '\n'.join(lines[code_start_idx:])

    def generate_file_docs(self, file_path: Path) -> bool:
        """Generate comprehensive docs for a single Ada file. Returns True if modified."""
        try:
            with open(file_path, 'r', encoding='utf-8') as f:
                content = f.read()

            # Determine if specification or body
            is_spec = file_path.suffix == '.ads'

            # Extract package name
            package_name = self.extract_package_name(file_path)

            # Extract code content (skip old header)
            code_content = self.extract_code_content(content)

            # Generate new comprehensive header
            new_header = self.generate_comprehensive_header(
                file_path, package_name, is_spec, code_content
            )

            # Combine new header with code
            new_content = new_header + code_content

            if new_content.strip() == content.strip():
                return False  # No changes needed

            if not self.dry_run:
                with open(file_path, 'w', encoding='utf-8') as f:
                    f.write(new_content)

            return True

        except Exception as e:
            print(f"  ⚠️  Error processing {file_path}: {e}")
            return False

    def generate_all_docs(self):
        """Generate comprehensive docs for all Ada files."""
        print(f"🔍 Finding Ada source and test files...")
        ada_files = self.find_ada_files()

        if not ada_files:
            print("  ⚠️  No Ada files found")
            return

        print(f"  ✓ Found {len(ada_files)} Ada files")

        if self.dry_run:
            print("🔍 DRY RUN MODE - No files will be modified")

        print(f"\n📝 Generating comprehensive documentation...")

        updated_count = 0
        unchanged_count = 0

        for ada_file in ada_files:
            rel_path = ada_file.relative_to(self.project_root)

            was_modified = self.generate_file_docs(ada_file)

            if was_modified:
                status = "would be documented" if self.dry_run else "documented"
                if self.verbose:
                    print(f"  ✏️  {rel_path} ({status})")
                updated_count += 1
            else:
                if self.verbose:
                    print(f"  ✓  {rel_path} (already documented)")
                unchanged_count += 1

        # Summary
        print(f"\n{'─' * 70}")
        print(f"📊 Summary:")
        print(f"   - Files processed: {len(ada_files)}")
        print(f"   - Documentation generated: {updated_count}")
        print(f"   - Already documented: {unchanged_count}")

        if self.dry_run and updated_count > 0:
            print(f"\n💡 Run without --dry-run to apply changes")
        elif updated_count > 0:
            print(f"\n✅ Comprehensive documentation generated for all files")
        else:
            print(f"\n✅ All files already have comprehensive documentation")


def main():
    parser = argparse.ArgumentParser(
        description="Generate comprehensive Ada file documentation"
    )
    parser.add_argument(
        "--dry-run",
        action="store_true",
        help="Show what would be changed without modifying files"
    )
    parser.add_argument(
        "--verbose",
        action="store_true",
        help="Show detailed information about each file"
    )

    args = parser.parse_args()

    # Project root (assumes script is in scripts/)
    project_root = Path(__file__).parent.parent

    generator = AdaDocGenerator(project_root, args.dry_run, args.verbose)
    generator.generate_all_docs()


if __name__ == '__main__':
    main()
