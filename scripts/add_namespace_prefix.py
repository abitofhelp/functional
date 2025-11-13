#!/usr/bin/env python3
"""
Add Namespace Prefix to TZif Packages

Renames all packages from Domain.*, Application.*, Infrastructure.*
to TZif.Domain.*, TZif.Application.*, TZif.Infrastructure.*

This includes:
1. Renaming package declarations in .ads/.adb files
2. Renaming filenames (domain-*.ads -> tzif-domain-*.ads)
3. Updating all 'with' clauses throughout the codebase
"""

import os
import re
import sys
from pathlib import Path
from typing import List, Tuple

# Mapping of old package prefixes to new prefixes
PACKAGE_MAPPINGS = {
    'Domain': 'TZif.Domain',
    'Application': 'TZif.Application',
    'Infrastructure': 'TZif.Infrastructure',
}

# File prefix mappings (lowercase for filenames)
FILE_MAPPINGS = {
    'domain-': 'tzif-domain-',
    'application-': 'tzif-application-',
    'infrastructure-': 'tzif-infrastructure-',
}

def find_ada_files(root_dir: Path) -> List[Path]:
    """Find all .ads and .adb files"""
    ada_files = []
    for ext in ['*.ads', '*.adb']:
        ada_files.extend(root_dir.rglob(ext))
    return ada_files

def update_package_declaration(content: str, filepath: Path) -> str:
    """Update package declarations in file content"""
    # Match: package Domain is
    # Match: package body Domain.Error is
    # Match: package Domain.Error.Result is

    for old_prefix, new_prefix in PACKAGE_MAPPINGS.items():
        # Package declaration: package Domain is
        pattern1 = rf'^(\s*package\s+)({old_prefix})(\s+is)'
        content = re.sub(pattern1, rf'\1{new_prefix}\3', content, flags=re.MULTILINE)

        # Package body: package body Domain is
        pattern2 = rf'^(\s*package\s+body\s+)({old_prefix})(\s+is)'
        content = re.sub(pattern2, rf'\1{new_prefix}\3', content, flags=re.MULTILINE)

        # Child packages: package Domain.Error is
        pattern3 = rf'^(\s*package\s+)({old_prefix}\.[A-Za-z_][A-Za-z0-9_.]*)(\s+is)'
        content = re.sub(pattern3, lambda m: f'{m.group(1)}{new_prefix}.{m.group(2)[len(old_prefix)+1:]}{m.group(3)}',
                        content, flags=re.MULTILINE)

        # Child package bodies: package body Domain.Error is
        pattern4 = rf'^(\s*package\s+body\s+)({old_prefix}\.[A-Za-z_][A-Za-z0-9_.]*)(\s+is)'
        content = re.sub(pattern4, lambda m: f'{m.group(1)}{new_prefix}.{m.group(2)[len(old_prefix)+1:]}{m.group(3)}',
                        content, flags=re.MULTILINE)

        # End statements: end Domain;
        pattern5 = rf'^(\s*end\s+)({old_prefix})(\s*;)'
        content = re.sub(pattern5, rf'\1{new_prefix}\3', content, flags=re.MULTILINE)

        # End child packages: end Domain.Error;
        pattern6 = rf'^(\s*end\s+)({old_prefix}\.[A-Za-z_][A-Za-z0-9_.]*)(\s*;)'
        content = re.sub(pattern6, lambda m: f'{m.group(1)}{new_prefix}.{m.group(2)[len(old_prefix)+1:]}{m.group(3)}',
                        content, flags=re.MULTILINE)

    return content

def update_with_clauses(content: str) -> str:
    """Update 'with' clauses to use new package names"""
    for old_prefix, new_prefix in PACKAGE_MAPPINGS.items():
        # with Domain;
        pattern1 = rf'^(\s*with\s+)({old_prefix})(\s*;)'
        content = re.sub(pattern1, rf'\1{new_prefix}\3', content, flags=re.MULTILINE)

        # with Domain.Error;
        pattern2 = rf'^(\s*with\s+)({old_prefix}\.[A-Za-z_][A-Za-z0-9_.]*)(\s*;)'
        content = re.sub(pattern2, lambda m: f'{m.group(1)}{new_prefix}.{m.group(2)[len(old_prefix)+1:]}{m.group(3)}',
                        content, flags=re.MULTILINE)

        # Multiple packages in one with: with Domain.A, Domain.B;
        pattern3 = rf'(\s)({old_prefix}(?:\.[A-Za-z_][A-Za-z0-9_.]*)?)(\s*[,;])'
        content = re.sub(pattern3, lambda m: f'{m.group(1)}{new_prefix}.{m.group(2)[len(old_prefix)+1:] if "." in m.group(2) else ""}{m.group(3)}' if m.group(2) != old_prefix else f'{m.group(1)}{new_prefix}{m.group(3)}',
                        content)

    return content

def update_use_clauses(content: str) -> str:
    """Update 'use' clauses to use new package names"""
    for old_prefix, new_prefix in PACKAGE_MAPPINGS.items():
        # use Domain;
        pattern1 = rf'^(\s*use\s+)({old_prefix})(\s*;)'
        content = re.sub(pattern1, rf'\1{new_prefix}\3', content, flags=re.MULTILINE)

        # use Domain.Error;
        pattern2 = rf'^(\s*use\s+)({old_prefix}\.[A-Za-z_][A-Za-z0-9_.]*)(\s*;)'
        content = re.sub(pattern2, lambda m: f'{m.group(1)}{new_prefix}.{m.group(2)[len(old_prefix)+1:]}{m.group(3)}',
                        content, flags=re.MULTILINE)

    return content

def process_ada_file(filepath: Path) -> bool:
    """Process a single Ada file, updating package references"""
    try:
        with open(filepath, 'r', encoding='utf-8') as f:
            content = f.read()

        original_content = content

        # Update package declarations
        content = update_package_declaration(content, filepath)

        # Update with clauses
        content = update_with_clauses(content)

        # Update use clauses
        content = update_use_clauses(content)

        # Only write if content changed
        if content != original_content:
            with open(filepath, 'w', encoding='utf-8') as f:
                f.write(content)
            return True
        return False
    except Exception as e:
        print(f"Error processing {filepath}: {e}")
        return False

def rename_file(filepath: Path) -> Path:
    """Rename file according to FILE_MAPPINGS"""
    filename = filepath.name
    new_filename = filename

    for old_prefix, new_prefix in FILE_MAPPINGS.items():
        if filename.startswith(old_prefix):
            new_filename = new_prefix + filename[len(old_prefix):]
            break

    if new_filename != filename:
        new_path = filepath.parent / new_filename
        filepath.rename(new_path)
        return new_path
    return filepath

def main():
    # Get project root (script is in <root>/scripts/)
    script_dir = Path(__file__).parent
    project_root = script_dir.parent

    print("=" * 70)
    print("TZif Namespace Prefix Addition")
    print("=" * 70)
    print(f"Project root: {project_root}")
    print()

    # Find all Ada files
    src_dir = project_root / 'src'
    test_dir = project_root / 'test'
    examples_dir = project_root / 'examples'

    all_files = []
    for directory in [src_dir, test_dir, examples_dir]:
        if directory.exists():
            all_files.extend(find_ada_files(directory))

    print(f"Found {len(all_files)} Ada files to process\n")

    # Step 1: Update package references in all files
    print("Step 1: Updating package references...")
    updated_count = 0
    for filepath in all_files:
        if process_ada_file(filepath):
            updated_count += 1
            print(f"  Updated: {filepath.relative_to(project_root)}")
    print(f"Updated {updated_count} files\n")

    # Step 2: Rename files
    print("Step 2: Renaming files...")
    renamed_count = 0
    for filepath in all_files:
        if not filepath.exists():
            continue  # Skip if already renamed
        new_path = rename_file(filepath)
        if new_path != filepath:
            renamed_count += 1
            print(f"  Renamed: {filepath.name} -> {new_path.name}")
    print(f"Renamed {renamed_count} files\n")

    print("=" * 70)
    print("Refactoring complete!")
    print("=" * 70)
    print()
    print("Next steps:")
    print("1. Review changes: git diff")
    print("2. Build project: alr build")
    print("3. Run tests: make test-all")
    print("4. If successful, commit: git commit -am 'refactor: Add TZif namespace prefix to all packages'")

if __name__ == '__main__':
    sys.exit(main())
