#!/usr/bin/env python3
"""
Fix test file references to use TZif prefix

Updates package references in test code bodies that weren't caught
by the initial refactoring script.
"""

import re
from pathlib import Path

# Mapping of old package prefixes to new prefixes
PACKAGE_MAPPINGS = {
    'Domain': 'TZif.Domain',
    'Application': 'TZif.Application',
    'Infrastructure': 'TZif.Infrastructure',
}

def fix_test_file(filepath: Path) -> bool:
    """Fix package references in a test file"""
    try:
        with open(filepath, 'r', encoding='utf-8') as f:
            content = f.read()

        original_content = content

        # Fix instantiations and other references in code body
        # Pattern: Application.Something or Infrastructure.Something or Domain.Something
        # but NOT when preceded by TZif. already
        for old_prefix, new_prefix in PACKAGE_MAPPINGS.items():
            # Match standalone package references not already prefixed with TZif.
            # Negative lookbehind to avoid TZif.Application -> TZif.TZif.Application
            pattern = rf'(?<!TZif\.)({old_prefix}\.[A-Za-z_][A-Za-z0-9_.]*)'

            def replacement(match):
                pkg_name = match.group(1)
                return f'{new_prefix}.{pkg_name[len(old_prefix)+1:]}'

            content = re.sub(pattern, replacement, content)

        # Only write if content changed
        if content != original_content:
            with open(filepath, 'w', encoding='utf-8') as f:
                f.write(content)
            return True
        return False
    except Exception as e:
        print(f"Error processing {filepath}: {e}")
        return False

def main():
    project_root = Path(__file__).parent.parent
    test_dir = project_root / 'test'

    print("=" * 70)
    print("Fixing Test File Package References")
    print("=" * 70)
    print(f"Project root: {project_root}")
    print()

    # Find all test .adb files
    test_files = list(test_dir.rglob('*.adb'))

    print(f"Found {len(test_files)} test files to process\n")

    updated_count = 0
    for filepath in test_files:
        if fix_test_file(filepath):
            updated_count += 1
            print(f"  Updated: {filepath.relative_to(project_root)}")

    print(f"\nUpdated {updated_count} test files\n")
    print("=" * 70)
    print("Fix complete!")
    print("=" * 70)

if __name__ == '__main__':
    main()
