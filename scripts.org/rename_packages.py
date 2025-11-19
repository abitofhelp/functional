#!/usr/bin/env python3
"""
Rename zoneinfo-core-* files to zoneinfo-* and update package names in contents.
"""
import os
import re
from pathlib import Path

def rename_files_and_content(root_dir):
    """Rename files and update their contents."""
    root = Path(root_dir)

    # Find all Ada files with '-core-' in the name
    files_to_rename = []
    for ada_file in root.rglob('*-core-*.ad[sb]'):
        new_name = ada_file.name.replace('-core-', '-')
        new_path = ada_file.parent / new_name
        files_to_rename.append((ada_file, new_path))

    print(f"Found {len(files_to_rename)} files to rename")

    # Rename files
    for old_path, new_path in files_to_rename:
        print(f"Renaming: {old_path.name} -> {new_path.name}")
        old_path.rename(new_path)

    # Update file contents
    print("\nUpdating file contents...")
    count = 0
    for ada_file in root.rglob('*.ad[sb]'):
        try:
            content = ada_file.read_text(encoding='utf-8')
            original = content

            # Replace package names
            content = re.sub(r'Zoneinfo\.Core\.Domain', 'Zoneinfo.Domain', content)
            content = re.sub(r'zoneinfo-core-domain', 'zoneinfo-domain', content)
            content = re.sub(r'Zoneinfo\.Core\.Application', 'Zoneinfo.Application', content)
            content = re.sub(r'zoneinfo-core-application', 'zoneinfo-application', content)

            if content != original:
                ada_file.write_text(content, encoding='utf-8')
                count += 1

        except Exception as e:
            print(f"Error processing {ada_file}: {e}")

    print(f"Updated {count} files")

if __name__ == '__main__':
    rename_files_and_content('/Users/mike/Ada/github.com/abitofhelp/zoneinfo')
    print("\nDone!")
