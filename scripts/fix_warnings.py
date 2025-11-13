#!/usr/bin/env python3
"""
Script to analyze and report Ada compiler warnings for systematic fixing.
Organizes warnings by type and file to make batch fixes easier.
"""

import sys
import re
from collections import defaultdict
from pathlib import Path

def parse_warnings(warning_text):
    """Parse warning text and organize by type and file."""
    warnings_by_type = defaultdict(list)

    lines = warning_text.strip().split('\n')
    for line in lines:
        # Parse warning format: filename:line:col: warning: message [-gnatwX]
        match = re.match(r'([^:]+):(\d+):(\d+):\s+\(?(warning|style)\)?:\s+(.+)\s+\[([^\]]+)\]', line)
        if match:
            filename, line_num, col_num, warning_type, message, warning_code = match.groups()
            warnings_by_type[warning_code].append({
                'file': filename,
                'line': int(line_num),
                'col': int(col_num),
                'type': warning_type,
                'message': message,
                'code': warning_code
            })

    return warnings_by_type

def print_warning_summary(warnings_by_type):
    """Print organized summary of warnings."""
    print("=" * 80)
    print("WARNING SUMMARY BY TYPE")
    print("=" * 80)

    for code in sorted(warnings_by_type.keys()):
        warnings = warnings_by_type[code]
        print(f"\n{code}: {len(warnings)} warnings")
        print("-" * 40)

        # Group by file
        by_file = defaultdict(list)
        for w in warnings:
            by_file[w['file']].append(w)

        for filename in sorted(by_file.keys()):
            file_warnings = by_file[filename]
            print(f"  {filename}: {len(file_warnings)} occurrences")
            for w in file_warnings[:3]:  # Show first 3
                print(f"    Line {w['line']}: {w['message']}")
            if len(file_warnings) > 3:
                print(f"    ... and {len(file_warnings) - 3} more")

def generate_unused_with_fixes(warnings_by_type):
    """Generate specific fixes for unused with clause warnings."""
    unused_with_warnings = []

    for code in ['-gnatwu', '-gnatwr']:
        if code in warnings_by_type:
            unused_with_warnings.extend(warnings_by_type[code])

    # Filter for "with clause" related warnings
    with_clause_warnings = [w for w in unused_with_warnings
                           if 'with clause' in w['message'].lower() or
                              'is not referenced' in w['message'].lower() or
                              'use clause' in w['message'].lower()]

    print("\n" + "=" * 80)
    print("UNUSED WITH/USE CLAUSE FIXES")
    print("=" * 80)

    by_file = defaultdict(list)
    for w in with_clause_warnings:
        by_file[w['file']].append(w)

    for filename in sorted(by_file.keys()):
        print(f"\n{filename}:")
        for w in sorted(by_file[filename], key=lambda x: x['line']):
            print(f"  Line {w['line']}: {w['message']}")

if __name__ == '__main__':
    # Sample warning text for testing
    warning_text = """
abohlib-domain-result.adb:10:09: warning: unit "Ada.Exceptions" is not referenced [-gnatwu]
zoneinfo-domain-ports-time-time_provider.ads:4:35: warning: unit "Zoneinfo.Domain.Value_Objects.Duration" is not referenced [-gnatwu]
zoneinfo-domain-ports-time-time_provider.ads:24:04: warning: use clause for package "Timestamp" has no effect [-gnatwu]
zoneinfo-domain-entities-timezone_rule.ads:44:21: warning: no entities of "Zoneinfo.Domain.Errors" are referenced [-gnatwu]
zoneinfo-domain-entities-timezone_rule.ads:56:04: warning: use clause for package "Errors" has no effect [-gnatwu]
zoneinfo-infrastructure-persistence-zone_repository.adb:4:06: warning: redundant with clause in body [-gnatwr]
zoneinfo-infrastructure-persistence-zone_repository.ads:3:17: warning: with clause might be moved to body [-gnatwu]
"""

    warnings_by_type = parse_warnings(warning_text)
    print_warning_summary(warnings_by_type)
    generate_unused_with_fixes(warnings_by_type)
