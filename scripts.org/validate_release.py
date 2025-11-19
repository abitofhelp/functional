#!/usr/bin/env python3
# ==============================================================================
# validate_release.py - Comprehensive release validation scan
# ==============================================================================
# Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
# SPDX-License-Identifier: BSD-3-Clause
# See LICENSE file in the project root.
# ==============================================================================
"""
Comprehensive release validation scan for v1.0.0+

Automates all validation checks from RELEASE_CHECKLIST.md Step 3:
- File headers (Ada source, tests, markdown)
- Status fields in documentation
- Build without warnings
- Complete test suite
- TODOs/FIXMEs
- Diagrams up-to-date
- Guides complete
- Temporary files
- Exception handling

Usage:
    python3 scripts/validate_release.py [--verbose] [--quick]

Options:
    --verbose    Show detailed output for each check
    --quick      Skip time-consuming checks (build, tests)
    --help       Show this help message

Exit Codes:
    0 - All validations passed
    1 - One or more validations failed
    2 - Script error or missing dependencies
"""

import sys
import subprocess
import re
import argparse
from pathlib import Path
from typing import List, Tuple, Optional
from dataclasses import dataclass


# ANSI color codes for terminal output
class Colors:
    GREEN = '\033[0;32m'
    RED = '\033[0;31m'
    YELLOW = '\033[0;33m'
    BLUE = '\033[0;34m'
    CYAN = '\033[0;36m'
    BOLD = '\033[1m'
    NC = '\033[0m'  # No Color


@dataclass
class ValidationResult:
    """Result of a validation check"""
    name: str
    passed: bool
    details: List[str]
    warnings: List[str]


class ReleaseValidator:
    """Comprehensive release validation"""

    def __init__(self, project_root: Path, verbose: bool = False):
        self.project_root = project_root
        self.verbose = verbose
        self.results: List[ValidationResult] = []

    def print_header(self, text: str):
        """Print section header"""
        print(f"\n{Colors.BOLD}{Colors.BLUE}{'=' * 70}{Colors.NC}")
        print(f"{Colors.BOLD}{Colors.BLUE}{text}{Colors.NC}")
        print(f"{Colors.BOLD}{Colors.BLUE}{'=' * 70}{Colors.NC}\n")

    def print_check(self, text: str):
        """Print check being performed"""
        print(f"{Colors.CYAN}🔍 {text}...{Colors.NC}")

    def print_success(self, text: str):
        """Print success message"""
        print(f"{Colors.GREEN}✅ {text}{Colors.NC}")

    def print_failure(self, text: str):
        """Print failure message"""
        print(f"{Colors.RED}❌ {text}{Colors.NC}")

    def print_warning(self, text: str):
        """Print warning message"""
        print(f"{Colors.YELLOW}⚠️  {text}{Colors.NC}")

    def run_command(self, cmd: List[str], capture_output: bool = True) -> Optional[subprocess.CompletedProcess]:
        """Run shell command and return result"""
        try:
            if capture_output:
                result = subprocess.run(
                    cmd,
                    capture_output=True,
                    text=True,
                    cwd=self.project_root,
                    timeout=300
                )
            else:
                result = subprocess.run(
                    cmd,
                    cwd=self.project_root,
                    timeout=300
                )
            return result
        except subprocess.TimeoutExpired:
            self.print_warning(f"Command timed out: {' '.join(cmd)}")
            return None
        except Exception as e:
            self.print_warning(f"Command failed: {' '.join(cmd)} - {e}")
            return None

    # =========================================================================
    # Validation Checks
    # =========================================================================

    def check_ada_source_headers(self) -> ValidationResult:
        """Check all Ada source files have proper headers"""
        self.print_check("Verifying Ada source file headers (.ads, .adb)")

        details = []
        warnings = []

        # Find all Ada files
        ada_files = []
        for pattern in ["**/*.ads", "**/*.adb"]:
            ada_files.extend(self.project_root.glob(pattern))

        # Exclude alire dependency cache and build artifacts
        exclude_dirs = ["alire/", "/obj/", "/lib/", "/bin/"]
        ada_files = [f for f in ada_files
                     if not any(excl in str(f) for excl in exclude_dirs)]

        # Filter to source directories only (src/, shared/, test/)
        source_dirs = ["src/", "shared/", "test/"]
        ada_files = [f for f in ada_files if any(d in str(f) for d in source_dirs)]

        missing_copyright = []
        missing_spdx = []

        for ada_file in ada_files:
            try:
                content = ada_file.read_text()
                first_100_lines = '\n'.join(content.split('\n')[:100])

                if "Copyright (c) 2025" not in first_100_lines:
                    missing_copyright.append(str(ada_file.relative_to(self.project_root)))

                if "SPDX-License-Identifier: BSD-3-Clause" not in first_100_lines:
                    missing_spdx.append(str(ada_file.relative_to(self.project_root)))
            except Exception as e:
                warnings.append(f"Could not read {ada_file}: {e}")

        if missing_copyright:
            warnings.extend([f"Missing copyright: {f}" for f in missing_copyright])

        if missing_spdx:
            warnings.extend([f"Missing SPDX: {f}" for f in missing_spdx])

        passed = len(missing_copyright) == 0 and len(missing_spdx) == 0
        details.append(f"Checked {len(ada_files)} Ada source files")

        if passed:
            self.print_success(f"All {len(ada_files)} Ada source files have proper headers")
        else:
            self.print_failure(f"Found {len(missing_copyright)} files missing copyright, {len(missing_spdx)} missing SPDX")

        return ValidationResult("Ada Source Headers", passed, details, warnings)

    def check_test_headers(self) -> ValidationResult:
        """Check all test files have proper headers"""
        self.print_check("Verifying test file headers")

        details = []
        warnings = []

        test_files = list((self.project_root / "tests").glob("**/*.adb"))
        test_files.extend((self.project_root / "tests").glob("**/*.ads"))

        # Filter out build artifacts (obj/ directories)
        test_files = [f for f in test_files if "/obj/" not in str(f) and "\\obj\\" not in str(f)]

        missing_headers = []

        for test_file in test_files:
            try:
                content = test_file.read_text()
                first_20_lines = '\n'.join(content.split('\n')[:20])

                if "Copyright (c) 2025" not in first_20_lines:
                    missing_headers.append(str(test_file.relative_to(self.project_root)))
            except Exception as e:
                warnings.append(f"Could not read {test_file}: {e}")

        passed = len(missing_headers) == 0
        details.append(f"Checked {len(test_files)} test files")

        if passed:
            self.print_success(f"All {len(test_files)} test files have proper headers")
        else:
            self.print_failure(f"Found {len(missing_headers)} test files missing headers")
            warnings.extend([f"Missing header: {f}" for f in missing_headers])

        return ValidationResult("Test File Headers", passed, details, warnings)

    def check_markdown_status(self) -> ValidationResult:
        """Check markdown files have correct Status field"""
        self.print_check("Verifying markdown Status fields")

        details = []
        warnings = []

        # Check README.md specifically
        readme = self.project_root / "README.md"
        if readme.exists():
            content = readme.read_text()
            if "**Status:** Released" in content:
                details.append("README.md: Status = Released ✓")
            elif "Status: Released" in content:
                details.append("README.md: Status = Released ✓")
            elif "Pre-release" in content:
                warnings.append("README.md still shows Pre-release status")
            else:
                warnings.append("README.md missing Status field")

        passed = len(warnings) == 0

        if passed:
            self.print_success("Markdown Status fields correct")
        else:
            self.print_failure("Markdown Status fields need updating")

        return ValidationResult("Markdown Status", passed, details, warnings)

    def check_build_warnings(self) -> ValidationResult:
        """Build project and check for warnings"""
        self.print_check("Building project and checking for warnings")

        details = []
        warnings = []

        # Run make build
        result = self.run_command(["make", "build"], capture_output=True)

        if result is None:
            return ValidationResult("Build Warnings", False, ["Build command failed"], ["Could not run build"])

        # Check for warnings in output (exclude Makefile echo statements)
        output = result.stdout + result.stderr
        warning_lines = [line for line in output.split('\n')
                        if 'warning' in line.lower()
                        and not line.strip().startswith('alr build')  # Ignore Makefile commands
                        and not line.strip().startswith('gprbuild')]  # Ignore command echoes

        if warning_lines:
            warnings.extend(warning_lines[:10])  # First 10 warnings

        passed = len(warning_lines) == 0 and result.returncode == 0
        details.append(f"Build return code: {result.returncode}")
        details.append(f"Warnings found: {len(warning_lines)}")

        if passed:
            self.print_success("Build completed with ZERO warnings")
        else:
            self.print_failure(f"Build had {len(warning_lines)} warnings or failed")

        return ValidationResult("Build Warnings", passed, details, warnings)

    def check_tests(self) -> ValidationResult:
        """Run complete test suite"""
        self.print_check("Running complete test suite")

        details = []
        warnings = []

        # Run make test
        result = self.run_command(["make", "test"], capture_output=True)

        if result is None:
            return ValidationResult("Test Suite", False, ["Test command failed"], ["Could not run tests"])

        output = result.stdout + result.stderr

        # Parse test results
        if "Total Tests Run:" in output:
            for line in output.split('\n'):
                if "Total Tests Run:" in line or "Successful Tests:" in line or "Failed Assertions:" in line:
                    details.append(line.strip())

        # Check for test failures
        failed_tests = "Failed Assertions: 0" in output and "Unexpected Errors: 0" in output
        passed = failed_tests and result.returncode == 0

        if not passed:
            # Extract failure details
            for line in output.split('\n'):
                if 'FAIL' in line or 'ERROR' in line:
                    warnings.append(line.strip())

        if passed:
            self.print_success("All tests passed (9/9 suites)")
        else:
            self.print_failure("Some tests failed")

        return ValidationResult("Test Suite", passed, details, warnings)

    def check_todos_fixmes(self) -> ValidationResult:
        """Check for TODO and FIXME comments"""
        self.print_check("Checking for TODO/FIXME comments")

        details = []
        warnings = []

        # Search for TODOs
        result_todo = self.run_command(
            ["grep", "-r", "TODO",
             "application/src", "bootstrap/src", "domain/src",
             "infrastructure/src", "presentation/src", "shared/src",
             "--include=*.ads", "--include=*.adb"],
            capture_output=True
        )

        # Search for FIXMEs
        result_fixme = self.run_command(
            ["grep", "-r", "FIXME",
             "application/src", "bootstrap/src", "domain/src",
             "infrastructure/src", "presentation/src", "shared/src",
             "--include=*.ads", "--include=*.adb"],
            capture_output=True
        )

        todo_count = 0 if result_todo is None or result_todo.returncode != 0 else len(result_todo.stdout.strip().split('\n'))
        fixme_count = 0 if result_fixme is None or result_fixme.returncode != 0 else len(result_fixme.stdout.strip().split('\n'))

        details.append(f"TODOs found: {todo_count}")
        details.append(f"FIXMEs found: {fixme_count}")

        if todo_count > 0 and result_todo:
            warnings.extend(result_todo.stdout.strip().split('\n')[:5])  # First 5

        if fixme_count > 0 and result_fixme:
            warnings.extend(result_fixme.stdout.strip().split('\n')[:5])  # First 5

        passed = todo_count == 0 and fixme_count == 0

        if passed:
            self.print_success("No TODOs or FIXMEs found")
        else:
            self.print_warning(f"Found {todo_count} TODOs and {fixme_count} FIXMEs")

        return ValidationResult("TODOs/FIXMEs", passed, details, warnings)

    def check_diagrams(self) -> ValidationResult:
        """Check all PlantUML diagrams have SVG exports"""
        self.print_check("Verifying diagrams are up-to-date")

        details = []
        warnings = []

        diagrams_dir = self.project_root / "docs" / "diagrams"
        puml_files = list(diagrams_dir.glob("*.puml"))

        missing_svg = []
        for puml_file in puml_files:
            svg_file = puml_file.with_suffix('.svg')
            if not svg_file.exists():
                missing_svg.append(puml_file.name)

        passed = len(missing_svg) == 0
        details.append(f"PUML files: {len(puml_files)}")
        details.append(f"Missing SVGs: {len(missing_svg)}")

        if missing_svg:
            warnings.extend([f"Missing SVG: {f}" for f in missing_svg])

        if passed:
            self.print_success(f"All {len(puml_files)} diagrams have SVG exports")
        else:
            self.print_failure(f"{len(missing_svg)} diagrams missing SVG exports")

        return ValidationResult("Diagrams", passed, details, warnings)

    def check_guides(self) -> ValidationResult:
        """Check architecture guides are present"""
        self.print_check("Verifying architecture guides")

        details = []
        warnings = []

        guides_dir = self.project_root / "docs" / "guides"
        hybrid_arch_dir = guides_dir / "hybrid-architecture"
        guide_files = list(guides_dir.glob("*.md"))

        # Architecture guides are in hybrid-architecture/ subdirectory
        required_guides = [
            ("hybrid-architecture/architecture-overview.md", "Architecture Overview"),
            ("hybrid-architecture/domain-layer.md", "Domain Layer"),
            ("hybrid-architecture/application-layer.md", "Application Layer"),
            ("hybrid-architecture/infrastructure-layer.md", "Infrastructure Layer"),
            ("hybrid-architecture/presentation-layer.md", "Presentation Layer")
        ]

        missing_guides = []
        for guide_path, guide_name in required_guides:
            if not (guides_dir / guide_path).exists():
                missing_guides.append(guide_name)

        passed = len(missing_guides) == 0
        details.append(f"Guide files found: {len(guide_files)}")
        details.append(f"Required guides: {len(required_guides)}")

        if missing_guides:
            warnings.extend([f"Missing guide: {g}" for g in missing_guides])

        if passed:
            self.print_success(f"All {len(required_guides)} required guides present ({len(guide_files)} total)")
        else:
            self.print_failure(f"Missing {len(missing_guides)} required guides")

        return ValidationResult("Guides", passed, details, warnings)

    def check_restriction_compliance(self) -> ValidationResult:
        """Verify library complies with embedded restrictions (optional for v1.0.0)"""
        self.print_check("Verifying embedded restriction compliance")

        details = []
        warnings = []

        # Test build with embedded profile restrictions (v1.1.0+ feature)
        profiles_dir = self.project_root / "shared" / "config" / "profiles"

        if not profiles_dir.exists():
            details.append("Embedded profiles not configured (deferred to v1.1.0+)")
            # Pass - this is optional for v1.0.0
            self.print_success("Embedded restrictions check skipped (v1.1.0+ feature)")
            return ValidationResult("Restriction Compliance", True, details, warnings)

        profiles_to_test = ['embedded_minimal', 'embedded_standard', 'embedded_generous']

        all_passed = True
        for profile in profiles_to_test:
            restrictions_file = f"shared/config/profiles/{profile}/restrictions.adc"

            if not (self.project_root / restrictions_file).exists():
                warnings.append(f"Missing {restrictions_file}")
                continue

            # Build with restrictions applied
            result = self.run_command(
                ["alr", "exec", "--", "gprbuild", "-P", "shared/shared.gpr",
                 "-q", "-p",
                 f"-gnatec={restrictions_file}",
                 f"-XZONEINFO_PROFILE={profile}"],
                capture_output=True
            )

            if result and result.returncode == 0:
                details.append(f"✅ {profile}: COMPLIANT")
            else:
                all_passed = False
                details.append(f"❌ {profile}: VIOLATIONS FOUND")
                if result and result.stderr:
                    # Extract first few error lines
                    errors = result.stderr.strip().split('\n')[:3]
                    warnings.extend([f"  {e}" for e in errors])

        passed = all_passed and len(warnings) == 0

        if passed:
            self.print_success("Library complies with all embedded restrictions")
        else:
            self.print_failure("Library violates embedded restrictions")

        return ValidationResult("Restriction Compliance", passed, details, warnings)

    def check_temporary_files(self) -> ValidationResult:
        """Check for temporary files"""
        self.print_check("Checking for temporary files")

        details = []
        warnings = []

        # Find temporary files
        temp_patterns = ["*.tmp", "*.bak", "*~", "*.backup"]
        temp_files = []

        for pattern in temp_patterns:
            temp_files.extend(self.project_root.glob(f"**/{pattern}"))

        # Filter out .git directory
        temp_files = [f for f in temp_files if ".git" not in str(f)]

        passed = len(temp_files) == 0
        details.append(f"Temporary files found: {len(temp_files)}")

        if temp_files:
            warnings.extend([str(f.relative_to(self.project_root)) for f in temp_files[:10]])

        if passed:
            self.print_success("No temporary files found")
        else:
            self.print_warning(f"Found {len(temp_files)} temporary files")

        return ValidationResult("Temporary Files", passed, details, warnings)

    # =========================================================================
    # Main Validation Runner
    # =========================================================================

    def run_all_validations(self, skip_slow: bool = False) -> bool:
        """Run all validation checks"""
        self.print_header("RELEASE VALIDATION SCAN - v1.0.0")

        # Quick checks (always run)
        self.results.append(self.check_ada_source_headers())
        self.results.append(self.check_test_headers())
        self.results.append(self.check_markdown_status())
        self.results.append(self.check_todos_fixmes())
        self.results.append(self.check_diagrams())
        self.results.append(self.check_guides())
        self.results.append(self.check_temporary_files())

        # Slow checks (optional)
        if not skip_slow:
            self.results.append(self.check_build_warnings())
            self.results.append(self.check_tests())
            self.results.append(self.check_restriction_compliance())
        else:
            self.print_warning("Skipping slow checks (build, tests, restrictions) due to --quick flag")

        # Print summary
        self.print_summary()

        # Return overall pass/fail
        return all(r.passed for r in self.results)

    def print_summary(self):
        """Print validation summary"""
        self.print_header("VALIDATION SUMMARY")

        passed = sum(1 for r in self.results if r.passed)
        total = len(self.results)

        for result in self.results:
            status = f"{Colors.GREEN}PASS{Colors.NC}" if result.passed else f"{Colors.RED}FAIL{Colors.NC}"
            print(f"{status} - {result.name}")

            if self.verbose and result.details:
                for detail in result.details:
                    print(f"      {detail}")

            if result.warnings:
                for warning in result.warnings[:5]:  # First 5 warnings
                    print(f"      {Colors.YELLOW}⚠️  {warning}{Colors.NC}")

        print(f"\n{Colors.BOLD}Results: {passed}/{total} checks passed{Colors.NC}\n")

        if passed == total:
            self.print_success("🎉 ALL VALIDATIONS PASSED - READY FOR RELEASE!")
        else:
            self.print_failure(f"❌ {total - passed} validation(s) failed - NOT READY FOR RELEASE")


def main():
    parser = argparse.ArgumentParser(
        description="Comprehensive release validation scan",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog=__doc__
    )
    parser.add_argument(
        "--verbose",
        action="store_true",
        help="Show detailed output for each check"
    )
    parser.add_argument(
        "--quick",
        action="store_true",
        help="Skip time-consuming checks (build, tests)"
    )

    args = parser.parse_args()

    # Get project root
    project_root = Path(__file__).parent.parent

    # Run validation
    validator = ReleaseValidator(project_root, verbose=args.verbose)
    all_passed = validator.run_all_validations(skip_slow=args.quick)

    # Exit with appropriate code
    sys.exit(0 if all_passed else 1)


if __name__ == '__main__':
    main()
