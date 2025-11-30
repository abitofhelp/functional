#!/usr/bin/env python3
# ==============================================================================
# release.py - Unified Release Management Script
# ==============================================================================
# Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
# SPDX-License-Identifier: BSD-3-Clause
# See LICENSE file in the project root.
#
# Purpose:
#   Unified release management for Go and Ada projects.
#   Auto-detects project language and applies appropriate release workflow.
#
# Usage:
#   python scripts/release/release.py prepare <version>
#   python scripts/release/release.py release <version>
#   python scripts/release/release.py diagrams
#   python scripts/release/release.py validate <version>
#
# Examples:
#   python scripts/release/release.py prepare 1.0.0
#   python scripts/release/release.py release 1.0.0
#   python scripts/release/release.py diagrams
#
# Design Notes:
#   Uses adapter pattern for language-specific operations.
#   Follows same patterns as arch_guard and brand_project.
#   Supports Go (go.mod) and Ada (alire.toml) projects.
#
# ==============================================================================

import argparse
import re
import sys
from datetime import datetime
from pathlib import Path
from typing import Optional

# Support both direct script execution and module import
try:
    from .models import ReleaseConfig, Language, ReleaseAction
    from .adapters import GoReleaseAdapter, AdaReleaseAdapter
except ImportError:
    from models import ReleaseConfig, Language, ReleaseAction
    from adapters import GoReleaseAdapter, AdaReleaseAdapter

# Add parent directory to path for common imports
sys.path.insert(0, str(Path(__file__).parent.parent))
from common import print_success, print_error, print_warning, print_info, print_section


def detect_language(project_root: Path) -> Optional[Language]:
    """
    Detect the project language from source directory.

    Args:
        project_root: Path to project directory

    Returns:
        Detected Language or None
    """
    if GoReleaseAdapter.detect(project_root):
        return Language.GO
    if AdaReleaseAdapter.detect(project_root):
        return Language.ADA
    return None


def get_adapter(language: Language):
    """
    Get the appropriate adapter for a language.

    Args:
        language: Target language

    Returns:
        Language adapter instance
    """
    adapters = {
        Language.GO: GoReleaseAdapter(),
        Language.ADA: AdaReleaseAdapter(),
    }
    return adapters.get(language)


def prompt_user_continue(message: str, allow_skip: bool = False) -> bool:
    """
    Prompt user to perform a manual task and continue.

    Args:
        message: Instructions for the user
        allow_skip: If True, user can skip this step

    Returns:
        True if user wants to continue, False to abort
    """
    print(f"\n{'='*70}")
    print(f"MANUAL STEP REQUIRED")
    print(f"{'='*70}")
    print(f"\n{message}\n")

    while True:
        if allow_skip:
            response = input("Press ENTER to continue, 's' to skip, or 'q' to quit: ").strip().lower()
            if response == '':
                return True
            elif response == 's':
                print("Skipping this step...")
                return True
            elif response == 'q':
                print("Release process aborted by user")
                return False
        else:
            response = input("Press ENTER to continue, or 'q' to quit: ").strip().lower()
            if response == '':
                return True
            elif response == 'q':
                print("Release process aborted by user")
                return False

        print("Invalid input. Please try again.")


def create_initial_changelog(config) -> str:
    """Create a clean Common Changelog format CHANGELOG.md for initial release."""
    today = datetime.now().strftime("%Y-%m-%d")

    return f"""# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Common Changelog](https://common-changelog.org),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

---

## [Unreleased]

### Changed

### Added

### Removed

### Fixed

---

## [{config.version}] - {today}

_Initial release of {config.project_name}._

### Added

- Initial implementation with hexagonal architecture
- Domain layer with core business logic
- Application layer with use cases
- Infrastructure layer with adapters
- Presentation layer with CLI
- Comprehensive test suite

---

## License & Copyright

- **License**: BSD-3-Clause
- **Copyright**: (c) {config.year} Michael Gardner, A Bit of Help, Inc.
- **SPDX-License-Identifier**: BSD-3-Clause
"""


def has_meaningful_content(section_content: str) -> bool:
    """
    Check if a CHANGELOG section has meaningful content.

    Returns True if the section contains actual bullet points with content,
    not just placeholder text like "_Initial release._" or "TBD".

    Args:
        section_content: The content of a version section

    Returns:
        True if section has meaningful content (bullet points with text)
    """
    # Look for bullet points with actual content (- followed by text)
    has_bullets = bool(re.search(r'^-\s+\S', section_content, re.MULTILINE))

    # Check for placeholder text patterns
    placeholder_patterns = [
        r'^\s*_[^_]+_\s*$',  # Single italic line like "_Initial release._"
        r'TBD',
        r'placeholder',
        r'TODO',
    ]
    is_placeholder = any(
        re.search(pattern, section_content, re.IGNORECASE | re.MULTILINE)
        for pattern in placeholder_patterns
    )

    return has_bullets or (not is_placeholder and len(section_content.strip()) > 50)


def update_changelog(config) -> bool:
    """
    Update or create CHANGELOG.md with new version.

    Behavior:
    - For initial releases: Create or overwrite with clean template
    - For later versions: Update [Unreleased] to new version
    - If version section exists but is placeholder, check [Unreleased] for content
    """
    changelog_file = config.project_root / "CHANGELOG.md"

    # Check if version already exists in CHANGELOG
    if changelog_file.exists():
        existing_content = changelog_file.read_text(encoding='utf-8')
        version_match = re.search(
            rf'## \[{re.escape(config.version)}\]\s*-?\s*[^\n]*\n(.*?)(?=\n## |\Z)',
            existing_content,
            re.DOTALL
        )
        if version_match:
            version_content = version_match.group(1).strip()
            if has_meaningful_content(version_content):
                print(f"  Version [{config.version}] already exists with content")
                print(f"  Skipping CHANGELOG update (already prepared)")
                return True
            else:
                print_warning(f"Version [{config.version}] exists but has placeholder content")
                print_info("Checking [Unreleased] section for content to merge...")

    # Handle initial release - only create template if CHANGELOG doesn't exist
    # or is essentially empty/template-only
    if config.is_initial_release:
        if changelog_file.exists():
            existing_content = changelog_file.read_text(encoding='utf-8')
            # Check if existing CHANGELOG has substantial content (not just a template)
            # Look for actual content beyond headers and empty sections
            has_content = bool(re.search(r'###\s+\w+.*\n\s*-\s+\S', existing_content, re.DOTALL))
            if has_content:
                print(f"  CHANGELOG.md has existing content - preserving it")
                print(f"  Skipping template generation for initial release")
                return True

        content = create_initial_changelog(config)

        if config.dry_run:
            print(f"  [DRY-RUN] Would create CHANGELOG.md for initial release {config.version}")
            return True

        if changelog_file.exists():
            backup_file = config.project_root / "CHANGELOG.md.backup"
            changelog_file.rename(backup_file)
            print(f"  Backed up existing CHANGELOG.md to {backup_file.name}")

        changelog_file.write_text(content, encoding='utf-8')
        print(f"  Created CHANGELOG.md for initial release {config.version}")
        return True

    # Handle subsequent releases
    if not changelog_file.exists():
        print_error("CHANGELOG.md not found!")
        print_info("For releases after 1.0.0, CHANGELOG.md must exist.")
        return False

    try:
        content = changelog_file.read_text(encoding='utf-8')

        # Check if this version already exists
        if re.search(rf'## \[{re.escape(config.version)}\]', content):
            print_warning(f"Version [{config.version}] already exists in CHANGELOG.md")
            print_info("Skipping CHANGELOG update (appears to be already prepared)")
            return True

        # Find the [Unreleased] section
        unreleased_pattern = r'## \[Unreleased\]\s*\n(.*?)(?=\n## |\Z)'
        match = re.search(unreleased_pattern, content, re.DOTALL)

        if not match:
            print_error("Could not find [Unreleased] section in CHANGELOG.md")
            return False

        unreleased_content = match.group(1).strip()

        # Create new release section
        today = datetime.now().strftime("%Y-%m-%d")
        release_section = f"""## [Unreleased]

### Changed

### Added

### Removed

### Fixed

---

## [{config.version}] - {today}

{unreleased_content}

"""

        # Replace the unreleased section
        content = re.sub(
            r'## \[Unreleased\]\s*\n.*?(?=\n## |\Z)',
            release_section,
            content,
            flags=re.DOTALL,
            count=1
        )

        if config.dry_run:
            print(f"  [DRY-RUN] Would update CHANGELOG.md with release {config.version}")
            return True

        changelog_file.write_text(content, encoding='utf-8')
        print(f"  Updated CHANGELOG.md with release {config.version}")
        return True

    except Exception as e:
        print_error(f"Error updating changelog: {e}")
        return False


def prepare_release(config, adapter) -> bool:
    """Prepare release by updating versions and running checks."""
    print_section(f"\n{'='*70}")
    print_section(f"PREPARING RELEASE {config.version} ({adapter.name})")
    print_section(f"{'='*70}\n")

    # Step 0a: Validate Makefile targets
    print_info("\nStep 0a: Validating Makefile targets...")
    if not adapter.validate_makefile(config):
        print_error("Makefile validation failed - fix targets before release")
        return False

    # Step 0b: Validate documentation links
    print_info("\nStep 0b: Validating documentation links...")
    if not adapter.validate_links(config):
        print_error("Link validation failed - fix broken links before release")
        return False

    # Step 0c: Validate documentation consistency
    print_info("\nStep 0c: Validating documentation consistency...")
    has_discrepancies, discrepancies = adapter.validate_documentation(config)
    if has_discrepancies:
        message = f"""Documentation validation found {len(discrepancies)} potential discrepancy(ies).

Please review the items listed above.

These may be:
- Incorrect terminology for project type (library vs application)
- References to non-existent files
- Outdated directory structures in documentation

You can:
- Press ENTER to acknowledge and continue (if they are false positives)
- Press 'q' to quit and fix the issues before releasing"""
        if not prompt_user_continue(message):
            return False

    # Step 0d: Validate AI Assistance & Authorship section (LEGALLY CRITICAL)
    print_info("\nStep 0d: Validating AI Assistance & Authorship section...")
    is_valid, ai_errors = adapter.validate_ai_assistance_section(config)
    if not is_valid:
        print_error("AI Assistance & Authorship section validation FAILED")
        print_error("This is a LEGALLY CRITICAL requirement for all releases")
        print_info("")
        print_info("Required section in README.md:")
        print_info("  ### AI Assistance & Authorship")
        print_info("")
        print_info("  This project — including its source code, tests, documentation,")
        print_info("  and other deliverables — is designed, implemented, and maintained")
        print_info("  by human developers, with Michael Gardner as the Principal Software")
        print_info("  Engineer and project lead.")
        print_info("")
        print_info("  [... see documentation agent for full required content ...]")
        print_info("")
        print_info("Placement: After project description, BEFORE installation instructions")
        return False

    # Step 0e: Scan git history for AI markers (CRITICAL - git hygiene)
    print_info("\nStep 0e: Scanning git history for AI assistant markers...")
    is_clean, git_violations = adapter.scan_git_history_for_ai_markers(config)
    if not is_clean:
        print_warning(f"Found {len(git_violations)} AI marker(s) in git history")
        message = f"""Git history contains {len(git_violations)} AI attribution marker(s).

These MUST be removed before release per our git attribution policy.

Options to clean git history:
1. For recent commits: git rebase -i HEAD~N and edit messages
2. For older commits: git filter-branch or BFG Repo-Cleaner
3. Contact the maintainer for assistance

You can:
- Press ENTER to acknowledge and continue (if cleanup is planned)
- Press 'q' to quit and clean history before releasing"""
        if not prompt_user_continue(message):
            return False

    # Step 0f: Scan for TODO/FIXME/STUB markers
    print_info("\nStep 0f: Scanning for TODO/FIXME/STUB markers...")
    is_clean, code_markers = adapter.scan_for_code_markers(config)
    if not is_clean:
        message = f"""Found {len(code_markers)} TODO/FIXME/STUB marker(s) in source code.

These markers indicate incomplete or temporary code that should be
reviewed before release:

- TODO: Planned work not yet completed
- FIXME: Known issues requiring fixes
- STUB: Placeholder implementations
- XXX/HACK: Technical debt or workarounds

You can:
- Press ENTER to acknowledge and continue (if these are acceptable for release)
- Press 'q' to quit and address the markers before releasing"""
        if not prompt_user_continue(message):
            return False

    # Step 0g: Scan for long files
    print_info("\nStep 0g: Scanning for long source files...")
    is_clean, long_files = adapter.scan_for_long_files(config, max_lines=800)
    if not is_clean:
        message = f"""Found {len(long_files)} source file(s) exceeding 800 lines.

Long files may indicate:
- Need for refactoring or decomposition
- Single Responsibility Principle violations
- Accumulated technical debt

You can:
- Press ENTER to acknowledge and continue (advisory only)
- Press 'q' to quit and refactor before releasing"""
        if not prompt_user_continue(message):
            return False

    # Step 1: Clean up temporary files
    print_info("\nStep 1: Cleaning up temporary files...")
    if not adapter.cleanup_temp_files(config):
        print_warning("Could not clean up temporary files (continuing)")

    # Step 2: Update version in config files
    print_info(f"\nStep 2: Updating {adapter.name} version...")
    if not adapter.update_version(config):
        return False

    # Step 3: Sync versions (if applicable)
    print_info("\nStep 3: Syncing layer versions...")
    if not adapter.sync_versions(config):
        print_warning("Could not sync layer versions (continuing)")

    # Step 4: Generate version file (if applicable)
    print_info("\nStep 4: Generating version file...")
    if not adapter.generate_version_file(config):
        print_warning("Could not generate version file (continuing)")

    # Step 5: Update markdown documentation
    print_info("\nStep 5: Updating markdown documentation...")
    adapter.update_all_markdown_files(config)

    # Step 6: CHANGELOG checkpoint
    changelog_file = config.project_root / "CHANGELOG.md"
    if changelog_file.exists():
        content = changelog_file.read_text(encoding='utf-8')
        if not re.search(rf'## \[{re.escape(config.version)}\]', content):
            message = f"""FINAL CHECKPOINT: CHANGELOG.md Review

The script is about to modify CHANGELOG.md:
- It will move [Unreleased] content -> [{config.version}] section
- It will create a fresh [Unreleased] section

LAST CHANCE to edit CHANGELOG.md if needed:
1. Edit CHANGELOG.md (add/modify release notes in [Unreleased])
2. If you made changes, commit them:
   git add CHANGELOG.md
   git commit -m "docs: Update release notes for {config.version}"
3. Press ENTER to let the script process CHANGELOG.md

If CHANGELOG is already correct, just press ENTER to continue."""
            if not prompt_user_continue(message):
                return False

    # Step 7: Update CHANGELOG.md
    print_info("\nStep 6: Updating CHANGELOG.md...")
    if not update_changelog(config):
        return False

    # Step 7: Generate diagrams
    print_info("\nStep 7: Generating diagrams...")
    if not adapter.generate_diagrams(config):
        print_warning("Could not generate diagrams (continuing)")

    # Checkpoint: Review and commit changes
    message = f"""All files have been updated for release {config.version}

IMPORTANT: Review and commit changes NOW (before build/test):

1. Review what changed:
   git diff

2. Commit the prepared release:
   git add -A
   git commit -m "chore: Prepare release {config.version}"

WHY COMMIT NOW?
- If build/tests fail, you can easily rollback (git reset HEAD~1)
- Clean separation between preparation and verification
- Safe fallback position

After committing, press ENTER to continue with build and test verification."""
    if not prompt_user_continue(message):
        return False

    # Step 8: Build verification
    print_info("\nStep 8: Running build...")
    if not adapter.run_build(config):
        print_error("Build failed")
        return False

    # Step 9: Test verification
    print_info("\nStep 9: Running tests...")
    if not adapter.run_tests(config):
        print_error("Tests failed")
        return False

    print_section(f"\n{'='*70}")
    print_success(f"RELEASE {config.version} PREPARED AND VERIFIED SUCCESSFULLY!")
    print_section(f"{'='*70}\n")
    print_info("All files updated")
    print_info("Build passing")
    print_info("Tests passing")
    print()
    print_info("Next step:")
    print_info(f"   python3 scripts/release/release.py release {config.version}")
    print()
    print_info("This will:")
    print_info(f"  - Create git tag v{config.version}")
    print_info("  - Push to GitHub")
    print_info("  - Create GitHub release with release notes")
    print()

    return True


def create_release(config, adapter) -> bool:
    """Create the actual release (tag and publish)."""
    print_section(f"\n{'='*70}")
    print_section(f"CREATING RELEASE {config.version} ({adapter.name})")
    print_section(f"{'='*70}\n")

    # Verify working tree is clean
    print_info("Verifying clean working tree...")
    if not adapter.verify_clean_working_tree(config):
        print_error("Working tree is not clean. Please commit changes first.")
        print_info("   Run: git status")
        return False
    print_success("Working tree is clean")

    # Create git tag
    print_info("\nCreating git tag...")
    if not adapter.create_git_tag(config):
        return False

    # Push changes and tag
    print_info("\nPushing to GitHub...")
    if not adapter.push_changes(config):
        return False

    # Create GitHub release
    print_info("\nCreating GitHub release...")
    if not adapter.create_github_release(config):
        return False

    print_section(f"\n{'='*70}")
    print_success(f"RELEASE {config.version} CREATED SUCCESSFULLY!")
    print_section(f"{'='*70}\n")
    print_info("Release is now live on GitHub!")
    if config.project_url:
        release_url = f"{config.project_url}/releases/tag/v{config.version}"
        print_info(f"View at: {release_url}")
    print()

    return True


def generate_diagrams_only(config, adapter) -> bool:
    """Generate diagrams without full release."""
    print_section(f"\n{'='*70}")
    print_section(f"GENERATING DIAGRAMS ({adapter.name})")
    print_section(f"{'='*70}\n")

    return adapter.generate_diagrams(config)


def main():
    """Main entry point."""
    parser = argparse.ArgumentParser(
        description='Unified release management for Go and Ada projects',
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  %(prog)s prepare 1.0.0     Prepare release (update files, build, test)
  %(prog)s release 1.0.0     Create release (tag, push, GitHub release)
  %(prog)s diagrams          Generate PlantUML diagrams only

The script auto-detects the project language (Go/Ada) and applies
the appropriate release workflow.
        """
    )

    parser.add_argument(
        'action',
        choices=['prepare', 'release', 'diagrams', 'validate'],
        help='Action to perform'
    )

    parser.add_argument(
        'version',
        nargs='?',
        help='Version to release (e.g., 1.0.0) - required for prepare/release'
    )

    parser.add_argument(
        '--project-root', '-p',
        type=Path,
        default=None,
        help='Project root directory (default: auto-detect from script location)'
    )

    parser.add_argument(
        '--dry-run',
        action='store_true',
        help='Show what would be done without making changes'
    )

    args = parser.parse_args()

    # Validate version is provided for prepare/release
    if args.action in ['prepare', 'release', 'validate'] and not args.version:
        print_error(f"Version is required for {args.action} action")
        parser.print_help()
        return 1

    # Validate semantic version format
    if args.version and not re.match(
        r'^\d+\.\d+\.\d+(-[a-zA-Z0-9.]+)?(\+[a-zA-Z0-9.]+)?$',
        args.version
    ):
        print_error("Version must follow semantic versioning (e.g., 1.0.0, 1.0.0-dev)")
        return 1

    # Determine project root
    if args.project_root:
        project_root = args.project_root.resolve()
    else:
        # Auto-detect: go up from script location to find project root
        script_dir = Path(__file__).parent
        project_root = script_dir.parent.parent

    if not project_root.exists():
        print_error(f"Project root does not exist: {project_root}")
        return 1

    # Detect language
    language = detect_language(project_root)
    if not language:
        print_error(f"Could not detect language in: {project_root}")
        print_info("Supported languages: Go, Ada")
        return 1

    # Get adapter
    adapter = get_adapter(language)
    if not adapter:
        print_error(f"No adapter available for language: {language.value}")
        return 1

    # Load project info
    project_name, project_url = adapter.load_project_info(
        type('Config', (), {'project_root': project_root})()
    )

    # Create config
    config = ReleaseConfig(
        project_root=project_root,
        version=args.version or "0.0.0",
        language=language,
        dry_run=args.dry_run,
    )
    config.project_name = project_name
    config.project_url = project_url

    print_info(f"Project: {project_name}")
    print_info(f"Language: {language.value}")
    print_info(f"Root: {project_root}")

    try:
        if args.action == 'prepare':
            success = prepare_release(config, adapter)
        elif args.action == 'release':
            success = create_release(config, adapter)
        elif args.action == 'diagrams':
            success = generate_diagrams_only(config, adapter)
        elif args.action == 'validate':
            # Future: add validation-only mode
            print_info("Validate action not yet implemented")
            success = True
        else:
            print_error(f"Unknown action: {args.action}")
            return 1

        return 0 if success else 1

    except KeyboardInterrupt:
        print("\n\nRelease process interrupted by user")
        return 1
    except Exception as e:
        print_error(f"Unexpected error: {e}")
        import traceback
        traceback.print_exc()
        return 1


if __name__ == '__main__':
    sys.exit(main())
