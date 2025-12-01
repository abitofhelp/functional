# SPDX-License-Identifier: BSD-3-Clause
# Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
"""
Abstract base class for language-specific adapters.
"""

from abc import ABC, abstractmethod
from pathlib import Path
from typing import List, Tuple, Set

# Support both direct script execution and module import
try:
    from ..models import ArchitectureViolation
except ImportError:
    from models import ArchitectureViolation


class LanguageAdapter(ABC):
    """
    Abstract base class for language-specific architecture validation.

    Each language adapter implements:
    - File extension detection
    - Import/dependency extraction
    - Configuration file validation
    - Language-specific validations (optional)
    """

    @property
    @abstractmethod
    def name(self) -> str:
        """Human-readable language name (e.g., 'Go', 'Ada', 'Rust')."""
        pass

    @property
    @abstractmethod
    def file_extensions(self) -> List[str]:
        """List of source file extensions (e.g., ['.go'], ['.ads', '.adb'])."""
        pass

    @property
    @abstractmethod
    def forbidden_test_imports(self) -> List[str]:
        """List of test framework imports forbidden in production code."""
        pass

    @property
    def source_root_subdir(self) -> str | None:
        """
        Subdirectory under project root where source files live.

        Returns None if sources are at project root (Go),
        or 'src' for Ada projects.
        """
        return None

    @abstractmethod
    def extract_imports(self, file_path: Path) -> List[Tuple[int, str]]:
        """
        Extract all import/dependency statements from a source file.

        Args:
            file_path: Path to source file

        Returns:
            List of (line_number, import_path) tuples
        """
        pass

    @abstractmethod
    def get_layer_from_import(self, import_path: str, project_root: Path) -> str | None:
        """
        Determine which architecture layer an import belongs to.

        Args:
            import_path: The imported module/package path
            project_root: Root directory of the project

        Returns:
            Layer name ('domain', 'application', etc.) or None if not a layer import
        """
        pass

    @abstractmethod
    def validate_config(self, project_root: Path, layers_present: Set[str]) -> Tuple[bool, List[str]]:
        """
        Validate language-specific configuration files.

        Args:
            project_root: Root directory of the project
            layers_present: Set of layer names present in the project

        Returns:
            Tuple of (is_valid, list_of_messages)
        """
        pass

    def is_test_file(self, file_path: Path) -> bool:
        """
        Check if a file is a test file (should skip production code checks).

        Default implementation can be overridden by language adapters.
        """
        return False

    def is_test_support_file(self, file_path: Path, project_root: Path, layer_rules: dict) -> bool:
        """
        Check if a file is test support infrastructure (not production code).

        For example, domain/test/test_framework.go is test infrastructure,
        not production code that should be validated.

        Default implementation can be overridden by language adapters.
        """
        return False

    def language_specific_validations(self, file_path: Path) -> List[ArchitectureViolation]:
        """
        Perform language-specific validations beyond dependency checking.

        Override this method to add language-specific checks like:
        - Ada: pragma vs aspect usage
        - Ada: file naming conventions
        - Rust: mod.rs patterns

        Returns:
            List of violations found (empty list if none)
        """
        return []

    def get_config_step_name(self) -> str:
        """Return the name for config validation step in output."""
        return "Configuration"

    @property
    def domain_allowed_external_prefixes(self) -> Set[str]:
        """
        Prefixes for external packages allowed in Domain layer.

        Domain must have ZERO external crate/module dependencies.
        Only standard library packages are allowed.

        Override in language adapters to specify language-specific stdlib.
        """
        return set()

    def is_domain_allowed_import(self, import_path: str) -> bool:
        """
        Check if an import is allowed in Domain layer.

        Args:
            import_path: The imported package/module path

        Returns:
            True if allowed (stdlib, config, or project-local domain)
        """
        normalized = import_path.lower()

        # Check allowed prefixes (language-specific stdlib)
        for prefix in self.domain_allowed_external_prefixes:
            if normalized.startswith(prefix) or normalized == prefix.rstrip('.'):
                return True

        # Project config packages (*_Config or *_config)
        if normalized.endswith('_config'):
            return True

        # Project-local domain packages (*.domain.* or domain.*)
        if '.domain.' in normalized or normalized.startswith('domain.'):
            return True

        return False
