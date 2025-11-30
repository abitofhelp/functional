# SPDX-License-Identifier: BSD-3-Clause
# Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
"""
Go language adapter for architecture validation.
"""

import re
from pathlib import Path
from typing import List, Tuple, Set

from .base import LanguageAdapter

# Support both direct script execution and module import
try:
    from ..models import ArchitectureViolation
except ImportError:
    from models import ArchitectureViolation


class GoAdapter(LanguageAdapter):
    """
    Go-specific architecture validation adapter.

    Handles:
    - .go file extension
    - import statement parsing
    - go.mod configuration validation
    """

    # Cache for module path
    _module_path_cache: dict = {}

    @property
    def name(self) -> str:
        return "Go"

    @property
    def file_extensions(self) -> List[str]:
        return ['.go']

    @property
    def forbidden_test_imports(self) -> List[str]:
        return ['testing', 'testify', 'assert', 'require', 'mock']

    def extract_imports(self, file_path: Path) -> List[Tuple[int, str]]:
        """Extract all import statements from a Go file."""
        imports = []

        try:
            with open(file_path, 'r', encoding='utf-8') as f:
                in_import_block = False

                for line_num, line in enumerate(f, start=1):
                    stripped = line.strip()

                    if stripped.startswith('import ('):
                        in_import_block = True
                        continue

                    if in_import_block:
                        if stripped == ')':
                            in_import_block = False
                            continue

                        # Match import with optional alias (word, dot, or underscore)
                        # Handles: "pkg", alias "pkg", . "pkg", _ "pkg"
                        match = re.match(r'^\s*(?:[\w.]+\s+)?"([^"]+)"', stripped)
                        if match:
                            imports.append((line_num, match.group(1)))
                    else:
                        # Match single import with optional alias
                        match = re.match(r'^\s*import\s+(?:[\w.]+\s+)?"([^"]+)"', stripped)
                        if match:
                            imports.append((line_num, match.group(1)))

        except Exception as e:
            print(f"Warning: Could not read {file_path}: {e}")

        return imports

    def _get_module_path(self, project_root: Path) -> str:
        """Extract the module path from the root go.mod file."""
        cache_key = str(project_root)
        if cache_key in self._module_path_cache:
            return self._module_path_cache[cache_key]

        root_gomod = project_root / 'go.mod'
        module_path = ""

        if root_gomod.exists():
            try:
                with open(root_gomod, 'r', encoding='utf-8') as f:
                    for line in f:
                        match = re.match(r'^\s*module\s+(.+)\s*$', line)
                        if match:
                            module_path = match.group(1).strip()
                            break
            except Exception as e:
                print(f"Warning: Could not read root go.mod: {e}")

        self._module_path_cache[cache_key] = module_path
        return module_path

    def get_layer_from_import(self, import_path: str, project_root: Path) -> str | None:
        """Determine which layer a Go import belongs to."""
        module_path = self._get_module_path(project_root)

        if not module_path or not import_path.startswith(module_path):
            return None

        relative = import_path[len(module_path):].lstrip('/')
        parts = relative.split('/')

        # Layer names must match LAYER_RULES keys
        layer_names = {'domain', 'application', 'infrastructure', 'api', 'presentation', 'bootstrap'}
        if parts and parts[0] in layer_names:
            return parts[0]

        return None

    def validate_config(self, project_root: Path, layers_present: Set[str]) -> Tuple[bool, List[str]]:
        """Validate go.mod files are configured correctly."""
        valid = True
        messages = []

        # Import layer rules here to avoid circular import
        try:
            from ..arch_guard import ArchitectureGuard
        except ImportError:
            from arch_guard import ArchitectureGuard
        layer_rules = ArchitectureGuard.LAYER_RULES

        for layer in sorted(layers_present):
            gomod_path = project_root / layer / 'go.mod'

            if not gomod_path.exists():
                messages.append(f"  ⚠ {layer:15} - no go.mod file")
                continue

            layer_deps = set()
            try:
                with open(gomod_path, 'r', encoding='utf-8') as f:
                    content = f.read()

                    module_path = self._get_module_path(project_root)

                    # Match single-line require: require github.com/... v0.0.0
                    for match in re.finditer(r'^\s*require\s+([^\s(]+)\s+v', content, re.MULTILINE):
                        dep = match.group(1)
                        dep_layer = self.get_layer_from_import(dep, project_root)
                        if dep_layer:
                            layer_deps.add(dep_layer)

                    # Match multi-line require block: require ( ... )
                    require_block = re.search(r'require\s*\((.*?)\)', content, re.DOTALL)
                    if require_block:
                        block_content = require_block.group(1)
                        # Match each package line in the block
                        for match in re.finditer(r'^\s*([^\s]+)\s+v', block_content, re.MULTILINE):
                            dep = match.group(1)
                            dep_layer = self.get_layer_from_import(dep, project_root)
                            if dep_layer:
                                layer_deps.add(dep_layer)

            except Exception as e:
                messages.append(f"  ❌ {layer:15} - error reading go.mod: {e}")
                valid = False
                continue

            allowed_deps = layer_rules.get(layer, set())
            forbidden = layer_deps - allowed_deps
            if forbidden:
                messages.append(f"  ❌ {layer:15} - forbidden dependencies: {', '.join(sorted(forbidden))}")
                valid = False
            else:
                deps_str = ', '.join(sorted(layer_deps)) if layer_deps else 'none'
                messages.append(f"  ✓ {layer:15} - dependencies: {deps_str}")

        return valid, messages

    def is_test_file(self, file_path: Path) -> bool:
        """Check if this is a Go test file (*_test.go)."""
        return file_path.name.endswith('_test.go')

    def is_test_support_file(self, file_path: Path, project_root: Path, layer_rules: dict) -> bool:
        """
        Check if file is test support infrastructure.

        For example, domain/test/test_framework.go is test support code,
        not production code that should be validated for test imports.
        """
        try:
            relative = file_path.relative_to(project_root)
            parts = relative.parts
            # Check if file is in a layer's test subdirectory (e.g., domain/test/)
            if len(parts) >= 2 and parts[0] in layer_rules and parts[1] == 'test':
                return True
        except ValueError:
            pass
        return False

    def get_config_step_name(self) -> str:
        return "go.mod Configuration"
