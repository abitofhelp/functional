# Functional Documentation Index

**Version:** 2.2.0
**Date:** November 30, 2025
**SPDX-License-Identifier:** BSD-3-Clause
**License File:** See the LICENSE file in the project root.
**Copyright:** © 2025 Michael Gardner, A Bit of Help, Inc.
**Status:** Released

---

## Formal Documentation

Comprehensive specifications and design documents for the Functional library:

### [Software Requirements Specification (SRS)](formal/software_requirements_specification.md)
Complete requirements documentation including:
- Functional requirements for Result, Option, Either, and Try types
- Non-functional requirements (purity, performance, compatibility)
- System constraints and dependencies
- Test coverage requirements

### [Software Design Specification (SDS)](formal/software_design_specification.md)
Detailed design documentation covering:
- Generic package architecture
- Type safety and invariants
- Railway-Oriented Programming patterns
- Exception boundary design (Try module)

### [Software Test Guide](formal/software_test_guide.md)
Complete testing documentation including:
- Test strategy and organization
- Running tests (unit, property-based)
- Writing new tests
- Coverage analysis procedures

---

## Quick Start

- **[Quick Start Guide](quick_start.md)** - Get started with Result, Option, Either, and Try

---

## Development Guides

Additional guides for developers:

- **[All About Our API](common/guides/all_about_our_api.md)** - API layer architecture and usage
- **[Architecture Enforcement](common/guides/architecture_enforcement.md)** - Hexagonal architecture rules
- **[Build Profiles](common/guides/build_profiles.md)** - Profile configuration for different targets
- **[Error Handling Strategy](common/guides/error_handling_strategy.md)** - Result monad patterns

---

## Visual Documentation

Architecture and sequence diagrams:

- **Diagrams** - PlantUML source in `docs/common/diagrams/`

---

## Quick Links

- [Main README](../README.md) - Project overview and quick start
- [CHANGELOG](../CHANGELOG.md) - Release history and changes
- [Examples](../examples/) - Working code examples
- [Tests](../test/) - Complete test suite

---

## Documentation Updates

All documentation is automatically updated during the release process:
- Ada source file docstrings regenerated with current copyright year
- Formal documentation rebuilt from current codebase
- Guide metadata updated with current version/date
- Diagrams regenerated from PlantUML sources

For documentation issues or suggestions, please file an issue on GitHub.
