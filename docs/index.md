# Functional Documentation Index

**Version:** 3.0.0
**Date:** December 06, 2025
**SPDX-License-Identifier:** BSD-3-Clause
**License File:** See the LICENSE file in the project root.
**Copyright:** © 2025 Michael Gardner, A Bit of Help, Inc.
**Status:** Released

---

## Quick Start

- **[Quick Start Guide](quick_start.md)** - Get started with Result, Option, Either, and Try in minutes

---

## User Guides

Practical guides for using the Functional library:

### [Cheatsheet](guides/cheatsheet.md)

Quick reference for all 87 operations across Result, Option, Either, and Try types:

- Constructor functions
- Predicate functions
- Extractors and defaults
- Transform and chain operations
- Operator overloads

### [User Guide](guides/user_guide.md)

Comprehensive guide covering:

- Design philosophy and railway-oriented programming
- SPARK compatibility and formal verification
- Embedded systems considerations
- Best practices and patterns
- Migration guide from v2.x to v3.0.0

---

## Formal Documentation

Comprehensive specifications and design documents:

### [Software Requirements Specification (SRS)](formal/software_requirements_specification.md)

Complete requirements documentation including:

- Functional requirements for Result, Option, Either, and Try types
- Non-functional requirements (purity, performance, compatibility)
- System constraints and dependencies
- Test coverage requirements (227 tests, 95%+ coverage)

### [Software Design Specification (SDS)](formal/software_design_specification.md)

Detailed design documentation covering:

- Generic package architecture
- Type safety and invariants (Boolean discriminants)
- Railway-Oriented Programming patterns
- Exception boundary design (Try module)
- SPARK compatibility considerations

### [Software Test Guide (STG)](formal/software_test_guide.md)

Complete testing documentation including:

- Test strategy and organization
- Running tests (unit tests via AUnit)
- Writing new tests
- Coverage analysis procedures

---

## Quick Links

- [Main README](../README.md) - Project overview and installation
- [CHANGELOG](../CHANGELOG.md) - Release history and changes
- [Tests](../test/) - Complete test suite (227 tests)

---

## Documentation Updates

Documentation is updated during the release process:

- Version package regenerated from alire.toml
- Formal documentation rebuilt from current codebase
- Guide metadata updated with current version/date

For documentation issues or suggestions, please file an issue on GitHub.
