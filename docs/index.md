# Functional Documentation Index

**Version:** 4.0.0
**Date:** December 12, 2025
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

Quick reference for all 87 operations across Result, Option, Either, Try, and Scoped types:

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
- Migration guide from v3.x to v4.0.0

---

## Formal Documentation

Comprehensive specifications and design documents:

### [Software Requirements Specification (SRS)](formal/software_requirements_specification.md)

Complete requirements documentation including:

- Functional requirements for Result, Option, Either, Try, and Scoped types
- Non-functional requirements (purity, performance, compatibility)
- System constraints and dependencies
- Test coverage requirements (269 tests, 95%+ coverage)

### [Software Design Specification (SDS)](formal/software_design_specification.md)

Detailed design documentation covering:

- Generic package architecture
- Type safety and invariants (Boolean discriminants)
- Railway-Oriented Programming patterns
- Exception boundary design (Try module)
- SPARK compatibility considerations

### [Software Test Guide (STG)](formal/software_test_guide.md)

Complete testing documentation including:

- Test strategy and organization (269 tests: Result 84, Option 65, Either 58, Try 14, Try_Option 6, Scoped 11, Map_To_Result 31)
- Running tests (unit tests via AUnit)
- Writing new tests
- Coverage analysis procedures

---

## Quick Links

- [Main README](../README.md) - Project overview and installation
- [CHANGELOG](../CHANGELOG.md) - Release history and changes
- [Tests](../test/) - Complete test suite (269 tests)

---

## Documentation Updates

Documentation is updated during the release process:

- Version package regenerated from alire.toml
- Formal documentation rebuilt from current codebase
- Guide metadata updated with current version/date

For documentation issues or suggestions, please file an issue on GitHub.
