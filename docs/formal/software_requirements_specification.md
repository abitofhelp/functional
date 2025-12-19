# Software Requirements Specification

**Version:** 4.1.0<br>
**Date:** 2025-12-18<br>
**SPDX-License-Identifier:** BSD-3-Clause<br>
**License File:** See the LICENSE file in the project root<br>
**Copyright:** © 2025 Michael Gardner, A Bit of Help, Inc.<br>
**Status:** Released

---

## 1. Introduction

### 1.1 Purpose

This Software Requirements Specification (SRS) defines the functional and non-functional requirements for the Functional library, a type-safe functional programming library for Ada 2022.

### 1.2 Scope

The Functional library provides:
- Result type for explicit error handling
- Option type for optional values
- Either type for neutral disjunction
- Try bridges for exception-to-Result/Option conversion
- Scoped guards for RAII resource management

### 1.3 Definitions and Acronyms

| Term | Definition |
|------|------------|
| Result | A discriminated union representing success (Ok) or failure (Error) |
| Option | A discriminated union representing presence (Some) or absence (None) |
| Either | A discriminated union representing one of two valid values (Left or Right) |
| Railway-Oriented Programming | A pattern where operations are chained, with errors automatically propagating |
| RAII | Resource Acquisition Is Initialization - automatic cleanup pattern |
| SPARK | SPARK Ada - a formally verifiable subset of Ada |

### 1.4 References

- Ada 2022 Language Reference Manual
- SPARK 2014 Reference Manual
- Functional Programming in Ada (internal design document)

---

## 2. Overall Description

### 2.1 Product Perspective

The Functional library is a standalone Ada 2022 library designed to enable functional programming patterns in Ada applications. It integrates with the Ada ecosystem through Alire package management.

### 2.2 Product Features

- **Result[T, E]**: Type-safe success/failure handling
- **Option[T]**: Null-safe optional values
- **Either[L, R]**: Neutral disjunction without error semantics
- **Try**: Exception boundary adapters
- **Scoped**: RAII guards for resource cleanup

### 2.3 User Classes

| User Class | Description |
|------------|-------------|
| Library Users | Ada developers using Functional in their projects |
| Domain Developers | Developers implementing business logic with Result/Option |
| Infrastructure Developers | Developers implementing I/O with Try bridges |

### 2.4 Operating Environment

- Ada 2022 compatible compiler (GNAT 14+)
- Any platform supported by GNAT (Linux, macOS, Windows)
- Alire 2.0+ for dependency management

### 2.5 Constraints

- No heap allocation in core types (stack-based discriminated unions)
- SPARK compatibility for Domain/Application layers
- No runtime dependencies beyond Ada standard library

---

## 3. Functional Requirements

### 3.1 Result Module

| ID | Requirement | Priority |
|----|-------------|----------|
| FR-R01 | SHALL provide Ok constructor to create success values | Must |
| FR-R02 | SHALL provide Error constructor to create failure values | Must |
| FR-R03 | SHALL provide Is_Ok/Is_Error discriminant checks | Must |
| FR-R04 | SHALL provide Value accessor with precondition Is_Ok | Must |
| FR-R05 | SHALL provide Get_Error accessor with precondition Is_Error | Must |
| FR-R06 | SHALL provide Map to transform success values | Must |
| FR-R07 | SHALL provide Map_Error to transform error values | Must |
| FR-R08 | SHALL provide And_Then for chaining fallible operations | Must |
| FR-R09 | SHALL provide Or_Else for error recovery | Must |
| FR-R10 | SHALL provide Unwrap_Or for default extraction | Must |
| FR-R11 | SHALL provide Match for pattern matching | Should |
| FR-R12 | SHALL provide Recover for error-to-success conversion | Should |

### 3.2 Option Module

| ID | Requirement | Priority |
|----|-------------|----------|
| FR-O01 | SHALL provide Some constructor for present values | Must |
| FR-O02 | SHALL provide None constructor for absent values | Must |
| FR-O03 | SHALL provide Is_Some/Is_None discriminant checks | Must |
| FR-O04 | SHALL provide Value accessor with precondition Is_Some | Must |
| FR-O05 | SHALL provide Map to transform present values | Must |
| FR-O06 | SHALL provide And_Then for chaining optional operations | Must |
| FR-O07 | SHALL provide Or_Else for absence recovery | Must |
| FR-O08 | SHALL provide Unwrap_Or for default extraction | Must |
| FR-O09 | SHALL provide Filter to conditionally keep values | Should |
| FR-O10 | SHALL provide Match for pattern matching | Should |

### 3.3 Either Module

| ID | Requirement | Priority |
|----|-------------|----------|
| FR-E01 | SHALL provide Left constructor | Must |
| FR-E02 | SHALL provide Right constructor | Must |
| FR-E03 | SHALL provide Is_Left/Is_Right discriminant checks | Must |
| FR-E04 | SHALL provide Left_Value accessor with precondition Is_Left | Must |
| FR-E05 | SHALL provide Right_Value accessor with precondition Is_Right | Must |
| FR-E06 | SHALL provide Map_Left to transform left values | Must |
| FR-E07 | SHALL provide Map_Right to transform right values | Must |
| FR-E08 | SHALL provide Fold for consuming either side | Should |

### 3.4 Try Module

| ID | Requirement | Priority |
|----|-------------|----------|
| FR-T01 | SHALL provide Map_To_Result for declarative exception mapping | Must |
| FR-T02 | SHALL provide Map_To_Result_With_Param for parameterized actions | Must |
| FR-T03 | SHALL support empty mappings for catch-all behavior | Must |
| FR-T04 | SHALL support multiple exception-to-error mappings | Must |
| FR-T05 | SHALL provide To_Result child package for procedural mapping | Should |
| FR-T06 | SHALL provide To_Option child package for probe operations | Should |
| FR-T07 | SHALL include exception message in error construction | Must |

### 3.5 Scoped Module

| ID | Requirement | Priority |
|----|-------------|----------|
| FR-S01 | SHALL provide Guard_For for unconditional RAII | Must |
| FR-S02 | SHALL provide Conditional_Guard_For for conditional RAII | Must |
| FR-S03 | SHALL catch and suppress exceptions from Release | Must |
| FR-S04 | SHALL support user-defined Release procedures | Must |

---

## 4. Non-Functional Requirements

| ID | Category | Requirement |
|----|----------|-------------|
| NFR-01 | Performance | SHALL have zero heap allocation in core operations |
| NFR-01 | Performance | SHALL use static dispatch (no tagged types in core) |
| NFR-02 | Reliability | SHALL handle all exceptions at Try boundaries |
| NFR-02 | Reliability | SHALL not propagate exceptions from Scoped guards |
| NFR-03 | Portability | SHALL compile on GNAT 14+ across Linux, macOS, Windows |
| NFR-03 | Portability | SHALL have no platform-specific code in library |
| NFR-04 | Maintainability | SHALL achieve ≥90% test coverage |
| NFR-04 | Maintainability | SHALL follow hybrid DDD/Clean/Hexagonal architecture |
| NFR-05 | Usability | SHALL provide clear error messages |
| NFR-05 | Usability | SHALL provide comprehensive documentation |
| NFR-06 | Platform Abstraction | SHALL isolate exception handling to Try module |
| NFR-07 | SPARK Verification | SHALL verify Domain types with SPARK_Mode => On |
| NFR-07 | SPARK Verification | SHALL mark Try/Scoped with SPARK_Mode => Off |
| NFR-08 | Testability | SHALL support unit testing of all public operations |
| NFR-08 | Testability | SHALL provide instantiation tests for SPARK compatibility |

---

## 5. System Requirements

### 5.1 Hardware Requirements

No specific hardware requirements. The library operates within standard Ada runtime constraints.

### 5.2 Software Requirements

| Component | Requirement |
|-----------|-------------|
| Compiler | GNAT 14+ (Ada 2022 support) |
| Package Manager | Alire 2.0+ |
| SPARK Prover | GNATprove 14+ (optional, for verification) |

---

## 6. Interface Requirements

### 6.1 User Interfaces

Not applicable - this is a library, not an application.

### 6.2 Software Interfaces

| Interface | Description |
|-----------|-------------|
| Ada.Exceptions | Used by Try module for exception handling |
| Ada.Finalization | Used by Scoped module for RAII |

### 6.3 Hardware Interfaces

Not applicable.

---

## 7. Verification and Validation

### 7.1 Verification Methods

| Method | Scope |
|--------|-------|
| Unit Testing | All public operations of Result, Option, Either, Try, Scoped |
| SPARK Proof | Result, Option, Either core operations |
| Integration Testing | Try with real exception scenarios |
| Code Review | All changes before merge |

### 7.2 Traceability Matrix

See [Software Test Guide](software_test_guide.md) Section 8 for requirements-to-tests mapping.

---

## 8. Appendices

### Appendix A: Glossary

| Term | Definition |
|------|------------|
| Discriminated Union | Ada record type with discriminant controlling variant |
| Monad | Abstraction for sequencing computations (Map, And_Then) |
| Functor | Abstraction for transforming wrapped values (Map) |
| Applicative | Abstraction for applying wrapped functions |

### Appendix B: Module Statistics

| Module | Operations | SPARK_Mode |
|--------|------------|------------|
| Result | 34 | On |
| Option | 26 | On |
| Either | 18 | On |
| Try | 6 + child packages | Off |
| Scoped | 2 | Off |

---

**Document Control:**
- Version: 4.1.0
- Last Updated: 2025-12-18
- Status: Released
