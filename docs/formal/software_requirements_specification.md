# Software Requirements Specification (SRS)

**Project:** Functional - Type-Safe Error Handling Library for Ada 2022
**Version:** 2.3.0
**Date:** December 05, 2025
**Author:** Michael Gardner, A Bit of Help, Inc.
**Status:** Released

---

## 1. Introduction

### 1.1 Purpose

This Software Requirements Specification (SRS) describes the functional and non-functional requirements for the Functional library, a production-ready Ada 2022 library providing type-safe functional programming abstractions for error handling.

### 1.2 Scope

Functional provides:
- `Result[T, E]` type for explicit error handling (success or failure with typed error)
- `Option[T]` type for optional values (presence or absence)
- `Either[L, R]` type for disjoint unions (one of two possible values)
- `Try` bridges for converting exception-based APIs to Result/Option types
- Railway-oriented programming patterns for composable error handling
- Full Ada 2022 contract support (Pre, Post, Inline aspects)

### 1.3 Definitions and Acronyms

- **Result**: A discriminated record representing either a success value (Ok) or an error value (Err)
- **Option**: A discriminated record representing either a present value (Some) or absence (None)
- **Either**: A discriminated record representing one of two possible values (Left or Right)
- **Try**: Exception-to-Result/Option conversion utilities
- **Railway-Oriented Programming (ROP)**: A pattern where operations chain along "happy path" (Ok/Some) or "error track" (Err/None)
- **Monadic Bind**: The `And_Then` operation that chains fallible operations

### 1.4 References

- Ada 2022 Reference Manual (ISO/IEC 8652:2023)
- Semantic Versioning 2.0.0 (semver.org)
- Railway-Oriented Programming (Scott Wlaschin)
- Rust std::result::Result and std::option::Option documentation

## 2. Overall Description

### 2.1 Product Perspective

Functional is a standalone utility library with no external dependencies beyond the Ada standard library. It provides foundational types used by domain, application, and infrastructure layers in hexagonal architecture projects.

The library is designed to:
- Replace exception-based error handling with explicit typed errors
- Enable composition of fallible operations without nested conditionals
- Provide a consistent API familiar to developers from Rust, Haskell, or F#

### 2.2 Product Features

| Feature | Description |
|---------|-------------|
| **Result Type** | 20+ operations for error handling (Ok/Err, Map, And_Then, Map_Err, Recover, etc.) |
| **Option Type** | 11 operations for optional values (Some/None, Map, And_Then, Filter, Or_Else, etc.) |
| **Either Type** | 8 operations for disjoint unions (Left/Right, Map_Left, Map_Right, Bimap, Fold) |
| **Try Bridges** | 5 generic functions for exception-to-Result/Option conversion |
| **Contract Support** | Pre/Post conditions, Inline aspects throughout |
| **Preelaborate** | Option package supports preelaborate instantiation |

### 2.3 User Classes

| User Class | Description |
|------------|-------------|
| **Library Developers** | Build domain/application layers using Result/Option for all fallible operations |
| **Infrastructure Developers** | Use Try bridges at I/O boundaries to convert exceptions to typed errors |
| **API Consumers** | Instantiate generic packages and compose operations via railway-oriented patterns |

### 2.4 Operating Environment

- **Ada Version**: Ada 2022 (requires GNAT >= 13)
- **Build System**: Alire 2.0+ with GPRbuild
- **Platforms**: Any platform supported by GNAT (Linux, macOS, Windows, embedded)
- **Memory Model**: No dynamic allocation in core types; stack-based discriminated records

## 3. Functional Requirements

### 3.1 Result Type (FR-01)

The `Functional.Result` generic package SHALL provide:

**Constructors:**
- `Ok(V)` - Create success result containing value V
- `Err(E)` - Create error result containing error E
- `From_Error(E)` - Alias for Err (infrastructure convenience)

**Predicates:**
- `Is_Ok(R)` - Returns True if R is Ok
- `Is_Err(R)` - Returns True if R is Err

**Extractors:**
- `Value(R)` - Extract Ok value (Pre: Is_Ok)
- `Error(R)` - Extract Err value (Pre: Is_Err)
- `Expect(R, Msg)` - Extract Ok or raise with message

**Defaults:**
- `Unwrap_Or(R, Default)` - Return value or default
- `Unwrap_Or_With(R)` - Return value or call lazy function

**Transformations:**
- `Map(R)` - Transform Ok value, pass Err unchanged
- `And_Then(R)` - Chain fallible operation (monadic bind)
- `And_Then_Into(R)` - Chain with type transformation
- `Map_Err(R)` - Transform Err value, pass Ok unchanged
- `Bimap(R)` - Transform both Ok and Err simultaneously

**Recovery:**
- `Fallback(A, B)` - Return A if Ok, else B (eager)
- `Fallback_With(R)` - Lazy fallback
- `Recover(R)` - Convert Err to value
- `Recover_With(R)` - Convert Err to Result

**Validation:**
- `Ensure(R)` - Validate Ok value with predicate
- `With_Context(R, Msg)` - Append context to Err

**Side Effects:**
- `Tap(R)` - Execute callbacks without changing Result

### 3.2 Option Type (FR-02)

The `Functional.Option` generic package SHALL provide:

**Constructors:**
- `New_Some(V)` - Create Option containing value V
- `None` - Create empty Option

**Predicates:**
- `Is_Some(O)` - Returns True if O contains a value
- `Is_None(O)` - Returns True if O is empty

**Extractors:**
- `Value(O)` - Extract value (Pre: Is_Some)

**Defaults:**
- `Unwrap_Or(O, Default)` - Return value or default
- `Unwrap_Or_With(O)` - Return value or call lazy function

**Transformations:**
- `Map(O)` - Transform Some value, pass None unchanged
- `And_Then(O)` - Chain optional operation (monadic bind)
- `Filter(O)` - Keep value only if predicate holds

**Fallback:**
- `Or_Else(A, B)` - Return A if Some, else B (eager)
- `Or_Else_With(O)` - Lazy fallback
- `Fallback` - Alias for Or_Else

### 3.3 Either Type (FR-03)

The `Functional.Either` generic package SHALL provide:

**Constructors:**
- `Left(V)` - Create Either with Left value
- `Right(V)` - Create Either with Right value

**Predicates:**
- `Is_Left(E)` - Returns True if E is Left
- `Is_Right(E)` - Returns True if E is Right

**Extractors:**
- `Left_Value(E)` - Extract Left value (Pre: Is_Left)
- `Right_Value(E)` - Extract Right value (Pre: Is_Right)

**Transformations:**
- `Map_Left(E)` - Transform Left value only
- `Map_Right(E)` - Transform Right value only
- `Bimap(E)` - Transform both values simultaneously
- `Fold(E)` - Reduce to single value via handlers

### 3.4 Try Bridges (FR-04)

The `Functional.Try` package SHALL provide:

**General Bridge:**
- `Try_To_Result` - Convert exception-throwing action to any Result type

**Convenience Wrappers:**
- `Try_To_Functional_Result` - Bridge to Functional.Result
- `Try_To_Functional_Option` - Bridge to Functional.Option

**Parameterized Bridges:**
- `Try_To_Result_With_Param` - Result bridge with input parameter
- `Try_To_Option_With_Param` - Option bridge with input parameter

### 3.5 Child Packages (FR-05)

Backwards-compatible child packages SHALL provide legacy API:
- `Functional.Try.To_Result` - Child package with `.Run` function
- `Functional.Try.To_Option` - Child package with `.Run` function

## 4. Non-Functional Requirements

### 4.1 Performance (NFR-01)

- All core operations SHALL be marked with `Inline` aspect
- No dynamic memory allocation in Result, Option, or Either types
- Zero-overhead abstraction: generated code equivalent to manual if/case

### 4.2 Reliability (NFR-02)

- Pre/Post contracts SHALL enforce correct usage at compile time (assertions enabled)
- No exceptions SHALL propagate from core type operations
- Try bridges SHALL catch all exceptions and convert to typed errors

### 4.3 Portability (NFR-03)

- Library SHALL compile on any GNAT >= 13 target
- No platform-specific code in core packages
- Library type (static, relocatable, static-pic) configurable via GPR

### 4.4 Maintainability (NFR-04)

- 90%+ statement+decision test coverage target
- All public API documented with purpose and usage
- Consistent naming following Ada and functional programming conventions

### 4.5 Usability (NFR-05)

- API naming familiar to Rust/Haskell developers
- Generic packages instantiable with any `private` type
- Option package supports `Preelaborate` for strict elaboration contexts

## 5. System Requirements

### 5.1 Hardware Requirements

None - pure library with no hardware dependencies.

### 5.2 Software Requirements

| Component | Requirement |
|-----------|-------------|
| Ada Compiler | GNAT >= 13 (FSF or Pro) |
| Build System | Alire >= 2.0, GPRbuild |
| Test Coverage | GNATcoverage (optional) |

## 6. Verification and Validation

### 6.1 Test Coverage

| Suite | Tests | Coverage Target |
|-------|-------|-----------------|
| Unit Tests | 93 | 90%+ stmt+decision |
| Result | 35 | 91% |
| Option | 22 | 100% |
| Either | 16 | 100% |
| Try | 14 | 100% |
| Try_Option | 6 | 100% |

### 6.2 Verification Methods

| Requirement | Method |
|-------------|--------|
| FR-01 to FR-05 | Unit tests exercising all operations |
| NFR-01 | Code review for Inline aspects |
| NFR-02 | Tests verify no exceptions from core operations |
| NFR-03 | Build on multiple targets |
| NFR-04 | GNATcoverage analysis |
| NFR-05 | API review against Rust documentation |

## 7. Appendices

### 7.1 API Summary

| Package | Operations |
|---------|------------|
| Functional.Result | 20 (Ok, Err, Is_Ok, Is_Err, Value, Error, Expect, Unwrap_Or, Unwrap_Or_With, Map, And_Then, And_Then_Into, Map_Err, Bimap, Fallback, Fallback_With, Recover, Recover_With, Ensure, With_Context, Tap) |
| Functional.Option | 11 (New_Some, None, Is_Some, Is_None, Value, Unwrap_Or, Unwrap_Or_With, Map, And_Then, Filter, Or_Else, Or_Else_With) |
| Functional.Either | 8 (Left, Right, Is_Left, Is_Right, Left_Value, Right_Value, Map_Left, Map_Right, Bimap, Fold) |
| Functional.Try | 5 (Try_To_Result, Try_To_Functional_Result, Try_To_Functional_Option, Try_To_Result_With_Param, Try_To_Option_With_Param) |

### 7.2 Project Statistics

- Source Files: 13
- Test Files: 8
- Total Tests: 93
- Code Coverage: 95% (stmt+decision)
- Lines of Code: ~1,200 (excluding tests)
