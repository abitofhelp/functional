# Software Requirements Specification (SRS)

**Project:** Functional - Type-Safe Error Handling Library for Ada 2022
**Version:** 3.0.0  
**Date:** December 06, 2025  
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
- SPARK compatibility for formal verification

### 1.3 Definitions and Acronyms

- **Result**: A discriminated record representing either a success value (Ok) or an error value (Error)
- **Option**: A discriminated record representing either a present value (Some) or absence (None)
- **Either**: A discriminated record representing one of two possible values (Left or Right)
- **Try**: Exception-to-Result/Option conversion utilities
- **Railway-Oriented Programming (ROP)**: A pattern where operations chain along "happy path" (Ok/Some) or "error track" (Error/None)
- **Monadic Bind**: The `And_Then` operation that chains fallible operations
- **SPARK**: Subset of Ada designed for formal verification

### 1.4 References

- Ada 2022 Reference Manual (ISO/IEC 8652:2023)
- Semantic Versioning 2.0.0 (semver.org)
- Railway-Oriented Programming (Scott Wlaschin)
- Rust std::result::Result and std::option::Option documentation
- SPARK 2014 Reference Manual

## 2. Overall Description

### 2.1 Product Perspective

Functional is a standalone utility library with no external dependencies beyond the Ada standard library. It provides foundational types used by domain, application, and infrastructure layers in hexagonal architecture projects.

The library is designed to:
- Replace exception-based error handling with explicit typed errors
- Enable composition of fallible operations without nested conditionals
- Provide a consistent API familiar to developers from Rust, Haskell, or F#
- Support formal verification through SPARK compatibility

### 2.2 Product Features

| Feature | Description |
|---------|-------------|
| **Result Type** | 36 operations for error handling (constructors, predicates, extractors, transforms, recovery, operators) |
| **Option Type** | 26 operations for optional values (constructors, predicates, transforms, fallbacks, operators) |
| **Either Type** | 20 operations for disjoint unions (constructors, predicates, transforms, conversions, operators) |
| **Try Bridges** | 5 generic functions for exception-to-Result/Option conversion |
| **SPARK Compatibility** | Option, Result, Either are `SPARK_Mode => On` for formal verification |
| **Contract Support** | Pre/Post conditions with postconditions for prover assistance |
| **Preelaborate** | Option, Result, Either packages support preelaborate instantiation |

### 2.3 User Classes

| User Class | Description |
|------------|-------------|
| **Library Developers** | Build domain/application layers using Result/Option for all fallible operations |
| **Infrastructure Developers** | Use Try bridges at I/O boundaries to convert exceptions to typed errors |
| **SPARK Users** | Leverage formal verification for safety-critical applications |
| **Embedded Developers** | Use zero-allocation types in resource-constrained environments |

### 2.4 Operating Environment

- **Ada Version**: Ada 2022 (requires GNAT >= 13)
- **Build System**: Alire 2.0+ with GPRbuild
- **Platforms**: Any platform supported by GNAT (Linux, macOS, Windows, embedded)
- **Memory Model**: No dynamic allocation in core types; stack-based discriminated records
- **SPARK**: GNATprove compatible for formal verification

## 3. Functional Requirements

### 3.1 Result Type (FR-01)

The `Functional.Result` generic package SHALL provide 36 operations:

**Constructors (3):**
- `Ok(V)` - Create success result containing value V
- `New_Error(E)` - Create error result containing error E
- `From_Error(E)` - Alias for New_Error (infrastructure convenience)

**Predicates (7):**
- `Is_Ok(R)` - Returns True if R is Ok
- `Is_Error(R)` - Returns True if R is Error
- `Is_Ok_And(R)` - True if Ok and predicate holds on value (strict)
- `Is_Error_And(R)` - True if Error and predicate holds on error (strict)
- `Is_Ok_Or(R)` - True if Error or predicate holds on Ok value (lenient)
- `Is_Error_Or(R)` - True if Ok or predicate holds on Error value (lenient)
- `Contains(R, V)` - True if Ok value equals V

**Extractors (5):**
- `Value(R)` - Extract Ok value (Pre: Is_Ok)
- `Error(R)` - Extract Error value (Pre: Is_Error)
- `Expect(R, Msg)` - Extract Ok or raise with message
- `Expect_Error(R, Msg)` - Extract Error or raise with message
- `Unwrap_Error(R)` - Extract Error (Pre: Is_Error)

**Defaults (2):**
- `Unwrap_Or(R, Default)` - Return value or default
- `Unwrap_Or_With(R)` - Return value or call lazy function

**Transformations (12):**
- `Map(R)` - Transform Ok value, pass Error unchanged
- `Map_Or(R, Default)` - Transform Ok value or return default (eager)
- `Map_Or_Else(R)` - Transform Ok value or call default producer (lazy)
- `And_Then(R)` - Chain fallible operation (monadic bind)
- `And_Then_Into(R)` - Chain with type transformation
- `Map_Error(R)` - Transform Error value, pass Ok unchanged
- `Bimap(R)` - Transform both Ok and Error simultaneously
- `Zip_With(R1, R2)` - Combine two Results with a function
- `Flatten(R)` - Unwrap nested Result[Result[T,E],E] to Result[T,E]
- `To_Option(R)` - Convert Ok(v) to Some(v), Error(_) to None

**Recovery (4):**
- `Fallback(A, B)` - Return A if Ok, else B (eager)
- `Fallback_With(R)` - Lazy fallback
- `Recover(R)` - Convert Error to value
- `Recover_With(R)` - Convert Error to Result

**Validation (2):**
- `Ensure(R)` - Validate Ok value with predicate
- `With_Context(R, Msg)` - Append context to Error

**Side Effects (3):**
- `Tap(R)` - Execute callbacks on both Ok and Error
- `Tap_Ok(R)` - Execute callback on Ok only
- `Tap_Error(R)` - Execute callback on Error only

**Operators (3):**
- `"or"` for Unwrap_Or - `R or Default` syntax
- `"or"` for Fallback - `A or B` syntax
- `"="` for Contains - `R = V` syntax

### 3.2 Option Type (FR-02)

The `Functional.Option` generic package SHALL provide 26 operations:

**Constructors (2):**
- `New_Some(V)` - Create Option containing value V
- `None` - Create empty Option

**Predicates (5):**
- `Is_Some(O)` - Returns True if O contains a value
- `Is_None(O)` - Returns True if O is empty
- `Is_Some_And(O)` - True if Some and predicate holds (strict)
- `Is_None_Or(O)` - True if None or predicate holds on Some (lenient)
- `Contains(O, V)` - True if Some value equals V

**Extractors (2):**
- `Value(O)` - Extract value (Pre: Has_Value)
- `Expect(O, Msg)` - Extract value or raise with message

**Defaults (2):**
- `Unwrap_Or(O, Default)` - Return value or default
- `Unwrap_Or_With(O)` - Return value or call lazy function

**Transformations (9):**
- `Map(O)` - Transform Some value, pass None unchanged
- `Map_Or(O, Default)` - Transform Some value or return default (eager)
- `Map_Or_Else(O)` - Transform Some value or call default producer (lazy)
- `And_Then(O)` - Chain optional operation (monadic bind)
- `Filter(O)` - Keep value only if predicate holds
- `Zip_With(O1, O2)` - Combine two Options with a function
- `Flatten(O)` - Unwrap nested Option[Option[T]] to Option[T]
- `Ok_Or(O, E)` - Convert Some(v) to Ok(v), None to Error(e) (eager)
- `Ok_Or_Else(O)` - Convert Some(v) to Ok(v), None to Error(f()) (lazy)

**Fallback (3):**
- `Or_Else(A, B)` - Return A if Some, else B (eager)
- `Or_Else_With(O)` - Lazy fallback
- `Fallback` - Alias for Or_Else

**Side Effects (1):**
- `Tap(O)` - Execute callback on Some value

**Operators (4):**
- `"or"` for Unwrap_Or - `O or Default` syntax
- `"or"` for Or_Else - `A or B` syntax
- `"and"` - Returns second when both have values
- `"xor"` - Returns one when exactly one has value
- `"="` for Contains - `O = V` syntax

### 3.3 Either Type (FR-03)

The `Functional.Either` generic package SHALL provide 20 operations:

**Constructors (2):**
- `Left(V)` - Create Either with Left value
- `Right(V)` - Create Either with Right value

**Predicates (7):**
- `Is_Left(E)` - Returns True if E is Left
- `Is_Right(E)` - Returns True if E is Right
- `Is_Left_And(E)` - True if Left and predicate holds (strict)
- `Is_Right_And(E)` - True if Right and predicate holds (strict)
- `Is_Left_Or(E)` - True if Right or predicate holds on Left (lenient)
- `Is_Right_Or(E)` - True if Left or predicate holds on Right (lenient)
- `Contains(E, V)` - True if Right value equals V

**Extractors (3):**
- `Left_Value(E)` - Extract Left value (Pre: Is_Left)
- `Right_Value(E)` - Extract Right value (Pre: Is_Right)
- `Get_Or_Else(E, Default)` - Get Right value or default

**Transformations (6):**
- `Map(E)` - Right-biased transform (convenience)
- `Map_Left(E)` - Transform Left value only
- `Map_Right(E)` - Transform Right value only
- `Bimap(E)` - Transform both values simultaneously
- `Swap(E)` - Exchange Left and Right values
- `And_Then(E)` - Right-biased monadic bind

**Reduction (2):**
- `Fold(E)` - Reduce to single value via handlers
- `Merge(E)` - Extract value when L and R are same type

**Conversion (2):**
- `To_Option(E)` - Convert Right(v) to Some(v), Left(_) to None
- `To_Result(E)` - Convert Right(v) to Ok(v), Left(e) to Error(e)

**Operators (1):**
- `"="` for Contains - `E = V` syntax

### 3.4 Try Bridges (FR-04)

The `Functional.Try` package SHALL provide 5 generic functions:

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
- Option, Result, Either packages support `Preelaborate` for strict elaboration contexts

### 4.6 SPARK Formal Verification (NFR-06)

| ID | Requirement |
|----|-------------|
| NFR-06.1 | Library SHALL pass SPARK legality checking (gnatprove --mode=check) |
| NFR-06.2 | Option, Result, Either, Version packages SHALL use `SPARK_Mode => On` |
| NFR-06.3 | Try package SHALL use `SPARK_Mode => Off` (exception boundary by design) |
| NFR-06.4 | All transform operations SHALL have postconditions for prover assistance |
| NFR-06.5 | No runtime errors provable (overflow, range, division by zero) |
| NFR-06.6 | All variables SHALL be properly initialized before use |
| NFR-06.7 | SPARK legality verification SHALL be runnable via `make spark-check` |
| NFR-06.8 | SPARK proof verification SHALL be runnable via `make spark-prove` |
| NFR-06.9 | No heap allocation, no controlled types, Ravenscar compatible |

**Verification Scope:**

| Package | SPARK_Mode | Rationale |
|---------|-----------|-----------|
| Functional.Result | On | Core monad, formally verifiable |
| Functional.Option | On | Core monad, formally verifiable |
| Functional.Either | On | Core type, formally verifiable |
| Functional.Version | On | Pure data, formally verifiable |
| Functional.Try | Off | Exception boundary by design |

## 5. System Requirements

### 5.1 Hardware Requirements

None - pure library with no hardware dependencies.

### 5.2 Software Requirements

| Component | Requirement |
|-----------|-------------|
| Ada Compiler | GNAT >= 13 (FSF or Pro) |
| Build System | Alire >= 2.0, GPRbuild |
| SPARK Prover | GNATprove (optional, for formal verification) |
| Test Coverage | GNATcoverage (optional) |

## 6. Verification and Validation

### 6.1 Test Coverage

| Suite | Tests | Coverage Target |
|-------|-------|-----------------|
| Unit Tests | 227 | 90%+ stmt+decision |
| Result | 84 | 95%+ |
| Option | 65 | 95%+ |
| Either | 58 | 95%+ |
| Try | 14 | 95%+ |
| Try_Option | 6 | 95%+ |

### 6.2 Verification Methods

| Requirement | Method |
|-------------|--------|
| FR-01 to FR-05 | Unit tests exercising all operations |
| NFR-01 | Code review for Inline aspects |
| NFR-02 | Tests verify no exceptions from core operations |
| NFR-03 | Build on multiple targets (POSIX, Windows) |
| NFR-04 | GNATcoverage analysis |
| NFR-05 | API review against Rust documentation |
| NFR-06 | GNATprove analysis (optional) |

## 7. Appendices

### 7.1 API Summary

| Package | Operations |
|---------|------------|
| Functional.Result | 36 operations |
| Functional.Option | 26 operations |
| Functional.Either | 20 operations |
| Functional.Try | 5 functions |
| **Total** | **87 operations** |

### 7.2 Project Statistics

- Source Files: 13
- Test Files: 6
- Total Tests: 227
- Code Coverage: 95% (stmt+decision)
- SPARK Packages: 3 (Option, Result, Either)

### 7.3 v3.0.0 Breaking Changes Summary

| Change | Old (2.x) | New (3.0.0) |
|--------|-----------|-------------|
| Option discriminant | `Kind : Kind_Type` | `Has_Value : Boolean` |
| Result discriminant | `Kind : Kind_Type` | `Is_Ok : Boolean` |
| Either discriminant | `Kind : Kind_Type` | `Is_Left : Boolean` |
| Result constructor | `Err(e)` | `New_Error(e)` |
| Result predicate | `Is_Err(r)` | `Is_Error(r)` |
| Result transform | `Map_Err(r)` | `Map_Error(r)` |
| Result field | `Err_Value` | `Error_Value` |
