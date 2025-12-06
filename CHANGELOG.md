# Changelog

**Version:** 3.0.0
**Date:** December 06, 2025
**SPDX-License-Identifier:** BSD-3-Clause<br>
**License File:** See the LICENSE file in the project root.<br>
**Copyright:** © 2025 Michael Gardner, A Bit of Help, Inc.<br>
**Status:** Released

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [3.0.0] - 2025-12-06

### ⚠️ BREAKING CHANGES
- **Boolean discriminants replace enumeration discriminants**
  - Option: `Kind` → `Has_Value` (Boolean)
  - Result: `Kind` → `Is_Ok` (Boolean), `Err_Value` → `Error_Value`
  - Either: `Kind` → `Is_Left` (Boolean)
- **Result API renaming for clarity**
  - `Err` → `New_Error` (constructor)
  - `Is_Err` → `Is_Error` (predicate)
  - `Map_Err` → `Map_Error` (transform)
- **Try module generic formal renamed**: `Err` → `New_Error`

### Added

**Result (25 operations, +5 new)**
- `Zip_With` - Combine two Results with a function
- `Flatten` - Unwrap nested Result[Result[T,E],E] → Result[T,E]
- `To_Option` - Convert Ok(v) → Some(v), Error(_) → None
- `"or"` operator for `Unwrap_Or` - `R or Default` syntax
- `"or"` operator for `Fallback` - `A or B` syntax

**Option (19 operations, +8 new)**
- `"and"` operator - Returns second when both have values
- `"xor"` operator - Returns one when exactly one has value
- `Zip_With` - Combine two Options with a function
- `Flatten` - Unwrap nested Option[Option[T]] → Option[T]
- `Ok_Or` - Convert Some(v) → Ok(v), None → Error(e) (eager)
- `Ok_Or_Else` - Convert Some(v) → Ok(v), None → Error(f()) (lazy)
- `"or"` operator for `Unwrap_Or` - `O or Default` syntax
- `"or"` operator for `Or_Else` - `A or B` syntax

**Either (11 operations, +3 new)**
- `Map` - Right-biased transform (convenience for common case)
- `Swap` - Exchange Left and Right values
- `And_Then` - Right-biased monadic bind for chaining

### Migration Guide (2.x → 3.0.0)

**Discriminant access:**
```ada
-- Old (2.x)
if R.Kind = K_Ok then ...
if O.Kind = K_Some then ...

-- New (3.0.0)
if R.Is_Ok then ...
if O.Has_Value then ...
```

**Result constructors/predicates:**
```ada
-- Old (2.x)
Str_Result.Err (E);
if Str_Result.Is_Err (R) then ...

-- New (3.0.0)
Str_Result.New_Error (E);
if Str_Result.Is_Error (R) then ...
```

**Map_Error:**
```ada
-- Old (2.x)
function Transform is new Str_Result.Map_Err (F => ...);

-- New (3.0.0)
function Transform is new Str_Result.Map_Error (F => ...);
```

### SPARK and Embedded Compliance (NEW)
- `SPARK_Mode => On` for Option, Result, Either (formally verifiable)
- `SPARK_Mode => Off` for Try (exception boundary, by design)
- `Preelaborate` categorization for Result, Either
- `Pure` categorization for Version
- Postconditions added to all transform operations for prover assistance
- Zero heap allocation, no controlled types, Ravenscar compatible

### Technical Details
- All 135 unit tests passing (Result: 52, Option: 40, Either: 23, Try: 14, Try_Option: 6)
- Tested on POSIX (macOS, Linux) and Windows platforms
- stmt+decision coverage: 95%+
- Total operations: 55 (Result: 25, Option: 19, Either: 11)

---

## [2.3.0] - 2025-12-05

### Added

- **Expanded test coverage**: Added tests for previously uncovered functions
  - `Result.From_Error` (boundary constructor - 3 tests)
  - `Result.And_Then_Into` (type-changing monadic chain - 5 tests)
  - `Either.Map_Left` (both Left and Right input paths)
  - `Either.Map_Right` (both Left and Right input paths)
  - `Try.Try_To_Result_With_Param` (success and exception paths)
  - `Try.Try_To_Option_With_Param` (success and exception paths)
  - Coverage improved: 89% → 95% (exceeds 90% target)

### Changed
- **Documentation regeneration**: Complete refresh of all documentation per documentation agent standards
  - **quick_start.md**: Regenerated with all 44 operations (Result: 20, Option: 11, Either: 10, Try: 5)
    - Added Map, Map_Err, Bimap examples for Result
    - Added Filter example for Option
    - Added Map_Left, Map_Right, Bimap, Fold examples for Either
    - Added Try_To_Option and Try_To_Option_With_Param sections
    - Added Tap/logging pattern to Common Patterns
  - **Formal docs (SRS, SDS, STG)**: Completely regenerated for functional library
    - Previous content was from TZif project (incorrect)
    - SDS now correctly describes nonhybrid utility library (not hexagonal architecture)
  - **Source file docstrings**: Updated headers with operation counts and purpose descriptions
  - **config/README.md**: Updated version to 2.3.0
- **Project cleanup**: Removed orphaned shared/ directory (release script generates to src/version/)
- **GPR cleanup**: Removed Documentation package reference (docs/api doesn't exist)

### Fixed
- **Release script library detection**: Added GPR-based project type detection
  - Checks for `Library_Name` or `Library_Kind` in GPR files
  - Correctly identifies nonhybrid utility libraries (like functional) as libraries
  - Prepares for future distinction: hybrid_app, hybrid_lib, nonhybrid_lib
- **Documentation terminology**: Updated Try.ads comment to use "Boundary adapters" instead of "Infrastructure/Presentation" to avoid false positives in release validation

### Technical Details
- All 101 unit tests passing (Result: 43, Option: 22, Either: 16, Try: 14, Try_Option: 6)
- stmt+decision coverage: 95%+
- All 20 Result operations now have tests (From_Error, And_Then_Into added)

---

## [2.2.1] - 2025-12-02

### Added
- **ROADMAP marker scanning**: Release script now detects ROADMAP markers alongside TODO/FIXME/STUB
  - ROADMAP markers indicate planned future work tracked in roadmap.md
  - Helps identify deferred features during release preparation

### Fixed
- **Library_Name casing**: Changed from `"Functional"` to `"functional"` in functional.gpr
  - Fixes GPRbuild elaboration object lookup (`p__functional_0.o` vs `p__Functional_0.o`)
  - Follows GNAT convention: library name matches lowercase project filename
  - Eliminates `ar: p__functional_0.o: not found in archive` warning in dependent projects
- **Alire deployment**: Add `export-ignore` for submodules (`scripts/python`, `test/python`, `docs/common`) to prevent path collisions with dependencies during Alire source archive creation

---

## [2.2.0] - 2025-11-30

### Added
- **Preelaborate pragma for Option**: Added `Preelaborate` categorization to `Functional.Option`
  - Allows Option to be instantiated in preelaborable packages
  - Enables use in domain/application layers with strict elaboration requirements
  - Backwards compatible: only expands where Option can be used, no breaking changes

### Technical Details
- All 83 Ada tests passing
- Zero breaking changes from 2.1.1

---

## [2.1.1] - 2025-11-19

### Added
- **Test Build Mode**: Added "test" mode to functional.gpr for compatibility with downstream projects that use `-Xmode=test`
  - Supports test-specific compiler flags (debug info, assertions, validity checks)
  - Enables symbolic tracebacks in test mode

---

## [2.1.0] - 2025-11-18

### Added
- **Parameterized Try Functions**: Enhanced exception handling with input context support
  - `Try_To_Result_With_Param` - Parameterized Result bridge for actions requiring input (e.g., console messages, file paths, user IDs)
  - `Try_To_Option_With_Param` - Parameterized Option bridge for actions with input context
  - Support for indefinite types (String, unconstrained arrays) via `type Param (<>) is private` generic formal parameter
  - Eliminates need for module-level mutable state and unsafe `Unchecked_Access` patterns
- **Backwards-Compatible Child Packages**: Restored legacy API for existing code
  - `Functional.Try.To_Result` - Child package wrapping `Try_To_Functional_Result` with `.Run` API
  - `Functional.Try.To_Option` - Child package wrapping `Try_To_Functional_Option` with `.Run` API
  - Zero breaking changes - all existing test code continues to work unchanged

### Changed
- **Enhanced Documentation**: Updated API documentation with parameterized Try usage patterns
  - Added examples showing safe parameter passing without `Unchecked_Access`
  - Documented indefinite type support (`type Param (<>) is private`)
  - Updated architecture notes explaining exception boundary conversion
- **Build Infrastructure**: Modernized build system with standardized tooling
  - Adopted Makefile from simple_hybrid with architecture validation (`check-arch`)
  - Added cleanup utilities (`clean-clutter`), Python testing (`test-python`), and tool installation targets
  - Renamed `tests/` directory to `test/` for consistency with project standards
  - Updated test infrastructure to work with relocated `scripts/makefile/arch_guard.py`
  - Professional colored test output with bordered success/failure banners

### Fixed
- **Type Safety**: Generic formal parameters now support indefinite types (String, unconstrained arrays)
- **Thread Safety**: Parameterized approach eliminates module-level mutable state in client code

### Technical Details
- All 83 Ada tests passing (Result: 35, Option: 22, Either: 12, Try: 14)
- All 35 Python tests passing (arch_guard validation suite)
- Zero unsafe code patterns required for using Try functions with parameters
- Fully backwards compatible with 2.0.0 API

---

## [2.0.0] - 2025-11-13

### ⚠️ BREAKING CHANGES
- **Try Module API Redesign**: Complete refactoring of exception handling

### Changed
- **Try Module**: Redesigned for maximum flexibility
  - Renamed `Functional.Try.To_Result` → `Functional.Try.Try_To_Result`
  - Renamed `Functional.Try.To_Option` → `Functional.Try.Try_To_Functional_Option`
  - Added `Functional.Try.Try_To_Functional_Result` (convenience for Functional.Result)
  - `Try_To_Result` now works with **any** Result type (domain-specific, custom implementations)
  - Consolidated all Try functions into single `Functional.Try` package
  - Removed child packages `Functional.Try.To_Result` and `Functional.Try.To_Option`
  - Generic parameters allow bridging to any Result implementation with custom `Ok`/`Err` constructors

### Added
- Generic `Try_To_Result` supports custom Result types with configurable constructors
- Example code showing usage at infrastructure boundaries
- Comprehensive documentation for Try pattern in README

### Migration Guide (1.0.0 → 2.0.0)

**Old Code (1.0.0):**
```ada
with Functional.Try.To_Result;
with Functional.Try.To_Option;

function Try_Read is new Functional.Try.To_Result (...);
function Try_Parse is new Functional.Try.To_Option (...);
```

**New Code (2.0.0):**
```ada
with Functional.Try;

-- For custom domain Result types:
function Try_Read is new Functional.Try.Try_To_Result
  (T => Integer_32, E => Error_Type,
   Result_Type => Domain_Result.Result,
   Ok => Domain_Result.Ok,
   Err => Domain_Result.From_Error,
   Map_Exception => From_Exception,
   Action => Raw_Read);

-- For Functional.Result:
function Try_Parse is new Functional.Try.Try_To_Functional_Result
  (T => String, E => Error,
   Result_Pkg => Str_Result,
   Map_Exception => From_Exception,
   Action => Raw_Parse);

-- For Functional.Option:
function Try_Lookup is new Functional.Try.Try_To_Functional_Option
  (T => Integer,
   Option_Pkg => Int_Option,
   Action => Raw_Lookup);
```


## [1.0.0] - 2025-10-25

### Overview
Initial stable release of the Functional library for Ada 2022, providing type-safe
functional programming abstractions including Result<T,E>, Option<T>, Either<L,R>,
and Try utilities (To_Result, To_Option) for exception handling.

### Added
- **Core Types**:
  - `Functional.Result<T,E>` - Type-safe error handling with 17 operations
  - `Functional.Option<T>` - Optional values with 11 operations
  - `Functional.Either<L,R>` - Disjoint union type with 8 operations
  - `Functional.Try.To_Result` - Convert exception-based code to Result
  - `Functional.Try.To_Option` - Convert exception-based code to Option

- **Result Operations**:
  - Constructors: `Ok`, `Err`
  - Predicates: `Is_Ok`, `Is_Err`
  - Extractors: `Value`, `Error`, `Expect` (with custom error messages)
  - Unwrapping: `Unwrap_Or`, `Unwrap_Or_With`
  - Transformations: `Map`, `And_Then`, `Map_Err`, `Bimap`
  - Recovery: `Fallback`, `Fallback_With`, `Recover`, `Recover_With`
  - Validation: `Ensure`, `With_Context`
  - Side effects: `Tap`
  - Postconditions on `Unwrap_Or` for contract verification

- **Option Operations**:
  - Constructors: `New_Some`, `None`
  - Predicates: `Is_Some`, `Is_None`
  - Extractors: `Value`
  - Unwrapping: `Unwrap_Or`, `Unwrap_Or_With`
  - Transformations: `Map`, `And_Then`, `Filter`
  - Fallback: `Or_Else`, `Or_Else_With`, `Fallback` (alias)
  - Postconditions on `Unwrap_Or` for contract verification

- **Either Operations**:
  - Constructors: `Left`, `Right`
  - Predicates: `Is_Left`, `Is_Right`
  - Extractors: `Left_Value`, `Right_Value`
  - Transformations: `Bimap` (transform both sides), `Fold` (reduce to single value)

- **Exception Handling**:
  - `Functional.Try.To_Result` - Convert exception-raising code to Result<T,E>
  - `Functional.Try.To_Option` - Convert exception-raising code to Option<T>
  - Full exception information preservation in Try.To_Result

- **Testing**:
  - 83 comprehensive unit tests achieving 100% function coverage
  - All packages tested with 90%+ code coverage target
  - Test framework with aggregated reporting

- **Code Quality**:
  - Package-level generics (Ada 2022 best practice)
  - Inline annotations for compiler optimization
  - Preconditions and postconditions for contract programming
  - Comprehensive API documentation with usage examples
  - Line length convention: 120 characters for code, 80 characters for comments
  - Comment separator lines standardized to exactly 80 characters
