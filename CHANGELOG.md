# Changelog

**Version:** 4.1.0
**Date:** December 18, 2025
**SPDX-License-Identifier:** BSD-3-Clause<br>
**License File:** See the LICENSE file in the project root.<br>
**Copyright:** © 2025 Michael Gardner, A Bit of Help, Inc.<br>
**Status:** Released

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

---

## [4.1.0] - 2025-12-18

### Deprecated

#### Result-based Try functions (procedural Map_Exception pattern)
- `Try_To_Result` - Use `Functional.Try.Map_To_Result`
- `Try_To_Any_Result_With_Param` - Use `Functional.Try.Map_To_Result_With_Param`
- `Try_To_Functional_Result` - Use `Functional.Try.Map_To_Result`
- `Try_To_Result_With_Param` - Use `Functional.Try.Map_To_Result_With_Param`

**Rationale:** Declarative exception mappings (data) are clearer and more
maintainable than procedural `Map_Exception` functions (code). The Map
versions use a mapping array that is self-documenting and allows multiple
exception-to-error-kind discriminations without if/elsif chains.

**Note:** Option-based Try functions (`Try_To_Functional_Option`,
`Try_To_Option_With_Param`) are NOT deprecated. They serve valid use cases
for "probe" operations with sensible defaults where error details don't matter.

### Changed

- `Functional.Try.To_Result` - Reimplemented with inline exception handling
  instead of delegating to deprecated `Try_To_Functional_Result`. This eliminates
  deprecation warnings when building the functional library itself.
- `Functional.Try.To_Option` - Reimplemented with inline exception handling
  instead of delegating to `Try_To_Functional_Option` (consistency).
- Added "When to Use Option" documentation section to `functional-try.ads`

### Fixed

- Suppress expected warnings in test suite:
  - `test_try.adb`: obsolescent warnings (testing deprecated functions still work)
  - `test_scoped.adb`: "condition is always True" (intentional test pattern)
- Added `*__GNATPP-TEMP` to `.gitignore` (GNAT Pretty Print temp files)

---

## [4.0.0] - 2025-12-12

**Test Coverage:** 269 unit + 0 integration + 0 examples = 269 total

### BREAKING CHANGES
- **Removed exception-raising functions from Result and Option**
  - `Expect` removed from Result (was: extract value or raise Program_Error)
  - `Expect_Error` removed from Result (was: extract error or raise Program_Error)
  - `Unwrap_Error` removed from Result (was: extract error with precondition)
  - `Expect` removed from Option (was: extract value or raise Program_Error)

**Rationale:**
1. **Functional programming principle**: Errors should be values, not exceptions.
   Exception-based control flow violates the core tenet of functional error handling.
2. **SPARK compatibility**: SPARK formal verification prohibits exceptions.
   These functions were incompatible with SPARK_Mode => On despite the package annotation.
3. **Architectural consistency**: The functional crate should model pure functional
   patterns without exception escape hatches.

**Migration:**
```ada
-- Old (3.0.0): Uses exception
Val := Int_Result.Expect (R, "Expected Ok");

-- New: Use precondition + Value (compile-time safety)
pragma Assert (Int_Result.Is_Ok (R));
Val := Int_Result.Value (R);

-- Or: Use Unwrap_Or for default fallback
Val := Int_Result.Unwrap_Or (R, Default_Value);
```

### Added

**New Exception Mapping Packages**
- `Functional.Try.Map_To_Result` - Declarative exception-to-Result mapping with
  configurable exception→error kind tables. Supports empty mappings (catch-all)
  or specific exception discrimination.
- `Functional.Try.Map_To_Result_With_Param` - Parameterized version for actions
  requiring input context (file paths, user IDs, etc.). Supports indefinite types.

**New RAII Resource Management**
- `Functional.Scoped.Guard_For` - Unconditional RAII guard for automatic resource
  cleanup. Calls Release procedure when guard goes out of scope.
- `Functional.Scoped.Conditional_Guard_For` - Conditional RAII guard that checks
  Should_Release predicate before calling Release.

**New Tests**
- 11 tests for `Functional.Scoped` covering normal exit, exception paths,
  multiple guards, conditional release, and exception-safe Finalize behavior.
- 35 tests for `Map_To_Result` and `Map_To_Result_With_Param` covering
  success paths, mapped exceptions, unmapped exceptions, and catch-all behavior.

### Fixed
- **Hardened Finalize in Scoped** - `Finalize` procedures now catch all exceptions
  to prevent exception propagation during stack unwinding (which causes
  Program_Error or termination).
- **Keyword casing** - Fixed `in Out` to `in out` in generic formal parameters.
- **Comment clarity** - Fixed misleading "order doesn't matter" comment in
  Map_To_Result; now correctly states "first match wins if duplicates exist".

### Changed
- **Operation counts:**
  - Result: 33 operations (was 36, removed Expect/Expect_Error/Unwrap_Error)
  - Option: 25 operations (was 26, removed Expect)
  - Try: 7 operations (was 5, added Map_To_Result variants)
  - Scoped: 2 generic packages (new)

### Technical Details
- All 269 unit tests passing
- SPARK proved (Option, Result, Either, Version)
- **SPARK proof coverage**: 402 checks, 401 proved (99.7%), 270 subprograms analyzed
  - Comprehensive instantiation tests (`test/spark/`) exercise all generic operations
  - All helper functions proven overflow-safe
- Scoped uses SPARK_Mode => Off (requires Ada.Finalization)
- Zero heap allocation maintained

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

**Documentation**
- `docs/guides/cheatsheet.md` - All types and operators on 1-2 pages
- `docs/guides/user_guide.md` - Design philosophy, SPARK, embedded, best practices

**Result (30 operations, +13 new)**
- `Zip_With` - Combine two Results with a function
- `Flatten` - Unwrap nested Result[Result[T,E],E] → Result[T,E]
- `To_Option` - Convert Ok(v) → Some(v), Error(_) → None
- `Contains` - Check if Ok value equals given value
- `Is_Ok_And` - Test if Ok and predicate holds (strict)
- `Is_Error_And` - Test if Error and predicate holds (strict)
- `Is_Ok_Or` - Test if Error or predicate holds on Ok (lenient)
- `Is_Error_Or` - Test if Ok or predicate holds on Error (lenient)
- `Map_Or` - Transform Ok value or return default (eager)
- `Map_Or_Else` - Transform Ok value or call default producer (lazy)
- `Tap_Ok` - Side effect on Ok only
- `Tap_Error` - Side effect on Error only
- `"or"` operator for `Unwrap_Or` - `R or Default` syntax
- `"or"` operator for `Fallback` - `A or B` syntax
- `"="` operator for `Contains` - `R = V` syntax

**Option (25 operations, +14 new)**
- `"and"` operator - Returns second when both have values
- `"xor"` operator - Returns one when exactly one has value
- `Zip_With` - Combine two Options with a function
- `Flatten` - Unwrap nested Option[Option[T]] → Option[T]
- `Ok_Or` - Convert Some(v) → Ok(v), None → Error(e) (eager)
- `Ok_Or_Else` - Convert Some(v) → Ok(v), None → Error(f()) (lazy)
- `Contains` - Check if Some value equals given value
- `Is_Some_And` - Test if Some and predicate holds (strict)
- `Is_None_Or` - Test if None or predicate holds on Some (lenient)
- `Map_Or` - Transform Some value or return default (eager)
- `Map_Or_Else` - Transform Some value or call default producer (lazy)
- `Tap` - Side effect on Some value
- `"or"` operator for `Unwrap_Or` - `O or Default` syntax
- `"or"` operator for `Or_Else` - `A or B` syntax
- `"="` operator for `Contains` - `O = V` syntax

**Either (20 operations, +12 new)**
- `Map` - Right-biased transform (convenience for common case)
- `Swap` - Exchange Left and Right values
- `And_Then` - Right-biased monadic bind for chaining
- `Contains` - Check if Right value equals given value
- `Is_Left_And` - Test if Left and predicate holds (strict)
- `Is_Right_And` - Test if Right and predicate holds (strict)
- `Is_Left_Or` - Test if Right or predicate holds on Left (lenient)
- `Is_Right_Or` - Test if Left or predicate holds on Right (lenient)
- `Get_Or_Else` - Get Right value or default
- `Merge` - Extract value when both types are the same
- `To_Option` - Convert Right(v) → Some(v), Left(_) → None
- `To_Result` - Convert Right(v) → Ok(v), Left(e) → Error(e)
- `"="` operator for `Contains` - `E = V` syntax

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
- All unit tests passing (Result, Option, Either, Try, Try_Option)
- Tested on POSIX (macOS, Linux) and Windows platforms
- stmt+decision coverage: 95%+
- Total operations: 80 (Result: 30, Option: 25, Either: 20, Try: 5)

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
