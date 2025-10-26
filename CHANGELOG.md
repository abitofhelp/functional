# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

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
