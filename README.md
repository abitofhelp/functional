# Type-Safe Functional Error Handling

[![License](https://img.shields.io/badge/license-BSD--3--Clause-blue.svg)](LICENSE) [![Ada](https://img.shields.io/badge/Ada-2022-blue.svg)](https://ada-lang.io) [![SPARK](https://img.shields.io/badge/SPARK-Friendly-green.svg)](https://www.adacore.com/about-spark) [![Alire](https://img.shields.io/badge/Alire-2.0+-blue.svg)](https://alire.ada.dev)

**Version:** 2.3.0<br>
**Date:** December 05, 2025<br>
**SPDX-License-Identifier:** BSD-3-Clause<br>
**License File:** See the LICENSE file in the project root<br>
**Copyright:** © 2025 Michael Gardner, A Bit of Help, Inc.<br>
**Status:** Released

## Overview

A clean, Ada-idiomatic library providing `Result<T,E>`, `Option<T>`, and `Either<L,R>` types for functional error handling in Ada 2022. Enables railway-oriented programming with composable operations like Map, And_Then, and Recover.

## Features

- **Result<T,E>** - Type-safe error handling (20 operations)
- **Option<T>** - Optional values (11 operations)
- **Either<L,R>** - Disjoint union type (8 operations)
- **Try Module** - Convert exceptions to functional types (5 functions)
  - `Try_To_Result` - General bridge for any Result type
  - `Try_To_Functional_Result` - Convenience for Functional.Result
  - `Try_To_Functional_Option` - Convenience for Functional.Option
  - `Try_To_Result_With_Param` - Parameterized Result bridge
  - `Try_To_Option_With_Param` - Parameterized Option bridge
- **Pure packages** - No side effects, compile-time guarantees
- **Zero dependencies** - Just Ada 2022 standard library
- **Production ready** - Comprehensive compiler checks and style enforcement

## Platform Support

| Platform | Status | Notes |
|----------|--------|-------|
| **Desktop** | Full | Standard Ada runtime |
| **Embedded** | Full | Pure packages, no heap allocation, no I/O |

## Quick Start

### Clone with Submodules

This repository uses git submodules for shared tooling:

```bash
git clone --recurse-submodules https://github.com/abitofhelp/functional.git
```

Or if already cloned without submodules:

```bash
git submodule update --init --recursive
# Or: make submodule-init
```

### Installation

Add to your `alire.toml`:

```toml
[[depends-on]]
functional = "^2.0.0"
```

Then build:

```bash
alr build
```

## Usage

### Result<T,E> - For Error Handling

```ada
with Functional.Result;

--  Instantiate for your types
package Str_Result is new Functional.Result (T => String, E => Error);

--  Create results
return Str_Result.Ok (Value);           --  Success
return Str_Result.Err (Error_Info);     --  Failure

--  Check and extract
if Str_Result.Is_Ok (R) then
   Put_Line (Str_Result.Value (R));
else
   Handle (Str_Result.Error (R));
end if;

--  Get with default
Val : constant String := Str_Result.Unwrap_Or (R, "default");
```

### Option<T> - For Optional Values

```ada
with Functional.Option;

package Int_Option is new Functional.Option (T => Integer);

--  Create options
return Int_Option.New_Some (42);        --  Has value
return Int_Option.None;                 --  No value

--  Get with default
Age : constant Integer := Int_Option.Unwrap_Or (Opt, 0);
```

### Either<L,R> - For Neutral Choices

```ada
with Functional.Either;

package Str_Int is new Functional.Either (L => String, R => Integer);

--  Create either
return Str_Int.Left ("text");           --  Left variant
return Str_Int.Right (42);              --  Right variant
```

### Try Module - Exception Boundaries

```ada
with Functional.Try;

--  Bridge exception-based code to Result types
function Try_Read is new Functional.Try.Try_To_Result
  (T => Integer_32, E => Error_Type, Result_Type => Int32_Result.Result,
   Ok => Int32_Result.Ok, Err => Int32_Result.From_Error,
   Map_Exception => From_Exception, Action => Raw_Read);
```

## Testing

```bash
# Run the test suite
make test

# Or directly
alr run test_runner
```

## Documentation

- **[Quick Start Guide](docs/quick_start.md)** - Get started in minutes
- **[CHANGELOG](CHANGELOG.md)** - Release history

### API Reference

**Result<T,E>** (20 operations):

| Category | Operations | Purpose |
|----------|------------|---------|
| **Construct** | `Ok(v)`, `Err(e)`, `From_Error(e)` | Create success or error result |
| **Predicates** | `Is_Ok(r)`, `Is_Err(r)` | Test result state |
| **Extract** | `Value(r)`, `Error(r)`, `Expect(r, msg)` | Get value (with Pre) or panic with message |
| **Defaults** | `Unwrap_Or(r, default)`, `Unwrap_Or_With(r)` | Get value or fallback (eager/lazy) |
| **Transform** | `Map(r)`, `And_Then(r)`, `And_Then_Into(r)` | Transform Ok value, chain operations |
| **Error Map** | `Map_Err(r)`, `Bimap(r)` | Transform error, transform both sides |
| **Fallback** | `Fallback(a, b)`, `Fallback_With(r)` | Try alternative on error (eager/lazy) |
| **Recovery** | `Recover(r)`, `Recover_With(r)` | Convert error to value or new Result |
| **Validation** | `Ensure(r)`, `With_Context(r, msg)` | Validate predicate, add error breadcrumbs |
| **Side Effects** | `Tap(r)` | Run callbacks without changing Result |

**Option<T>** (11 operations):

| Category | Operations | Purpose |
|----------|------------|---------|
| **Construct** | `New_Some(v)`, `None` | Create present or absent value |
| **Predicates** | `Is_Some(o)`, `Is_None(o)` | Test presence |
| **Extract** | `Value(o)` | Get value (with Pre) |
| **Defaults** | `Unwrap_Or(o, default)`, `Unwrap_Or_With(o)` | Get value or fallback (eager/lazy) |
| **Transform** | `Map(o)`, `And_Then(o)` | Transform Some value, chain operations |
| **Filter** | `Filter(o)` | Keep value only if predicate holds |
| **Fallback** | `Or_Else(a, b)`, `Or_Else_With(o)`, `Fallback` | Try alternative on None (eager/lazy) |

**Either<L,R>** (8 operations):

| Category | Operations | Purpose |
|----------|------------|---------|
| **Construct** | `Left(v)`, `Right(v)` | Create left or right value |
| **Predicates** | `Is_Left(e)`, `Is_Right(e)` | Test which side |
| **Extract** | `Left_Value(e)`, `Right_Value(e)` | Get value (with Pre) |
| **Transform** | `Map_Left(e)`, `Map_Right(e)`, `Bimap(e)` | Transform one or both sides |
| **Reduce** | `Fold(e)` | Reduce to single value |

**Try Module** (5 functions):

| Function | Purpose |
|----------|---------|
| `Try_To_Result` | General bridge for any Result type |
| `Try_To_Functional_Result` | Convenience for Functional.Result |
| `Try_To_Functional_Option` | Convenience for Functional.Option |
| `Try_To_Result_With_Param` | Parameterized Result bridge |
| `Try_To_Option_With_Param` | Parameterized Option bridge |

## Code Standards

This project follows:
- **Ada Agent** (`~/.claude/agents/ada.md`)
- **Functional Agent** (`~/.claude/agents/functional.md`)

## Submodule Management

This project uses git submodules for shared Python tooling:

- `scripts/python` - Build, release, and architecture scripts
- `test/python` - Shared test fixtures and configuration

### Commands

```bash
# After fresh clone
make submodule-init

# Pull latest from submodule repos
make submodule-update

# Check current submodule commits
make submodule-status
```

## Contributing

This project is not open to external contributions at this time.

## AI Assistance & Authorship

This project — including its source code, tests, documentation, and other deliverables — is designed, implemented, and maintained by human developers, with Michael Gardner as the Principal Software Engineer and project lead.

We use AI coding assistants (such as OpenAI GPT models and Anthropic Claude Code) as part of the development workflow to help with:

- drafting and refactoring code and tests,
- exploring design and implementation alternatives,
- generating or refining documentation and examples,
- and performing tedious and error-prone chores.

AI systems are treated as tools, not authors. All changes are reviewed, adapted, and integrated by the human maintainers, who remain fully responsible for the architecture, correctness, and licensing of this project.

## License

Copyright © 2025 Michael Gardner, A Bit of Help, Inc.

Licensed under the BSD-3-Clause License. See [LICENSE](LICENSE) for details.

## Author

Michael Gardner
A Bit of Help, Inc.
https://github.com/abitofhelp

## Project Status

**Status**: Production Ready (v2.3.0)

- Result<T,E> with 20 operations
- Option<T> with 11 operations
- Either<L,R> with 8 operations
- Try module with 5 exception bridges
- Pure packages (no side effects)
- Zero external dependencies
- Comprehensive test suite
- Alire publication
