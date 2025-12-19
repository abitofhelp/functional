# Functional Programming Library for Ada 2022

[![License](https://img.shields.io/badge/license-BSD--3--Clause-blue.svg)](LICENSE) [![Ada](https://img.shields.io/badge/Ada-2022-blue.svg)](https://ada-lang.io) [![SPARK](https://img.shields.io/badge/SPARK-Proved-green.svg)](https://www.adacore.com/about-spark) [![Alire](https://img.shields.io/badge/Alire-2.0+-blue.svg)](https://alire.ada.dev)

**Version:** 4.1.0<br>
**Date:** 2025-12-18<br>
**SPDX-License-Identifier:** BSD-3-Clause<br>
**License File:** See the LICENSE file in the project root<br>
**Copyright:** © 2025 Michael Gardner, A Bit of Help, Inc.<br>
**Status:** Released

## Overview

A production-ready Ada 2022 library providing functional programming abstractions for type-safe computation. Implements `Result<T,E>`, `Option<T>`, `Either<L,R>` monadic types, and `Scoped` RAII guards with 80+ composable operations enabling railway-oriented programming, explicit error handling, optional value management, and automatic resource cleanup.

Designed for safety-critical, embedded, and high-assurance applications with full SPARK compatibility.

## Features

### Core Types (80+ operations)

| Type | Operations | Purpose |
|------|------------|---------|
| **Result[T,E]** | 30 | Success/failure with typed errors |
| **Option[T]** | 25 | Optional values (presence/absence) |
| **Either[L,R]** | 20 | Disjoint unions (one of two types) |
| **Try** | 6 + child packages | Exception-to-functional bridges |
| **Scoped** | 2 | RAII resource guards |

### Embedded Systems Ready

- **Preelaborate/Pure** categorization on all packages
- **Zero heap allocation** - Stack-based discriminated records only
- **No controlled types in core** - No finalization overhead
- **No tasking dependencies** - Ravenscar profile compatible
- **Bounded memory** - Fixed-size structures
- **No OS dependencies** - Pure Ada 2022

### Production Quality

- **Comprehensive test coverage** (90%+ code coverage)
- **Zero dependencies** - Ada 2022 standard library only
- **Comprehensive contracts** - Pre/Post conditions throughout
- **Cross-platform** - POSIX and Windows tested

## SPARK Formal Verification

<table>
<tr>
<td width="120"><strong>Status</strong></td>
<td><img src="https://img.shields.io/badge/SPARK-Proved-green.svg" alt="SPARK Proved"></td>
</tr>
<tr>
<td><strong>Scope</strong></td>
<td>All packages (Option, Result, Either, Version) + comprehensive instantiation tests</td>
</tr>
<tr>
<td><strong>Mode</strong></td>
<td>gnatprove --mode=prove --level=2 (full proof)</td>
</tr>
<tr>
<td><strong>Results</strong></td>
<td>See <a href="CHANGELOG.md">CHANGELOG</a> for current proof statistics</td>
</tr>
</table>

The entire library is formally verified using SPARK Ada, providing mathematical guarantees of:

- **No runtime errors** - Division by zero, overflow, range violations
- **No uninitialized data** - All variables properly initialized before use
- **Contract compliance** - Pre/postconditions proven correct
- **Data flow integrity** - No aliasing or information flow violations

### Verification Commands

```bash
make spark-check    # Run SPARK legality verification
make spark-prove    # Run full SPARK proof verification
```

### SPARK Coverage

| Package | SPARK_Mode | Description |
|---------|-----------|-------------|
| `Functional.Result` | On | Result[T,E] monad (30 operations) |
| `Functional.Option` | On | Option[T] monad (25 operations) |
| `Functional.Either` | On | Either[L,R] type (20 operations) |
| `Functional.Version` | On | Version information |
| `Functional.Try` | Off | Exception boundary (by design) |
| `Functional.Scoped` | Off | RAII guards (requires finalization) |

The `Try` and `Scoped` modules use `SPARK_Mode => Off` because they interact with exception handling and finalization respectively.

### SPARK Proof Coverage

Since SPARK only analyzes instantiated generics (not generic templates), the library includes a comprehensive SPARK test suite (`test/spark/`) that instantiates all generic operations, providing:

- **Full operation coverage**: All Map, And_Then, Filter, Zip, Flatten, Fold operations instantiated
- **Helper function verification**: All predicates and transformers formally proven overflow-safe
- **High proof rate**: See [CHANGELOG](CHANGELOG.md) for current statistics

## Quick Start

### Installation

Add to your `alire.toml`:

```toml
[[depends-on]]
functional = "^4.1.0"
```

Then build:

```bash
alr build
```

### Clone with Submodules

```bash
git clone --recurse-submodules https://github.com/abitofhelp/functional.git

# Or if already cloned:
git submodule update --init --recursive
```

## Usage

### Result\<T,E\> - Success or Failure

```ada
with Functional.Result;

package Int_Result is new Functional.Result (T => Integer, E => Error);

--  Create results
R := Int_Result.Ok (42);                    --  Success
R := Int_Result.New_Error (Parse_Failed);   --  Failure

--  Check and extract
if Int_Result.Is_Ok (R) then
   Process (Int_Result.Value (R));
end if;

--  With default (or use operator syntax: R or 0)
Val := Int_Result.Unwrap_Or (R, 0);

--  Transform (railway-oriented)
function Double is new Int_Result.Map (F => Times_Two);
R := Double (R);  --  Ok(42) -> Ok(84), Error stays Error
```

See how the Result monad is used in production:
  - [zoneinfo: Domain.Error.Result](https://github.com/abitofhelp/zoneinfo_ada/tree/main/src/domain/error)
  - [tzif: Domain.Error.Result](https://github.com/abitofhelp/tzif_ada/tree/main/src/domain/error)

### Option\<T\> - Presence or Absence

```ada
with Functional.Option;

package Int_Option is new Functional.Option (T => Integer);

--  Create options
O := Int_Option.New_Some (42);   --  Has value
O := Int_Option.None;            --  No value

--  With default (or use operator syntax: O or 0)
Age := Int_Option.Unwrap_Or (O, 0);

--  Chain operations
function Parse is new Int_Option.And_Then (F => Try_Parse);
O := Parse (Input);  --  Some -> parse it, None -> stays None
```

See how the Option monad is used in production:
  - [zoneinfo: Domain.Types.Option](https://github.com/abitofhelp/zoneinfo_ada/tree/main/src/domain/types)
  - [tzif: Domain.Types.Option](https://github.com/abitofhelp/tzif_ada/tree/main/src/domain/types)

### Either\<L,R\> - One of Two Types

```ada
with Functional.Either;

package Str_Int is new Functional.Either (L => String, R => Integer);

--  Create either
E := Str_Int.Left ("text");    --  Left variant
E := Str_Int.Right (42);       --  Right variant

--  Fold to single value
Result := Str_Int.Fold (E, On_String'Access, On_Integer'Access);
```

### Try - Exception Boundaries

Use `Map_To_Result_With_Param` (recommended) for declarative exception mapping:

```ada
with Functional.Try.Map_To_Result_With_Param;

package Try_Read is new Functional.Try.Map_To_Result_With_Param
  (Error_Kind_Type    => Error_Kind,
   Param_Type         => String,
   Result_Type        => IO_Result.Result,
   Make_Error         => Make_Error,
   Default_Error_Kind => Internal_Error,
   Action             => Read_File_Raw);

Mappings : constant Try_Read.Mapping_Array :=
  [(Name_Error'Identity, Not_Found),
   (Use_Error'Identity,  Permission_Error)];

Result := Try_Read.Run (File_Path, Mappings);
```

Use `Try_To_Option_With_Param` for probe operations:

```ada
--  Returns None on any exception, then use Unwrap_Or
Exists := Bool_Option.Unwrap_Or (Try_Is_TZif (Path), False);
```

## API Reference

### Result<T,E> (30 operations)

| Category | Operations |
|----------|------------|
| **Constructors** | `Ok`, `New_Error`, `From_Error` |
| **Predicates** | `Is_Ok`, `Is_Error`, `Is_Ok_And`, `Is_Error_And`, `Is_Ok_Or`, `Is_Error_Or`, `Contains` |
| **Extractors** | `Value`, `Error` |
| **Defaults** | `Unwrap_Or`, `Unwrap_Or_With` |
| **Transforms** | `Map`, `Map_Or`, `Map_Or_Else`, `And_Then`, `And_Then_Into`, `Map_Error`, `Bimap`, `Zip_With`, `Flatten`, `To_Option` |
| **Recovery** | `Fallback`, `Fallback_With`, `Recover`, `Recover_With` |
| **Validation** | `Ensure`, `With_Context` |
| **Side Effects** | `Tap`, `Tap_Ok`, `Tap_Error` |
| **Operators** | `"or"` (Unwrap_Or, Fallback), `"="` (Contains) |

### Option<T> (25 operations)

| Category | Operations |
|----------|------------|
| **Constructors** | `New_Some`, `None` |
| **Predicates** | `Is_Some`, `Is_None`, `Is_Some_And`, `Is_None_Or`, `Contains` |
| **Extractors** | `Value` |
| **Defaults** | `Unwrap_Or`, `Unwrap_Or_With` |
| **Transforms** | `Map`, `Map_Or`, `Map_Or_Else`, `And_Then`, `Filter`, `Zip_With`, `Flatten` |
| **Fallback** | `Or_Else`, `Or_Else_With`, `Fallback` |
| **Side Effects** | `Tap` |
| **Conversion** | `Ok_Or`, `Ok_Or_Else` |
| **Operators** | `"or"`, `"and"`, `"xor"`, `"="` |

### Either<L,R> (20 operations)

| Category | Operations |
|----------|------------|
| **Constructors** | `Left`, `Right` |
| **Predicates** | `Is_Left`, `Is_Right`, `Is_Left_And`, `Is_Right_And`, `Is_Left_Or`, `Is_Right_Or`, `Contains` |
| **Extractors** | `Left_Value`, `Right_Value`, `Get_Or_Else` |
| **Transforms** | `Map`, `Map_Left`, `Map_Right`, `Bimap`, `Swap`, `And_Then` |
| **Reduction** | `Fold`, `Merge` |
| **Conversion** | `To_Option`, `To_Result` |
| **Operators** | `"="` (Contains) |

### Try Module (6 functions + child packages)

| Function | Purpose |
|----------|---------|
| `Try_To_Functional_Option` | Convenience for Functional.Option |
| `Try_To_Option_With_Param` | Parameterized Option bridge |
| `Map_To_Result` | Declarative exception mapping tables |
| `Map_To_Result_With_Param` | Parameterized declarative mapping |
| `Try_To_Result` | General bridge (deprecated) |
| `Try_To_Functional_Result` | Convenience for Functional.Result (deprecated) |

### Scoped Module (2 generic packages)

| Package | Purpose |
|---------|---------|
| `Guard_For` | Unconditional RAII guard for automatic cleanup |
| `Conditional_Guard_For` | Conditional RAII guard with Should_Release check |

## Testing

```bash
# Run all tests
make test-all

# Run unit tests only
make test-unit

# Run with coverage
make test-coverage
```

**Results:** All tests passing. See [CHANGELOG](CHANGELOG.md) for current counts.

## Documentation

- **[Cheatsheet](docs/guides/cheatsheet.md)** - All operations at a glance
- **[User Guide](docs/guides/user_guide.md)** - Design philosophy, SPARK, embedded, best practices
- **[Quick Start](docs/quick_start.md)** - Get started in minutes
- **[SRS](docs/formal/software_requirements_specification.md)** - Requirements specification
- **[SDS](docs/formal/software_design_specification.md)** - Design specification
- **[STG](docs/formal/software_test_guide.md)** - Test guide
- **[CHANGELOG](CHANGELOG.md)** - Release history

## Submodule Management

```bash
make submodule-init      # After fresh clone
make submodule-update    # Pull latest
make submodule-status    # Check commits
```

## Code Standards

This project follows:
- **Ada Agent** (`~/.claude/agents/ada.md`) - Ada 2022 standards
- **Functional Agent** (`~/.claude/agents/functional.md`) - Result/Option/Either patterns

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
[github.com/abitofhelp](https://github.com/abitofhelp)

## Project Status

**Status**: Production Ready (v4.1.0)

- [x] Result[T,E] - 30 operations
- [x] Option[T] - 25 operations
- [x] Either[L,R] - 20 operations
- [x] Try module - 6 functions + child packages
- [x] Scoped module - 2 RAII guard packages
- [x] SPARK compatible (see CHANGELOG for proof stats)
- [x] Comprehensive test coverage (see CHANGELOG)
- [x] Zero external dependencies
- [x] Cross-platform (POSIX, Windows)
