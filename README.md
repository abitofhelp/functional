# Type-Safe Functional Error Handling

[![License](https://img.shields.io/badge/license-BSD--3--Clause-blue.svg)](LICENSE) [![Ada](https://img.shields.io/badge/Ada-2022-blue.svg)](https://ada-lang.io) [![SPARK](https://img.shields.io/badge/SPARK-Friendly-green.svg)](https://www.adacore.com/about-spark) [![Alire](https://img.shields.io/badge/Alire-2.0+-blue.svg)](https://alire.ada.dev)

**Version:** 2.2.0  
**Date:** November 30, 2025  
**SPDX-License-Identifier:** BSD-3-Clause<br>
**License File:** See the LICENSE file in the project root<br>
**Copyright:** © 2025 Michael Gardner, A Bit of Help, Inc.<br>  
**Status:** Released  

## Overview

A clean, Ada-idiomatic library providing `Result<T,E>`, `Option<T>`, and `Either<L,R>` types for functional error handling in Ada 2022.

## Features

- ✅ **Result<T,E>** - Type-safe error handling (17 operations)
- ✅ **Option<T>** - Optional values (11 operations)
- ✅ **Either<L,R>** - Disjoint union type (8 operations)
- ✅ **Try Module** - Convert exceptions to functional types
  - `Try_To_Result` - General bridge for any Result type
  - `Try_To_Functional_Result` - Convenience for Functional.Result
  - `Try_To_Functional_Option` - Convenience for Functional.Option
- ✅ **Pure packages** - No side effects, compile-time guarantees
- ✅ **Zero dependencies** - Just Ada 2022 standard library
- ✅ **Production ready** - Comprehensive compiler checks and style enforcement

## Platform Support

| Platform | Status | Notes |
|----------|--------|-------|
| **Desktop** | ✅ Full | Standard Ada runtime |
| **Embedded** | ✅ Full | Pure packages, no heap allocation, no I/O |

## Quick Start

### Installation

Add to your `alire.toml`:

```toml
[[depends-on]]
functional = "^2.0.0"
```

Then run:
```bash
alr build
```

### Building

```bash
# Build the library
alr build

# Run tests
make test
```

## Quick Snippets

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

### API Reference

**Result<T,E>:**

| Function | Purpose |
|----------|---------|
| `Ok(v)`, `Err(e)` | Constructors |
| `Is_Ok`, `Is_Err` | Check state |
| `Value`, `Error` | Extract values |
| `Unwrap_Or` | Get value or default |
| `And_Then` | Chain operations |
| `Fallback` | Try alternative on error |
| `Recover` | Turn error into value |
| `Ensure` | Validate with predicate |
| `With_Context` | Add error breadcrumbs |

**Option<T>:**

| Function | Purpose |
|----------|---------|
| `New_Some(v)`, `None` | Constructors |
| `Is_Some`, `Is_None` | Check presence |
| `Value` | Extract value |
| `Unwrap_Or` | Get value or default |
| `And_Then` | Chain operations |
| `Filter` | Keep if predicate holds |
| `Or_Else` | Try alternative |

**Either<L,R>:**

| Function | Purpose |
|----------|---------|
| `Left(v)`, `Right(v)` | Constructors |
| `Is_Left`, `Is_Right` | Check side |
| `Left_Value`, `Right_Value` | Extract values |

## Code Standards

This project follows:
- **Ada Agent** (`~/.claude/agents/ada.md`)
- **Functional Agent** (`~/.claude/agents/functional.md`)

## Submodule Management

This project uses git submodules for shared Python tooling:

- `scripts/python` - Build, release, and architecture scripts
- `test/python` - Shared test fixtures and configuration

### Workflow

```
hybrid_python_scripts (source repo)
         │
         │ git push (manual)
         ▼
      GitHub
         │
         │ make submodule-update (in each consuming repo)
         ▼
┌─────────────────────────────────┐
│  1. Pull new submodule commit   │
│  2. Stage reference change      │
│  3. Commit locally              │
│  4. Push to remote              │
└─────────────────────────────────┘
```

### Commands

```bash
# After fresh clone
make submodule-init

# Pull latest from submodule repos
make submodule-update

# Check current submodule commits
make submodule-status
```

### Bulk Update (all repositories)

```bash
python3 ~/Python/src/github.com/abitofhelp/git/update_submodules.py

# Options:
#   --dry-run   Show what would happen without changes
#   --no-push   Update locally but do not push to remote
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

**Status**: Production Ready (v2.1.1)

- ✅ Result<T,E> with 17 operations
- ✅ Option<T> with 11 operations
- ✅ Either<L,R> with 8 operations
- ✅ Try module for exception bridging
- ✅ Pure packages (no side effects)
- ✅ Zero external dependencies
- ✅ Comprehensive test suite
- ✅ Production ready
- ✅ Alire publication
