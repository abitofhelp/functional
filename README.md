# Type-Safe Functional Error Handling

[![License](https://img.shields.io/badge/license-BSD--3--Clause-blue.svg)](LICENSE) [![Ada](https://img.shields.io/badge/Ada-2022-blue.svg)](https://ada-lang.io) [![Alire](https://img.shields.io/badge/Alire-2.0+-blue.svg)](https://alire.ada.dev)

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

## Usage

### Result<T,E> - For Error Handling

```ada
with Functional.Result;

--  Define your error type
type Error_Kind is (Validation_Error, Parse_Error, IO_Error);
type Error is record
   Kind    : Error_Kind;
   Message : String (1 .. 100);
end record;

--  Instantiate Result for your types
package Str_Result is new Functional.Result (T => String, E => Error);

--  Use it in functions
function Parse_Config (Path : String) return Str_Result.Result is
begin
   if not File_Exists (Path) then
      return Str_Result.Err ((IO_Error, "File not found"));
   end if;

   return Str_Result.Ok (Read_File (Path));
end Parse_Config;

--  Handle results
declare
   Config : constant Str_Result.Result := Parse_Config ("app.conf");
begin
   if Str_Result.Is_Ok (Config) then
      Put_Line ("Loaded: " & Str_Result.Value (Config));
   else
      Put_Line ("Error: " & Str_Result.Error (Config).Message);
   end if;
end;
```

### Option<T> - For Optional Values

```ada
with Functional.Option;

package Int_Option is new Functional.Option (T => Integer);

function Find_User_Age (User_ID : String) return Int_Option.Option is
begin
   if User_Exists (User_ID) then
      return Int_Option.New_Some (Get_Age (User_ID));
   else
      return Int_Option.None;
   end if;
end Find_User_Age;

--  Use with default
Age : constant Integer := Int_Option.Unwrap_Or (Find_User_Age ("bob"), 0);
```

### Either<L,R> - For Neutral Choices

```ada
with Functional.Either;

package Str_Int_Either is new Functional.Either
  (L => String, R => Integer);

function Parse_Or_Default (Input : String) return Str_Int_Either.Either is
begin
   if Is_Numeric (Input) then
      return Str_Int_Either.Right (Integer'Value (Input));
   else
      return Str_Int_Either.Left (Input);  --  Keep as string
   end if;
end Parse_Or_Default;
```

### Try Module - Exception Boundaries

The Try module bridges exception-based code to functional Result/Option types:

```ada
with Functional.Try;
with Ada.Exceptions;

function Read_Int32 (Stream : Stream_Access) return Int32_Result.Result is

   function Raw_Read return Integer_32 is
      Value : Integer_32;
   begin
      Integer_32'Read (Stream, Value);
      return Value;
   end Raw_Read;

   function From_Exception (Occ : Exception_Occurrence) return Error_Type is
   begin
      return (IO_Error, Exception_Message (Occ));
   end From_Exception;

   function Try_Read is new Functional.Try.Try_To_Result
     (T             => Integer_32,
      E             => Error_Type,
      Result_Type   => Int32_Result.Result,
      Ok            => Int32_Result.Ok,
      Err           => Int32_Result.From_Error,
      Map_Exception => From_Exception,
      Action        => Raw_Read);
begin
   return Try_Read;
end Read_Int32;
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
