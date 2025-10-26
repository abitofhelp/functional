# Functional - Type-Safe Error Handling for Ada 2022

**Version:** 1.0.0
**Date:** October 25, 2025
**SPDX-License-Identifier:** BSD-3-Clause
**License File:** See the LICENSE file in the project root.
**Copyright:** © 2025 Michael Gardner, A Bit of Help, Inc.
**Status:** Release

A clean, Ada-idiomatic library providing `Result<T,E>`, `Option<T>`, and `Either<L,R>` types for functional error handling in Ada 2022.

## Features

✅ **Result<T,E>** - Type-safe error handling (17 operations)
✅ **Option<T>** - Optional values (11 operations)
✅ **Either<L,R>** - Disjoint union type (8 operations)
✅ **Try.To_Result** - Convert exceptions to Result
✅ **Try.To_Option** - Convert exceptions to Option
✅ **Pure packages** - No side effects, compile-time guarantees
✅ **Zero dependencies** - Just Ada 2022 standard library
✅ **Production ready** - Comprehensive compiler checks and style enforcement

## Installation

Add to your `alire.toml`:

```toml
[[depends-on]]
functional = "^1.0.0"
```

Then run:
```bash
alr build
```

## Quick Start

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

## API Reference

### Result<T,E>

| Function | Purpose | Example |
|----------|---------|---------|
| `Ok(v)`, `Err(e)` | Constructors | `Str_Result.Ok("success")` |
| `Is_Ok`, `Is_Err` | Check state | `if Is_Ok(r) then ...` |
| `Value`, `Error` | Extract values | `Str_Result.Value(r)` |
| `Unwrap_Or` | Get value or default | `Unwrap_Or(r, "default")` |
| `And_Then` | Chain operations | See examples below |
| `Fallback` | Try alternative on error | `Fallback(primary, backup)` |
| `Recover` | Turn error into value | `Recover(r)` |
| `Ensure` | Validate with predicate | `Ensure(r)` |
| `With_Context` | Add error breadcrumbs | `With_Context(r, "in function X")` |

### Option<T>

| Function | Purpose | Example |
|----------|---------|---------|
| `New_Some(v)`, `None` | Constructors | `Int_Option.New_Some(42)` |
| `Is_Some`, `Is_None` | Check presence | `if Is_Some(opt) then ...` |
| `Value` | Extract value | `Int_Option.Value(opt)` |
| `Unwrap_Or` | Get value or default | `Unwrap_Or(opt, 0)` |
| `And_Then` | Chain operations | See examples below |
| `Filter` | Keep if predicate holds | `Filter(opt)` |
| `Or_Else` | Try alternative | `Or_Else(primary, backup)` |

### Either<L,R>

| Function | Purpose | Example |
|----------|---------|---------|
| `Left(v)`, `Right(v)` | Constructors | `Str_Int_Either.Left("error")` |
| `Is_Left`, `Is_Right` | Check side | `if Is_Left(e) then ...` |
| `Left_Value`, `Right_Value` | Extract values | `Left_Value(e)` |

## Patterns

### Chaining with And_Then

```ada
--  Chain fallible operations
function Load_And_Parse (Path : String) return Int_Result.Result is
   function Load_File (P : String) return Str_Result.Result is ...;
   function Parse_Int (S : String) return Int_Result.Result is ...;

   --  Instantiate And_Then
   function Chain is new Str_Result.And_Then (F => Parse_Int);
begin
   return Chain (Load_File (Path));
end Load_And_Parse;
```

### Validation with Ensure

```ada
function Is_Positive (X : Integer) return Boolean is (X > 0);
function Not_Positive_Error (X : Integer) return Error is
  ((Validation_Error, "Must be positive"));

function Validate is new Int_Result.Ensure
  (Pred => Is_Positive, To_Error => Not_Positive_Error);

Result := Validate (Int_Result.Ok (-5));  --  Returns Err
```

### Error Context Breadcrumbs

```ada
function Add_Context (E : Error; Msg : String) return Error is
   (E.Kind, E.Message & " :: " & Msg);

function With_File_Context is new Str_Result.With_Context
  (Append => Add_Context);

R := With_File_Context (R, "reading config.yaml");
```

## Integration with Hybrid Architecture

This library works seamlessly with clean/hexagonal architecture patterns. Since Functional uses package-level generics, you can use it directly or wrap it in domain-specific adapters.

### Direct Usage (Recommended)

```ada
--  In your application/domain layer
with Functional.Result;

--  Define your domain error type
type Domain_Error is record
   Kind    : Error_Kind;
   Message : String (1 .. 200);
end record;

--  Instantiate directly for your domain types
package User_Result is new Functional.Result
  (T => User, E => Domain_Error);

--  Use in domain services
function Find_User (ID : String) return User_Result.Result;
```

### Adapter Pattern (For Decoupling)

If you want to completely decouple from Functional library:

```ada
--  Domain layer defines its own generic Result
--  domain/src/model/result.ads
generic
   type T is private;
   type E is private;
package Domain.Result is
   type Result is private;
   function Ok (V : T) return Result;
   function Is_Ok (R : Result) return Boolean;
   --  ... only the functions your domain needs
private
   --  No implementation details exposed
end Domain.Result;

--  Infrastructure provides implementation using Functional
--  infrastructure/adapters/functional_adapter.adb
with Functional.Result;
package body Domain.Result is
   package FR is new Functional.Result (T => T, E => E);
   type Result is new FR.Result;  --  Derive from Functional's type

   function Ok (V : T) return Result is (Result (FR.Ok (V)));
   function Is_Ok (R : Result) return Boolean is (FR.Is_Ok (FR.Result (R)));
end Domain.Result;
```

**Benefits:**
- Domain depends only on its own interfaces
- Functional library can be swapped without changing domain code
- Infrastructure layer handles the adaptation

## Design Decisions

### Map Functions

Package-level generics enable same-type `Map` functions (`T -> T`) for Result and Option, and `Map_Left`/`Map_Right` for Either. Use `And_Then` for type-changing transformations.

### Why New_Some?

Following Ada naming conventions for constructors (New_*). Note: `some` and `or` are reserved keywords in Ada 2022.

### Why No Swap for Either?

`Swap` is impossible for `Either<L,R>` when L and R are different types - you can't put an L value into an R slot. Use pattern matching instead.

## Testing

Run the test suite:
```bash
make test
```

Or directly:
```bash
alr run test_runner
```

## Contributing

Contributions welcome! Please:
1. Follow Ada 2022 style guide (enforced by gnatformat)
2. Add tests for new features
3. Keep the API minimal and focused

## License

BSD-3-Clause - See LICENSE file

## Credits

Created by [A Bit of Help, Inc.](https://abitofhelp.com)

Part of the Ada 2022 Hybrid Architecture ecosystem.
