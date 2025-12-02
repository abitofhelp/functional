# Quick Start Guide

**Version:** 2.2.1  
**Date:** December 02, 2025  
**SPDX-License-Identifier:** BSD-3-Clause
**License File:** See the LICENSE file in the project root.
**Copyright:** © 2025 Michael Gardner, A Bit of Help, Inc.  
**Status:** Released  

Type-safe error handling for Ada 2022: `Result<T,E>`, `Option<T>`, `Either<L,R>`

---

## Setup

```ada
pragma Ada_2022;
with Functional.Result;
with Functional.Option;
with Functional.Either;
with Functional.Try.To_Result;

-- Define your error type
type Error_Kind is (IO_Error, Parse_Error, Validation_Error);
type Error is record
   Kind    : Error_Kind;
   Message : String (1 .. 100);
end record;

-- Instantiate for your types
package Str_Result is new Functional.Result (T => String, E => Error);
package Int_Option is new Functional.Option (T => Integer);
```

---

## Result<T,E> — Error Handling

| Operation | Usage | Purpose |
|-----------|-------|---------|
| **Construct** | `Ok(v)`, `Err(e)` | Create result |
| **Check** | `Is_Ok(r)`, `Is_Err(r)` | Test state |
| **Extract** | `Value(r)`, `Error(r)` | Get value (with Pre) |
| **Default** | `Unwrap_Or(r, default)` | Get value or fallback |
| **Chain** | `And_Then(r)` | Compose operations |
| **Recover** | `Recover(r)` | Turn error → value |
| **Validate** | `Ensure(r)` | Check predicate |
| **Context** | `With_Context(r, msg)` | Add error breadcrumbs |

**Example:**
```ada
function Parse_Int (S : String) return Str_Result.Result is
begin
   return Str_Result.Ok (Integer'Value (S));
exception
   when Constraint_Error =>
      return Str_Result.Err ((Parse_Error, "Invalid integer"));
end Parse_Int;

R := Parse_Int ("42");
if Str_Result.Is_Ok (R) then
   Put_Line (Integer'Image (Str_Result.Value (R)));  -- 42
end if;
```

---

## Option<T> — Optional Values

| Operation | Usage | Purpose |
|-----------|-------|---------|
| **Construct** | `New_Some(v)`, `None` | Create option |
| **Check** | `Is_Some(o)`, `Is_None(o)` | Test presence |
| **Extract** | `Value(o)` | Get value (with Pre) |
| **Default** | `Unwrap_Or(o, default)` | Get value or fallback |
| **Chain** | `And_Then(o)` | Compose operations |
| **Filter** | `Filter(o)` | Keep if predicate holds |
| **Alternative** | `Or_Else(a, b)` | Try alternative |

**Example:**
```ada
function Find_User (ID : String) return Int_Option.Option is
begin
   if User_Exists (ID) then
      return Int_Option.New_Some (Get_Age (ID));
   else
      return Int_Option.None;
   end if;
end Find_User;

Age := Int_Option.Unwrap_Or (Find_User ("bob"), 0);
```

---

## Either<L,R> — Neutral Choice

| Operation | Usage | Purpose |
|-----------|-------|---------|
| **Construct** | `Left(v)`, `Right(v)` | Create either |
| **Check** | `Is_Left(e)`, `Is_Right(e)` | Test which side |
| **Extract** | `Left_Value(e)`, `Right_Value(e)` | Get value (with Pre) |

**Example:**
```ada
package Str_Int_Either is new Functional.Either
  (L => String, R => Integer);

function Parse_Or_Keep (Input : String) return Str_Int_Either.Either is
begin
   if Is_Numeric (Input) then
      return Str_Int_Either.Right (Integer'Value (Input));
   else
      return Str_Int_Either.Left (Input);  -- Keep as string
   end if;
end Parse_Or_Keep;
```

---

## Try — Exception Boundaries

### Try.To_Result (No Parameters)

Convert exception-based code to Result:

```ada
function Load_File return String;  -- may raise
function To_Error (Exc : Exception_Occurrence) return Error is
  ((IO_Error, Exception_Message (Exc)));

package Load_Try is new Functional.Try.To_Result
  (T => String, E => Error, T_Result => Str_Result,
   Action => Load_File, Map_Exception => To_Error);

R : constant Str_Result.Result := Load_Try.Run;
-- Never raises! Always returns Ok or Err
```

### Try_To_Result_With_Param (With Parameters)

**NEW in 2.1.0**: Convert exception-based code that needs input parameters:

```ada
with Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;

-- Action that takes a parameter and may raise
function Write_Action (Message : String) return Unit is
begin
   Ada.Text_IO.Put_Line (Message);
   return Unit_Value;
end Write_Action;

-- Map exceptions to domain errors
function Map_Exception (Occ : Exception_Occurrence) return Error is
  ((IO_Error, Exception_Message (Occ)));

-- Instantiate parameterized Try bridge
function Write_With_Try is new Functional.Try.Try_To_Result_With_Param
  (T             => Unit,
   E             => Error,
   Param         => String,           -- ← Parameter type (supports indefinite types!)
   Result_Pkg    => Unit_Result,
   Map_Exception => Map_Exception,
   Action        => Write_Action);

-- Use it - pass parameter directly
R : constant Unit_Result.Result := Write_With_Try ("Hello, World!");
-- Never raises! Safe, thread-safe, no global state
```

**Why use With_Param?**
- ✅ **Thread-safe** - No module-level mutable state
- ✅ **Type-safe** - No `Unchecked_Access` needed
- ✅ **Flexible** - Supports indefinite types (String, unconstrained arrays)
- ✅ **Clean** - Direct parameter passing

---

## Common Patterns

### Railway-Oriented Programming (Chaining)

```ada
function Validate is new Int_Result.Ensure
  (Pred => Is_Positive, To_Error => Not_Positive_Error);

function Add_One (X : Integer) return Int_Result.Result is
  (Int_Result.Ok (X + 1));

function Chain is new Int_Result.And_Then (F => Add_One);

Result := Chain (Validate (Int_Result.Ok (5)));
-- Ok(5) → Ensure positive → Ok(5) → Add_One → Ok(6)
```

### Error Context Breadcrumbs

```ada
function Add_Context (E : Error; Msg : String) return Error is
  ((E.Kind, E.Message & " :: " & Msg));

function With_File_Context is new Str_Result.With_Context
  (Append => Add_Context);

R := With_File_Context (R, "reading config.yaml");
-- Error message: "File not found :: reading config.yaml"
```

### Fallback on Error

```ada
R := Str_Result.Fallback (Try_Primary, Try_Backup);
-- If primary is Err, uses backup
```

---

## When to Use Each Type

| Type | Use For | Example |
|------|---------|---------|
| **Result** | Fallible operations | Parsing, I/O, validation, DB queries |
| **Option** | Optional values (absence is OK) | Config values, search results, nullable fields |
| **Either** | Neutral disjunction | Union types, "this OR that" data |
| **Try** | Exception boundaries (no params) | File I/O, network operations |
| **Try_With_Param** | Exception boundaries (with params) | Console output, parameterized I/O, logging |

**Rule of thumb:**
- If absence/failure is an **error**, use **Result**. If it's **expected**, use **Option**.
- If your exception-prone code needs **input parameters**, use **Try_With_Param** instead of module-level state.

---

## See Also

- **README.md** - Full documentation with examples
- **examples/hybrid_architecture_pattern.md** - Integration with clean architecture
