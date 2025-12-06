# Quick Start Guide

**Version:** 2.3.0<br>
**Date:** December 05, 2025<br>
**SPDX-License-Identifier:** BSD-3-Clause<br>
**License File:** See the LICENSE file in the project root<br>
**Copyright:** © 2025 Michael Gardner, A Bit of Help, Inc.<br>
**Status:** Released

Type-safe error handling for Ada 2022: `Result<T,E>`, `Option<T>`, `Either<L,R>`

---

## Setup

```ada
pragma Ada_2022;
with Functional.Result;
with Functional.Option;
with Functional.Either;
with Functional.Try;

-- Define your error type
type Error_Kind is (IO_Error, Parse_Error, Validation_Error);
type Error is record
   Kind    : Error_Kind;
   Message : String (1 .. 100);
end record;

-- Instantiate for your types
package Int_Result is new Functional.Result (T => Integer, E => Error);
package Int_Option is new Functional.Option (T => Integer);
```

---

## Result<T,E> — Error Handling (20 Operations)

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

**Basic Example:**
```ada
function Parse_Int (S : String) return Int_Result.Result is
begin
   return Int_Result.Ok (Integer'Value (S));
exception
   when Constraint_Error =>
      return Int_Result.Err ((Parse_Error, "Invalid integer" & [15 .. 100 => ' ']));
end Parse_Int;

R := Parse_Int ("42");
if Int_Result.Is_Ok (R) then
   Put_Line (Integer'Image (Int_Result.Value (R)));  -- 42
end if;
```

**Transform with Map:**
```ada
function Double (X : Integer) return Integer is (X * 2);
function Transform is new Int_Result.Map (F => Double);

R := Transform (Int_Result.Ok (5));  -- Ok(10)
R := Transform (Int_Result.Err (E)); -- Err(E) unchanged
```

---

## Option<T> — Optional Values (11 Operations)

| Category | Operations | Purpose |
|----------|------------|---------|
| **Construct** | `New_Some(v)`, `None` | Create present or absent value |
| **Predicates** | `Is_Some(o)`, `Is_None(o)` | Test presence |
| **Extract** | `Value(o)` | Get value (with Pre) |
| **Defaults** | `Unwrap_Or(o, default)`, `Unwrap_Or_With(o)` | Get value or fallback (eager/lazy) |
| **Transform** | `Map(o)`, `And_Then(o)` | Transform Some value, chain operations |
| **Filter** | `Filter(o)` | Keep value only if predicate holds |
| **Fallback** | `Or_Else(a, b)`, `Or_Else_With(o)`, `Fallback` | Try alternative on None (eager/lazy) |

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

**Filter Example:**
```ada
function Is_Adult (Age : Integer) return Boolean is (Age >= 18);
function Adults_Only is new Int_Option.Filter (Pred => Is_Adult);

O := Adults_Only (Int_Option.New_Some (25));  -- Some(25)
O := Adults_Only (Int_Option.New_Some (15));  -- None
```

---

## Either<L,R> — Neutral Choice (10 Operations)

| Category | Operations | Purpose |
|----------|------------|---------|
| **Construct** | `Left(v)`, `Right(v)` | Create left or right value |
| **Predicates** | `Is_Left(e)`, `Is_Right(e)` | Test which side |
| **Extract** | `Left_Value(e)`, `Right_Value(e)` | Get value (with Pre) |
| **Transform** | `Map_Left(e)`, `Map_Right(e)`, `Bimap(e)` | Transform one or both sides |
| **Reduce** | `Fold(e)` | Reduce to single value |

**Basic Example:**
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

**Transform with Map_Left/Map_Right:**
```ada
function Upper (S : String) return String is ...;
function Double (X : Integer) return Integer is (X * 2);

function Upper_Left is new Str_Int_Either.Map_Left (F => Upper);
function Double_Right is new Str_Int_Either.Map_Right (F => Double);

E := Upper_Left (Str_Int_Either.Left ("hello"));   -- Left("HELLO")
E := Double_Right (Str_Int_Either.Right (5));      -- Right(10)
```

**Fold to Single Value:**
```ada
function To_Str (S : String) return String is (S);
function Int_To_Str (X : Integer) return String is (Integer'Image (X));

function To_String is new Str_Int_Either.Fold
  (U => String, On_Left => To_Str, On_Right => Int_To_Str);

S := To_String (Str_Int_Either.Left ("hello"));  -- "hello"
S := To_String (Str_Int_Either.Right (42));      -- " 42"
```

---

## Try — Exception Boundaries (5 Functions)

### Try_To_Result (No Parameters)

Convert exception-based code to Result:

```ada
function Load_File return String;  -- may raise
function To_Error (Exc : Exception_Occurrence) return Error is
  ((IO_Error, Exception_Message (Exc) & [others => ' ']));

function Load_Safely is new Functional.Try.Try_To_Functional_Result
  (T => String, E => Error, Result_Pkg => Str_Result,
   Map_Exception => To_Error, Action => Load_File);

R : constant Str_Result.Result := Load_Safely;
-- Never raises! Always returns Ok or Err
```

### Try_To_Option (No Parameters)

When you only need success/failure without error details:

```ada
function Parse_Config return Config;  -- may raise

function Parse_Safely is new Functional.Try.Try_To_Functional_Option
  (T => Config, Option_Pkg => Config_Option, Action => Parse_Config);

O : constant Config_Option.Option := Parse_Safely;
-- Some(config) on success, None on any exception
```

### Try_To_Result_With_Param (With Parameters)

Convert exception-based code that needs input parameters:

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
  ((IO_Error, Exception_Message (Occ) & [others => ' ']));

-- Instantiate parameterized Try bridge
function Write_With_Try is new Functional.Try.Try_To_Result_With_Param
  (T             => Unit,
   E             => Error,
   Param         => String,           -- Supports indefinite types!
   Result_Pkg    => Unit_Result,
   Map_Exception => Map_Exception,
   Action        => Write_Action);

-- Use it - pass parameter directly
R : constant Unit_Result.Result := Write_With_Try ("Hello, World!");
-- Never raises! Safe, thread-safe, no global state
```

### Try_To_Option_With_Param (With Parameters)

When you need parameters but only care about success/failure:

```ada
function Read_Line (Prompt : String) return String;  -- may raise

function Read_Safely is new Functional.Try.Try_To_Option_With_Param
  (T => String, Param => String, Option_Pkg => Str_Option, Action => Read_Line);

O : constant Str_Option.Option := Read_Safely ("Enter name: ");
-- Some(input) on success, None on any exception
```

**Why use With_Param variants?**
- Thread-safe: No module-level mutable state
- Type-safe: No `Unchecked_Access` needed
- Flexible: Supports indefinite types (String, unconstrained arrays)
- Clean: Direct parameter passing

---

## Common Patterns

### Railway-Oriented Programming (Chaining)

```ada
function Is_Positive (X : Integer) return Boolean is (X > 0);
function Not_Positive_Error (X : Integer) return Error is
  ((Validation_Error, "Not positive" & [12 .. 100 => ' ']));

function Validate is new Int_Result.Ensure
  (Pred => Is_Positive, To_Error => Not_Positive_Error);

function Add_One (X : Integer) return Int_Result.Result is
  (Int_Result.Ok (X + 1));

function Chain is new Int_Result.And_Then (F => Add_One);

Result := Chain (Validate (Int_Result.Ok (5)));
-- Ok(5) → Ensure positive → Ok(5) → Add_One → Ok(6)

Result := Chain (Validate (Int_Result.Ok (-1)));
-- Ok(-1) → Ensure positive → Err(Not positive) → Chain skipped → Err
```

### Error Context Breadcrumbs

```ada
function Add_Context (E : Error; Msg : String) return Error is
  ((E.Kind, E.Message (1 .. E.Len) & " :: " & Msg & [others => ' ']));

function With_File_Context is new Str_Result.With_Context
  (Append => Add_Context);

R := With_File_Context (R, "reading config.yaml");
-- Error message: "File not found :: reading config.yaml"
```

### Fallback on Error

```ada
-- Eager: both evaluated
R := Int_Result.Fallback (Try_Primary, Try_Backup);

-- Lazy: backup only evaluated if primary fails
function Get_Backup return Int_Result.Result is ...;
function Fallback_Lazy is new Int_Result.Fallback_With (F => Get_Backup);

R := Fallback_Lazy (Try_Primary);
```

### Logging with Tap

```ada
procedure Log_Ok (V : Integer) is
begin
   Put_Line ("Success: " & Integer'Image (V));
end Log_Ok;

procedure Log_Err (E : Error) is
begin
   Put_Line ("Error: " & E.Message (1 .. E.Len));
end Log_Err;

function With_Logging is new Int_Result.Tap
  (On_Ok => Log_Ok, On_Err => Log_Err);

R := With_Logging (Some_Operation);  -- Logs, returns unchanged Result
```

---

## When to Use Each Type

| Type | Use For | Example |
|------|---------|---------|
| **Result** | Fallible operations with error info | Parsing, I/O, validation, DB queries |
| **Option** | Optional values (absence is OK) | Config values, search results, nullable fields |
| **Either** | Neutral disjunction (both valid) | Union types, "this OR that" data |
| **Try_To_Result** | Exception boundaries (no params) | File I/O, network operations |
| **Try_To_Option** | Exception boundaries (no params, no error details) | Simple lookups, optional parsing |
| **Try_With_Param** | Exception boundaries (with params) | Console output, parameterized I/O |

**Rule of thumb:**
- If absence/failure is an **error**, use **Result**. If it's **expected**, use **Option**.
- If your exception-prone code needs **input parameters**, use **Try_With_Param** variants.
- If you don't need error details, use **Option**. If you need error info, use **Result**.

---

## See Also

- **README.md** - Full documentation with examples
- **docs/formal/** - SRS, SDS, and STG formal documentation

