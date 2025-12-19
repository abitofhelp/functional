# User Guide

**Version:** 4.1.0<br>
**Date:** 2025-12-18<br>
**SPDX-License-Identifier:** BSD-3-Clause<br>
**License File:** See the LICENSE file in the project root<br>
**Copyright:** © 2025 Michael Gardner, A Bit of Help, Inc.<br>
**Status:** Released

---

## Table of Contents

1. [Overview](#overview)
2. [Result Module](#result-module)
3. [Option Module](#option-module)
4. [Either Module](#either-module)
5. [Try Module](#try-module)
6. [Scoped Module](#scoped-module)
7. [Patterns and Best Practices](#patterns-and-best-practices)

---

## Overview

The Functional library provides type-safe functional programming abstractions for Ada 2022:

- **Result[T, E]** - Success/failure with typed errors (30 operations)
- **Option[T]** - Presence/absence without null (25 operations)
- **Either[L, R]** - Neutral disjunction (20 operations)
- **Try** - Exception-to-Result/Option bridges
- **Scoped** - RAII guards for automatic cleanup

All core types (Result, Option, Either) are SPARK-compatible with `SPARK_Mode => On`. Try and Scoped use `SPARK_Mode => Off` for exception and finalization handling.

---

## Result Module

### Overview

Result represents success (`Ok`) or failure (`Error`) with a typed error value.

```ada
with Functional.Result;

type Error_Kind is (Validation_Error, Not_Found, IO_Error);

package User_Result is new Functional.Result
  (T => User_Type, E => Error_Kind);
```

### Type Definition

```ada
type Result (Is_Ok : Boolean := True) is record
   case Is_Ok is
      when True  => Ok_Value    : T;
      when False => Error_Value : E;
   end case;
end record;
```

### Constructors

| Operation | Signature | Description |
|-----------|-----------|-------------|
| `Ok` | `(V : T) -> Result` | Create success value |
| `New_Error` | `(E_Val : E) -> Result` | Create error value |
| `From_Error` | `(E_Val : E) -> Result` | Alias for New_Error (semantic clarity at boundaries) |

```ada
--  Create success
Result := User_Result.Ok (User);

--  Create failure
Result := User_Result.New_Error (Not_Found);
```

### Predicates

| Operation | Signature | Description |
|-----------|-----------|-------------|
| `Is_Ok` | `(R : Result) -> Boolean` | Check if success |
| `Is_Error` | `(R : Result) -> Boolean` | Check if failure |
| `Is_Ok_And` | `generic Pred; (R : Result) -> Boolean` | Success and predicate holds |
| `Is_Error_And` | `generic Pred; (R : Result) -> Boolean` | Error and predicate holds |
| `Is_Ok_Or` | `generic Pred; (R : Result) -> Boolean` | Error or (success and predicate) |
| `Is_Error_Or` | `generic Pred; (R : Result) -> Boolean` | Success or (error and predicate) |
| `Contains` | `(R : Result; Value : T) -> Boolean` | Check if success equals value |

```ada
if User_Result.Is_Ok (Result) then
   Process (User_Result.Value (Result));
end if;

--  Operator alias for Contains
if Result = Expected_User then
   --  User matches
end if;
```

### Extractors

| Operation | Signature | Description |
|-----------|-----------|-------------|
| `Value` | `(R : Result) -> T` | Extract success value (requires `Is_Ok`) |
| `Error` | `(R : Result) -> E` | Extract error value (requires `Is_Error`) |

```ada
if User_Result.Is_Ok (Result) then
   User := User_Result.Value (Result);
else
   Handle (User_Result.Error (Result));
end if;
```

### Defaults

| Operation | Signature | Description |
|-----------|-----------|-------------|
| `Unwrap_Or` | `(R : Result; Default : T) -> T` | Extract or return default |
| `Unwrap_Or_With` | `generic F; (R : Result) -> T` | Extract or call default producer |

```ada
--  Get value or default
User := User_Result.Unwrap_Or (Result, Default_User);

--  Operator alias
User := Result or Default_User;
```

### Transformations

| Operation | Signature | Description |
|-----------|-----------|-------------|
| `Map` | `generic F; (R : Result) -> Result` | Transform success value |
| `Map_Or` | `generic F; (R : Result; Default : T) -> T` | Transform or default (eager) |
| `Map_Or_Else` | `generic F, Default; (R : Result) -> T` | Transform or default (lazy) |
| `And_Then` | `generic F; (R : Result) -> Result` | Chain fallible operations |
| `And_Then_Into` | `generic ...; (R : Result) -> Result_U` | Chain with type change |
| `Map_Error` | `generic F; (R : Result) -> Result` | Transform error value |
| `Bimap` | `generic Map_Ok, Map_Error; (R : Result) -> Result` | Transform both |
| `Zip_With` | `generic ...; (A : Result; B : Result_U) -> Result` | Combine two Results |
| `Flatten` | `generic ...; (Outer : Result) -> Result` | Unwrap nested Result |

```ada
--  Map transforms success
function Double (X : Integer) return Integer is (X * 2);
function Map_Double is new Int_Result.Map (Double);
Result := Map_Double (Result);

--  And_Then chains fallible operations (the workhorse!)
function Validate is new User_Result.And_Then (Validate_User);
Result := Validate (Result);
```

### Recovery

| Operation | Signature | Description |
|-----------|-----------|-------------|
| `Fallback` | `(A, B : Result) -> Result` | Try alternative on error (eager) |
| `Fallback_With` | `generic F; (R : Result) -> Result` | Try alternative on error (lazy) |
| `Recover` | `generic Handle; (R : Result) -> T` | Turn error into value |
| `Recover_With` | `generic Handle; (R : Result) -> Result` | Turn error into Result |

```ada
--  Fallback operator alias
Config := Primary_Config or Backup_Config;
```

### Validation

| Operation | Signature | Description |
|-----------|-----------|-------------|
| `Ensure` | `generic Pred, To_Error; (R : Result) -> Result` | Validate success value |
| `With_Context` | `generic Append; (R : Result; Msg : String) -> Result` | Enrich error |

### Side Effects

| Operation | Signature | Description |
|-----------|-----------|-------------|
| `Tap` | `generic On_Ok, On_Error; (R : Result) -> Result` | Run side effects |
| `Tap_Ok` | `generic On_Ok; (R : Result) -> Result` | Side effect on success |
| `Tap_Error` | `generic On_Error; (R : Result) -> Result` | Side effect on error |

### Conversion

| Operation | Signature | Description |
|-----------|-----------|-------------|
| `To_Option` | `generic ...; (R : Result) -> Option_Type` | Convert to Option |

---

## Option Module

### Overview

Option represents presence (`Some`) or absence (`None`) without null references.

```ada
with Functional.Option;

package User_Option is new Functional.Option (T => User_Type);
```

### Type Definition

```ada
type Option (Has_Value : Boolean := False) is record
   case Has_Value is
      when True  => Value : T;
      when False => null;
   end case;
end record;
```

### Constructors

| Operation | Signature | Description |
|-----------|-----------|-------------|
| `New_Some` | `(V : T) -> Option` | Create present value |
| `None` | `() -> Option` | Create absent value |

```ada
Opt := User_Option.New_Some (User);
Opt := User_Option.None;
```

### Predicates

| Operation | Signature | Description |
|-----------|-----------|-------------|
| `Is_Some` | `(O : Option) -> Boolean` | Check if present |
| `Is_None` | `(O : Option) -> Boolean` | Check if absent |
| `Is_Some_And` | `generic Pred; (O : Option) -> Boolean` | Present and predicate holds |
| `Is_None_Or` | `generic Pred; (O : Option) -> Boolean` | Absent or predicate holds |
| `Contains` | `(O : Option; Value : T) -> Boolean` | Check if value equals |

### Extractors

| Operation | Signature | Description |
|-----------|-----------|-------------|
| `Value` | `(O : Option) -> T` | Extract value (requires `Has_Value`) |

### Defaults

| Operation | Signature | Description |
|-----------|-----------|-------------|
| `Unwrap_Or` | `(O : Option; Default : T) -> T` | Extract or return default |
| `Unwrap_Or_With` | `generic F; (O : Option) -> T` | Extract or call producer |

```ada
--  Get value or default
Name := Name_Option.Unwrap_Or (Opt, "Unknown");

--  Operator alias
Name := Opt or "Unknown";
```

### Transformations

| Operation | Signature | Description |
|-----------|-----------|-------------|
| `Map` | `generic F; (O : Option) -> Option` | Transform present value |
| `Map_Or` | `generic F; (O : Option; Default : T) -> T` | Transform or default (eager) |
| `Map_Or_Else` | `generic F, Default; (O : Option) -> T` | Transform or default (lazy) |
| `And_Then` | `generic F; (O : Option) -> Option` | Chain optional operations |
| `Filter` | `generic Pred; (O : Option) -> Option` | Keep only if predicate holds |
| `Zip_With` | `generic ...; (A : Option; B : Option_U) -> Option` | Combine two Options |
| `Flatten` | `generic ...; (Outer : Option) -> Inner_Option` | Unwrap nested Option |

```ada
--  Filter keeps value only if predicate passes
function Is_Active is new User_Option.Filter (User_Is_Active);
Opt := Is_Active (Opt);
```

### Fallback

| Operation | Signature | Description |
|-----------|-----------|-------------|
| `Or_Else` | `(A, B : Option) -> Option` | Fallback on absence (eager) |
| `Or_Else_With` | `generic F; (O : Option) -> Option` | Fallback on absence (lazy) |
| `Fallback` | `(A, B : Option) -> Option` | Alias for Or_Else |

### Operators

| Operator | Description |
|----------|-------------|
| `or` (Option, T) | Unwrap_Or |
| `or` (Option, Option) | Or_Else |
| `and` | Returns B if both have values |
| `xor` | Returns value if exactly one has value |
| `=` | Contains |

### Conversion

| Operation | Signature | Description |
|-----------|-----------|-------------|
| `Ok_Or` | `generic ...; (O : Option; Error : Error_Type) -> Result_Type` | Convert to Result (eager error) |
| `Ok_Or_Else` | `generic ...; (O : Option) -> Result_Type` | Convert to Result (lazy error) |

---

## Either Module

### Overview

Either represents one of two possible values (Left or Right). Unlike Result, neither side is designated as "error."

```ada
with Functional.Either;

package String_Or_Int is new Functional.Either
  (L => String, R => Integer);
```

### Type Definition

```ada
type Either (Is_Left : Boolean := True) is record
   case Is_Left is
      when True  => Left_Value  : L;
      when False => Right_Value : R;
   end case;
end record;
```

### Constructors

| Operation | Signature | Description |
|-----------|-----------|-------------|
| `Left` | `(V : L) -> Either` | Create left value |
| `Right` | `(V : R) -> Either` | Create right value |

### Predicates

| Operation | Signature | Description |
|-----------|-----------|-------------|
| `Is_Left` | `(E : Either) -> Boolean` | Check if left |
| `Is_Right` | `(E : Either) -> Boolean` | Check if right |
| `Is_Left_And` | `generic Pred; (E : Either) -> Boolean` | Left and predicate holds |
| `Is_Right_And` | `generic Pred; (E : Either) -> Boolean` | Right and predicate holds |
| `Is_Left_Or` | `generic Pred; (E : Either) -> Boolean` | Right or (left and predicate) |
| `Is_Right_Or` | `generic Pred; (E : Either) -> Boolean` | Left or (right and predicate) |
| `Contains` | `(E : Either; Value : R) -> Boolean` | Check if right equals value |

### Extractors

| Operation | Signature | Description |
|-----------|-----------|-------------|
| `Left_Value` | `(E : Either) -> L` | Extract left (requires `Is_Left`) |
| `Right_Value` | `(E : Either) -> R` | Extract right (requires `Is_Right`) |
| `Get_Or_Else` | `(E : Either; Default : R) -> R` | Get right or default |

### Transformations

| Operation | Signature | Description |
|-----------|-----------|-------------|
| `Map_Left` | `generic F; (E : Either) -> Either` | Transform left value |
| `Map_Right` | `generic F; (E : Either) -> Either` | Transform right value |
| `Map` | `generic F; (E : Either) -> Either` | Transform right (convenience) |
| `Bimap` | `generic Map_L, Map_R; (E : Either) -> Either` | Transform both |
| `Swap` | `generic ...; (E : Either) -> Either_Swapped` | Exchange left and right |
| `And_Then` | `generic F; (E : Either) -> Either` | Chain (right-biased) |

### Reduction

| Operation | Signature | Description |
|-----------|-----------|-------------|
| `Fold` | `generic On_Left, On_Right; (E : Either) -> U` | Reduce to single value |
| `Merge` | `generic From_Left, From_Right; (E : Either) -> T` | Extract when both types same |

### Conversion

| Operation | Signature | Description |
|-----------|-----------|-------------|
| `To_Option` | `generic ...; (E : Either) -> Option_Type` | Right to Some, Left to None |
| `To_Result` | `generic ...; (E : Either) -> Result_Type` | Right to Ok, Left to Error |

---

## Try Module

### Overview

Try bridges exception-based code to Result/Option types at system boundaries.

**SPARK Boundary:** Try uses `SPARK_Mode => Off` because it handles exceptions. Use at infrastructure layer boundaries only.

### Choosing the Right Try Function

| Scenario | Use |
|----------|-----|
| Declarative exception mapping (recommended) | `Map_To_Result` / `Map_To_Result_With_Param` |
| Catch-all with default error | `Map_To_Result.Run_Catch_All` |
| Probe operations with defaults | `Try_To_Functional_Option` / `Try_To_Option_With_Param` |
| Legacy procedural mapping | `Try_To_Result` (deprecated) |

### Map_To_Result_With_Param (Recommended)

Declarative exception-to-error mapping with parameter support:

```ada
with Functional.Try.Map_To_Result_With_Param;
with Ada.IO_Exceptions;

--  1. Define error factory
function Make_Error (Kind : Error_Kind; Msg : String)
  return IO_Result.Result is
begin
   return IO_Result.New_Error (Kind);
end Make_Error;

--  2. Define action that may raise
function Read_File_Raw (Path : String) return IO_Result.Result is
begin
   --  File I/O that may raise
   return IO_Result.Ok (Data);
end Read_File_Raw;

--  3. Instantiate
package Try_Read is new Functional.Try.Map_To_Result_With_Param
  (Error_Kind_Type    => Error_Kind,
   Param_Type         => String,
   Result_Type        => IO_Result.Result,
   Make_Error         => Make_Error,
   Default_Error_Kind => Internal_Error,
   Action             => Read_File_Raw);

--  4. Declare mappings (data, not code!)
Mappings : constant Try_Read.Mapping_Array :=
  [(Ada.IO_Exceptions.Name_Error'Identity, Not_Found),
   (Ada.IO_Exceptions.Use_Error'Identity,  Permission_Error)];

--  5. Use it
Result := Try_Read.Run (File_Path, Mappings);

--  Or catch-all
Result := Try_Read.Run_Catch_All (File_Path);
```

### Map_To_Result (No Parameter)

Same as above when action takes no parameters:

```ada
package Try_Init is new Functional.Try.Map_To_Result
  (Error_Kind_Type    => Error_Kind,
   Result_Type        => Init_Result.Result,
   Make_Error         => Make_Error,
   Default_Error_Kind => Internal_Error,
   Action             => Initialize_System);

Result := Try_Init.Run (Mappings);
```

### Try_To_Functional_Option (Probes)

Use for "probe" operations where exceptions mean absence:

```ada
with Functional.Try;
with Functional.Option;

package Bool_Option is new Functional.Option (T => Boolean);

function Check_File_Raw return Boolean is
begin
   --  Returns True/raises on error
end Check_File_Raw;

function Try_Check is new Functional.Try.Try_To_Functional_Option
  (T          => Boolean,
   Option_Pkg => Bool_Option,
   Action     => Check_File_Raw);

--  Returns None on any exception
Exists := Bool_Option.Unwrap_Or (Try_Check, False);
```

### Try_To_Option_With_Param (Parameterized Probes)

Same pattern with parameter:

```ada
function Is_TZif_File_Raw (Path : String) return Boolean is ...;

function Try_Is_TZif is new Functional.Try.Try_To_Option_With_Param
  (T          => Boolean,
   Param      => String,
   Option_Pkg => Bool_Option,
   Action     => Is_TZif_File_Raw);

Is_TZif := Bool_Option.Unwrap_Or (Try_Is_TZif (Path), False);
```

### When to Use Option vs Result

**Use Option when:**
- Failure means "not found" or "doesn't apply"
- You have a sensible default value
- Error details don't matter
- Pattern: `Unwrap_Or(Try_Action(...), default)`

**Use Result when:**
- Error details matter for logging/debugging
- Different exceptions need different handling
- Failures should be explicit in the type system

---

## Scoped Module

### Overview

Scoped provides RAII (Resource Acquisition Is Initialization) guards for automatic resource cleanup.

**SPARK Boundary:** Scoped uses `SPARK_Mode => Off` because it uses Ada.Finalization.

### Guard_For

Unconditional cleanup when guard goes out of scope:

```ada
with Functional.Scoped;
with Ada.Streams.Stream_IO;

package File_Guard is new Functional.Scoped.Guard_For
  (Resource => Ada.Streams.Stream_IO.File_Type,
   Release  => Ada.Streams.Stream_IO.Close);

declare
   File  : aliased Ada.Streams.Stream_IO.File_Type;
   Guard : File_Guard.Guard (File'Access);
begin
   Ada.Streams.Stream_IO.Open (File, In_File, "data.txt");
   --  ... use file ...
end;  --  File automatically closed here, even on exception
```

### Conditional_Guard_For

Cleanup only when condition is met:

```ada
function Is_Open (F : File_Type) return Boolean is
  (Ada.Streams.Stream_IO.Is_Open (F));

package Safe_File_Guard is new Functional.Scoped.Conditional_Guard_For
  (Resource       => Ada.Streams.Stream_IO.File_Type,
   Should_Release => Is_Open,
   Release        => Ada.Streams.Stream_IO.Close);

declare
   File  : aliased Ada.Streams.Stream_IO.File_Type;
   Guard : Safe_File_Guard.Guard (File'Access);
begin
   --  Open may fail
   Ada.Streams.Stream_IO.Open (File, In_File, Path);
   --  ... use file ...
end;  --  Only closes if Is_Open returns True
```

---

## Patterns and Best Practices

### Railway-Oriented Programming

Chain operations without explicit error checking:

```ada
Result := User_Result.Ok (User)
  |> Map (Validate'Access)
  |> And_Then (Save'Access)
  |> Map (Format'Access);

--  Without prefix notation:
function Validate_User is new User_Result.And_Then (Validate);
function Save_User is new User_Result.And_Then (Save);
Result := Save_User (Validate_User (User_Result.Ok (User)));
```

### Extract with Default

```ada
--  Using operator
User := Result or Default_User;

--  Using function
User := User_Result.Unwrap_Or (Result, Default_User);
```

### Error Recovery

```ada
--  Try fallback
Config := Primary_Config or Backup_Config;

--  Recover from error
function Make_Default is new User_Result.Recover (Create_Default_From_Error);
User := Make_Default (Result);
```

### Exception Boundaries

Keep exceptions at infrastructure layer only:

```ada
--  Infrastructure layer: converts exceptions to Result
function Load_Config (Path : String) return Config_Result.Result is
begin
   return Try_Load.Run (Path, Config_Mappings);
end Load_Config;

--  Application layer: works with Result only
function Process_Config (R : Config_Result.Result) return App_Result.Result is
begin
   if Config_Result.Is_Ok (R) then
      return Process (Config_Result.Value (R));
   else
      return App_Result.New_Error (Config_Failed);
   end if;
end Process_Config;
```

### SPARK Compatibility

Domain and application layers use SPARK-compatible types:

```ada
pragma SPARK_Mode (On);

package Domain.User is
   package User_Result is new Functional.Result
     (T => User_Type, E => Error_Kind);

   --  Pure functions, no exceptions
   function Validate (U : User_Type) return User_Result.Result;
end Domain.User;
```

Infrastructure layer bridges to SPARK types:

```ada
pragma SPARK_Mode (Off);

package Infrastructure.User_Repository is
   --  Converts database exceptions to Domain.User_Result
   function Find (ID : User_ID) return Domain.User_Result.Result;
end Infrastructure.User_Repository;
```

---

## Design Philosophy

### Railway-Oriented Programming

The Functional library implements **railway-oriented programming** (ROP), a pattern where operations form a "railway track" with two paths:

- **Success track**: Values flow through transformations
- **Error track**: Errors short-circuit and propagate unchanged

```
Input -> [Parse] -> [Validate] -> [Transform] -> [Save] -> Output
            |           |             |            |
         Error ----------------------------------------> Error
```

When an operation fails, subsequent operations are skipped, and the error propagates to the end. This eliminates nested if-else chains and exception handling scattered throughout code.

### Explicit Over Implicit

The library favors **explicit error handling** over implicit exceptions:

| Approach | Pros | Cons |
|----------|------|------|
| Exceptions | Less boilerplate | Hidden control flow, hard to reason about |
| Result types | Visible in signatures, composable | More verbose |

Result types make error handling **visible in the type signature**:

```ada
--  Implicit: caller doesn't know this can fail
function Parse (S : String) return Integer;

--  Explicit: failure is part of the contract
function Parse (S : String) return Int_Result.Result;
```

---

## Exception Boundary Pattern

### The Core Principle

**Exceptions should only be handled at system boundaries**, not scattered throughout application code.

```
+-------------------------------------------------------------+
|  External APIs (Ada.Text_IO, GNAT.Sockets, third-party)     |
|  - May raise exceptions                                      |
+-------------------------------------------------------------+
                              |
                              v
+-------------------------------------------------------------+
|  Functional.Try (SPARK_Mode => Off)                         |
|  - Catches ALL exceptions                                    |
|  - Converts to Result/Option                                 |
|  - ~5% of code, manually audited                             |
+-------------------------------------------------------------+
                              |
                              v
+-------------------------------------------------------------+
|  Application Code using Result/Option/Either                 |
|  - SPARK_Mode => On (formally verifiable)                    |
|  - No exception handling needed                              |
|  - ~95% of code, provably correct                            |
+-------------------------------------------------------------+
```

### Why This Design?

1. **SPARK Compatibility**: Exception handlers are prohibited in SPARK. By isolating them to Try, the rest of the library is formally verifiable.
2. **Predictable Control Flow**: Result types make error paths explicit and visible.
3. **Composability**: Result operations chain naturally; exceptions break composition.
4. **Testability**: Pure functions with Result returns are easier to test than exception-throwing code.

### User-Provided Functions Must Not Raise

Generic formal functions passed to `Map`, `And_Then`, etc. **must not raise exceptions**:

```ada
--  Map calls F(value) - if F raises, exception propagates uncaught
generic
   with function F (X : T) return T;  --  Must not raise!
function Map (R : Result) return Result;
```

If your function can raise, wrap it with Try first. Exceptions in user callbacks indicate bugs, not expected errors.

---

## Common Pitfalls

### Pitfall 1: Passing Exception-Raising Functions

```ada
--  WRONG: Integer'Value can raise Constraint_Error
function Parse (S : String) return Integer is (Integer'Value (S));
function Do_Parse is new Int_Result.Map (F => Parse);  --  Exception escapes!

--  CORRECT: Wrap with Try first
function Safe_Parse is new Try.Try_To_Functional_Result (...);
```

### Pitfall 2: Forgetting to Check Before Extract

```ada
--  WRONG: May raise discriminant check error
Value := Int_Result.Value (R);  --  What if R is Error?

--  CORRECT: Check first (or use Unwrap_Or)
if Int_Result.Is_Ok (R) then
   Value := Int_Result.Value (R);
end if;

--  BETTER: Use Unwrap_Or for default
Value := R or 0;
```

### Pitfall 3: Using Either When Result is Appropriate

```ada
--  WRONG: Using Either for error handling
package Parse_Either is new Either (L => String, R => Integer);
--  Semantically unclear: is Left the error or the alternative?

--  CORRECT: Use Result for errors
package Parse_Result is new Result (T => Integer, E => Parse_Error);
--  Clear: Ok is success, Error is failure
```

---

## Embedded Systems Usage

### Library Characteristics

| Property | Status | Benefit |
|----------|--------|---------|
| Preelaborate | All packages | Static elaboration, predictable startup |
| Zero heap allocation | All types | No malloc/free, deterministic memory |
| No controlled types in core | Result/Option/Either | No finalization overhead |
| No tasking | All packages | Ravenscar compatible |
| Bounded memory | Discriminated records | Fixed-size, stack-allocated |

### Memory Model

All types are **stack-allocated discriminated records**:

```ada
--  Option[Integer] is approximately:
--  1 byte discriminant + 4 bytes value = 5 bytes (+ alignment)

--  Result[Integer, Error] is approximately:
--  1 byte discriminant + max(4 bytes, sizeof(Error)) = varies
```

No heap allocation ever occurs within the library.

---

## Integration Patterns

### Layered Architecture

```
+-------------------------------------------------------------+
|  API Layer (Facade)                                          |
|  - Receives Result/Option from Application                   |
|  - Converts to HTTP responses, CLI output, etc.              |
+-------------------------------------------------------------+
                              |
                              v
+-------------------------------------------------------------+
|  Application Layer                                           |
|  - Orchestrates domain operations                            |
|  - All operations return Result/Option                       |
|  - No exception handling here                                |
+-------------------------------------------------------------+
                              |
                              v
+-------------------------------------------------------------+
|  Domain Layer                                                |
|  - Pure business logic                                       |
|  - Result/Option for all fallible operations                 |
|  - SPARK_Mode => On (100% provable)                          |
+-------------------------------------------------------------+
                              |
                              v
+-------------------------------------------------------------+
|  Infrastructure Layer                                        |
|  - Adapters for external systems                             |
|  - Try wrappers at I/O boundaries                            |
|  - Converts exceptions to Result/Option                      |
+-------------------------------------------------------------+
```

### Repository Pattern with Result

```ada
package User_Repository is
   function Find_By_ID (ID : User_ID) return User_Result.Result;
   function Save (User : User_Type) return Unit_Result.Result;
   function Delete (ID : User_ID) return Unit_Result.Result;
end User_Repository;

--  Implementation wraps database exceptions
function Find_By_ID (ID : User_ID) return User_Result.Result is
   function Do_Query return User_Type is ...;  --  May raise
   function Safe_Query is new Try.Try_To_Functional_Result
     (T => User_Type, E => DB_Error, Result_Pkg => User_Result,
      Map_Exception => To_DB_Error, Action => Do_Query);
begin
   return Safe_Query;
end Find_By_ID;
```

### Error Context Breadcrumbs

Add context at each layer for debugging:

```ada
function Add_Context (E : Error; Msg : String) return Error is ...;
function With_Context is new Result.With_Context (Append => Add_Context);

--  Each layer adds context
R := With_Context (Inner_Operation, "in Process_Order");
R := With_Context (R, "for customer " & Customer_ID);
--  Error: "File not found :: in Process_Order :: for customer 12345"
```

---

## See Also

- [Cheatsheet](cheatsheet.md) - Quick reference for all operations
- [Quick Start Guide](../quick_start.md) - Get started in minutes
- [CHANGELOG](../../CHANGELOG.md) - Version history

---

**Document Control:**
- Version: 4.1.0
- Last Updated: 2025-12-18
- Status: Released
