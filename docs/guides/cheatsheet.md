# Cheatsheet

**Version:** 4.1.0<br>
**Date:** 2025-12-18<br>
**SPDX-License-Identifier:** BSD-3-Clause<br>
**License File:** See the LICENSE file in the project root<br>
**Copyright:** © 2025 Michael Gardner, A Bit of Help, Inc.<br>
**Status:** Released

---

## Quick Reference

### Instantiation

```ada
with Functional.Result;
with Functional.Option;
with Functional.Either;

type Error_Kind is (Validation_Error, Not_Found, IO_Error);

package User_Result is new Functional.Result (T => User, E => Error_Kind);
package User_Option is new Functional.Option (T => User);
package String_Or_Int is new Functional.Either (L => String, R => Integer);
```

---

## Result[T, E] - 30 Operations

### Create

| Operation | Example |
|-----------|---------|
| `Ok(v)` | `R := User_Result.Ok (User);` |
| `New_Error(e)` | `R := User_Result.New_Error (Not_Found);` |
| `From_Error(e)` | `R := User_Result.From_Error (IO_Error);` |

### Check

| Operation | Returns | Example |
|-----------|---------|---------|
| `Is_Ok(r)` | Boolean | `if User_Result.Is_Ok (R) then` |
| `Is_Error(r)` | Boolean | `if User_Result.Is_Error (R) then` |
| `Contains(r, v)` | Boolean | `if R = Expected then` (operator) |

### Extract

| Operation | Returns | Example |
|-----------|---------|---------|
| `Value(r)` | T | `U := User_Result.Value (R);` (requires Is_Ok) |
| `Error(r)` | E | `E := User_Result.Error (R);` (requires Is_Error) |
| `Unwrap_Or(r, d)` | T | `U := R or Default;` (operator) |

### Transform (Generic)

| Operation | Description |
|-----------|-------------|
| `Map[F]` | Transform success value |
| `Map_Error[F]` | Transform error value |
| `Bimap[F_Ok, F_Err]` | Transform both |
| `And_Then[F]` | Chain fallible operation (monadic bind) |
| `And_Then_Into[...]` | Chain with type change |

### Recover (Generic)

| Operation | Description |
|-----------|-------------|
| `Fallback(a, b)` | Try alternative on error (eager) |
| `Fallback_With[F]` | Try alternative on error (lazy) |
| `Recover[Handle]` | Error to value |
| `Recover_With[Handle]` | Error to Result |

### Operators

| Operator | Meaning |
|----------|---------|
| `R or Default` | Unwrap_Or |
| `A or B` (both Result) | Fallback |
| `R = Value` | Contains |

---

## Option[T] - 25 Operations

### Create

| Operation | Example |
|-----------|---------|
| `New_Some(v)` | `O := User_Option.New_Some (User);` |
| `None` | `O := User_Option.None;` |

### Check

| Operation | Returns | Example |
|-----------|---------|---------|
| `Is_Some(o)` | Boolean | `if User_Option.Is_Some (O) then` |
| `Is_None(o)` | Boolean | `if User_Option.Is_None (O) then` |
| `Contains(o, v)` | Boolean | `if O = Expected then` (operator) |

### Extract

| Operation | Returns | Example |
|-----------|---------|---------|
| `Value(o)` | T | `U := User_Option.Value (O);` (requires Has_Value) |
| `Unwrap_Or(o, d)` | T | `U := O or Default;` (operator) |

### Transform (Generic)

| Operation | Description |
|-----------|-------------|
| `Map[F]` | Transform present value |
| `And_Then[F]` | Chain optional operation |
| `Filter[Pred]` | Keep only if predicate holds |

### Operators

| Operator | Meaning |
|----------|---------|
| `O or Default` | Unwrap_Or |
| `A or B` (both Option) | Or_Else |
| `A and B` | Returns B if both have values |
| `A xor B` | Returns value if exactly one has value |
| `O = Value` | Contains |

---

## Either[L, R] - 20 Operations

### Create

| Operation | Example |
|-----------|---------|
| `Left(v)` | `E := String_Or_Int.Left ("hello");` |
| `Right(v)` | `E := String_Or_Int.Right (42);` |

### Check

| Operation | Returns | Example |
|-----------|---------|---------|
| `Is_Left(e)` | Boolean | `if String_Or_Int.Is_Left (E) then` |
| `Is_Right(e)` | Boolean | `if String_Or_Int.Is_Right (E) then` |

### Extract

| Operation | Returns | Example |
|-----------|---------|---------|
| `Left_Value(e)` | L | `S := String_Or_Int.Left_Value (E);` |
| `Right_Value(e)` | R | `I := String_Or_Int.Right_Value (E);` |
| `Get_Or_Else(e, d)` | R | `I := String_Or_Int.Get_Or_Else (E, 0);` |

### Transform (Generic)

| Operation | Description |
|-----------|-------------|
| `Map_Left[F]` | Transform left value |
| `Map_Right[F]` | Transform right value |
| `Map[F]` | Transform right (convenience) |
| `Bimap[F_L, F_R]` | Transform both |
| `Swap[...]` | Exchange left and right |
| `And_Then[F]` | Chain (right-biased) |

### Reduce (Generic)

| Operation | Description |
|-----------|-------------|
| `Fold[On_Left, On_Right]` | Reduce to single value |
| `Merge[From_Left, From_Right]` | Extract when L = R |

---

## Try - Exception Bridges

### Map_To_Result_With_Param (Recommended)

```ada
with Functional.Try.Map_To_Result_With_Param;

package Try_Read is new Functional.Try.Map_To_Result_With_Param
  (Error_Kind_Type    => Error_Kind,
   Param_Type         => String,
   Result_Type        => IO_Result.Result,
   Make_Error         => Make_Error,
   Default_Error_Kind => Internal_Error,
   Action             => Read_Raw);

Mappings : constant Try_Read.Mapping_Array :=
  [(Name_Error'Identity, Not_Found),
   (Use_Error'Identity,  Permission_Error)];

Result := Try_Read.Run (Path, Mappings);
Result := Try_Read.Run_Catch_All (Path);  -- All exceptions -> Internal_Error
```

### Map_To_Result (No Parameter)

```ada
with Functional.Try.Map_To_Result;

package Try_Init is new Functional.Try.Map_To_Result
  (Error_Kind_Type    => Error_Kind,
   Result_Type        => Init_Result.Result,
   Make_Error         => Make_Error,
   Default_Error_Kind => Internal_Error,
   Action             => Initialize);

Result := Try_Init.Run (Mappings);
```

### Try_To_Option (Probes)

```ada
--  For probe operations with sensible defaults
function Try_Check is new Functional.Try.Try_To_Functional_Option
  (T          => Boolean,
   Option_Pkg => Bool_Option,
   Action     => Check_Raw);

Exists := Bool_Option.Unwrap_Or (Try_Check, False);

--  With parameter
function Try_Is_Valid is new Functional.Try.Try_To_Option_With_Param
  (T          => Boolean,
   Param      => String,
   Option_Pkg => Bool_Option,
   Action     => Is_Valid_Raw);

Valid := Bool_Option.Unwrap_Or (Try_Is_Valid (Path), False);
```

---

## Scoped - RAII Guards

### Unconditional Guard

```ada
with Functional.Scoped;

package File_Guard is new Functional.Scoped.Guard_For
  (Resource => File_Type,
   Release  => Close);

declare
   F : aliased File_Type;
   G : File_Guard.Guard (F'Access);
begin
   Open (F, In_File, "data.txt");
   --  use file
end;  -- F closed automatically
```

### Conditional Guard

```ada
package Safe_Guard is new Functional.Scoped.Conditional_Guard_For
  (Resource       => File_Type,
   Should_Release => Is_Open,
   Release        => Close);

declare
   F : aliased File_Type;
   G : Safe_Guard.Guard (F'Access);
begin
   Open (F, In_File, Path);  -- may fail
end;  -- only closes if Is_Open(F)
```

---

## Common Patterns

### Check and Extract

```ada
if User_Result.Is_Ok (R) then
   Process (User_Result.Value (R));
else
   Handle (User_Result.Error (R));
end if;
```

### Extract with Default

```ada
User := R or Default_User;
Name := O or "Unknown";
```

### Chain Operations

```ada
function Validate is new User_Result.And_Then (Validate_User);
function Save is new User_Result.And_Then (Save_User);
Result := Save (Validate (User_Result.Ok (Input)));
```

### Fallback Chain

```ada
Config := Primary or Secondary or Fallback_Config;
```

### Probe with Default

```ada
--  Pattern: Try -> Option -> Unwrap_Or
Exists := Bool_Option.Unwrap_Or (Try_Check (Path), False);
Version := Str_Option.Unwrap_Or (Try_Read_Version (Path), "unknown");
Count := Int_Option.Unwrap_Or (Try_Count (Dir), 0);
```

---

## SPARK Compatibility

| Module | SPARK_Mode |
|--------|------------|
| Functional.Result | On |
| Functional.Option | On |
| Functional.Either | On |
| Functional.Try | Off |
| Functional.Scoped | Off |

Use Result/Option/Either in domain and application layers (SPARK-verifiable).
Use Try and Scoped only at infrastructure boundaries.

---

**Document Control:**
- Version: 4.1.0
- Last Updated: 2025-12-18
- Status: Released
