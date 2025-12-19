# Quick Start Guide

**Version:** 4.1.0<br>
**Date:** 2025-12-18<br>
**SPDX-License-Identifier:** BSD-3-Clause<br>
**License File:** See the LICENSE file in the project root<br>
**Copyright:** © 2025 Michael Gardner, A Bit of Help, Inc.<br>
**Status:** Released

---

## Table of Contents

1. [Installation](#installation)
2. [First Program](#first-program)
3. [Result Type](#result-type)
4. [Option Type](#option-type)
5. [Either Type](#either-type)
6. [Try - Exception Bridges](#try---exception-bridges)
7. [Error Handling Patterns](#error-handling-patterns)
8. [Running Tests](#running-tests)
9. [Common Issues](#common-issues)
10. [Next Steps](#next-steps)

---

## Installation

### Using Alire (Recommended)

```bash
# Add to your project
alr with functional

# Or get standalone
alr get functional
cd functional_*
alr build
```

### Manual Installation

```bash
git clone --recurse-submodules https://github.com/abitofhelp/functional.git
cd functional
alr build
```

### Prerequisites

- **Alire** 2.0+ (Ada package manager)
- **GNAT** 14+ (via Alire toolchain)
- **Make** (for convenience targets)

---

## First Program

Create a simple program that uses Result for error handling:

```ada
pragma Ada_2022;

with Ada.Text_IO;    use Ada.Text_IO;
with Functional.Result;

procedure Hello_Functional is
   --  Define error type
   type Error_Kind is (Parse_Error, Range_Error);

   --  Instantiate Result
   package Int_Result is new Functional.Result
     (T => Integer, E => Error_Kind);

   --  A function that may fail
   function Safe_Parse (S : String) return Int_Result.Result is
   begin
      declare
         Value : constant Integer := Integer'Value (S);
      begin
         if Value < 0 then
            return Int_Result.Error (Range_Error);
         end if;
         return Int_Result.Ok (Value);
      end;
   exception
      when Constraint_Error =>
         return Int_Result.Error (Parse_Error);
   end Safe_Parse;

   Result : constant Int_Result.Result := Safe_Parse ("42");
begin
   if Int_Result.Is_Ok (Result) then
      Put_Line ("Parsed: " & Integer'Image (Int_Result.Value (Result)));
   else
      Put_Line ("Error: " & Error_Kind'Image (Int_Result.Get_Error (Result)));
   end if;
end Hello_Functional;
```

**Expected Output:**
```
Parsed:  42
```

---

## Result Type

Result represents success (`Ok`) or failure (`Error`) with a typed error value.

### Basic Usage

```ada
package User_Result is new Functional.Result (T => User, E => Error_Kind);

--  Create success
Result := User_Result.Ok (My_User);

--  Create failure
Result := User_Result.Error (Not_Found);

--  Check state
if User_Result.Is_Ok (Result) then
   Process (User_Result.Value (Result));
else
   Handle_Error (User_Result.Get_Error (Result));
end if;
```

### Chaining Operations

```ada
--  Map transforms the success value
Result := User_Result.Map (Result, Format_User'Access);

--  And_Then chains fallible operations
Result := User_Result.And_Then (Result, Validate_User'Access);

--  Or_Else provides fallback on error
Result := User_Result.Or_Else (Result, Create_Default_User'Access);

--  Unwrap_Or extracts value with default
User := User_Result.Unwrap_Or (Result, Default_User);
```

---

## Option Type

Option represents presence (`Some`) or absence (`None`) without null references.

### Basic Usage

```ada
package User_Option is new Functional.Option (T => User);

--  Create presence
Opt := User_Option.Some (My_User);

--  Create absence
Opt := User_Option.None;

--  Check state
if User_Option.Is_Some (Opt) then
   Process (User_Option.Value (Opt));
end if;
```

### Chaining Operations

```ada
--  Map transforms if present
Opt := User_Option.Map (Opt, Get_Name'Access);

--  And_Then chains optional operations
Opt := User_Option.And_Then (Opt, Find_Manager'Access);

--  Filter keeps value only if predicate passes
Opt := User_Option.Filter (Opt, Is_Active'Access);

--  Unwrap_Or extracts with default
Name := User_Option.Unwrap_Or (Opt, "Unknown");
```

---

## Either Type

Either represents one of two possible values (Left or Right), where neither is designated as "error."

### Basic Usage

```ada
package String_Or_Int is new Functional.Either (L => String, R => Integer);

--  Create left value
E := String_Or_Int.Left ("hello");

--  Create right value
E := String_Or_Int.Right (42);

--  Pattern match
if String_Or_Int.Is_Left (E) then
   Process_String (String_Or_Int.Left_Value (E));
else
   Process_Int (String_Or_Int.Right_Value (E));
end if;
```

---

## Try - Exception Bridges

Try converts exception-based code to Result/Option at system boundaries.

### Map_To_Result (Recommended)

Use declarative exception mappings:

```ada
with Functional.Try.Map_To_Result_With_Param;

--  Define error factory
function Make_Error (Kind : Error_Kind; Msg : String) return IO_Result.Result is
begin
   return IO_Result.Error (Kind);
end Make_Error;

--  The action that may raise
function Read_File_Raw (Path : String) return String is
begin
   --  File I/O that may raise
end Read_File_Raw;

--  Instantiate Map_To_Result_With_Param
package Try_Read is new Functional.Try.Map_To_Result_With_Param
  (Error_Kind_Type    => Error_Kind,
   Param_Type         => String,
   Result_Type        => IO_Result.Result,
   Make_Error         => Make_Error,
   Default_Error_Kind => IO_Error,
   Action             => Read_File_Raw);

--  Declarative exception mappings
Mappings : constant Try_Read.Mapping_Array :=
  [(Ada.IO_Exceptions.Name_Error'Identity, Not_Found),
   (Ada.IO_Exceptions.Use_Error'Identity,  Permission_Denied)];

--  Use it
Result := Try_Read.Run (File_Path, Mappings);
```

### To_Option (For Probes)

Use when you just need success/failure with a default:

```ada
with Functional.Try.To_Option;

function Check_File_Raw (Path : String) return Boolean is ...;

package Try_Check is new Functional.Try.To_Option
  (T        => Boolean,
   T_Option => Bool_Option,
   Action   => Check_File_Raw);

--  Returns None on any exception, then use Unwrap_Or
Exists := Bool_Option.Unwrap_Or (Try_Check.Run, False);
```

---

## Error Handling Patterns

### Pattern 1: Check and Extract

```ada
Result := Do_Something;

if Int_Result.Is_Ok (Result) then
   Value := Int_Result.Value (Result);
   --  Use Value
else
   Error := Int_Result.Get_Error (Result);
   --  Handle Error
end if;
```

### Pattern 2: Extract with Default

```ada
Value := Int_Result.Unwrap_Or (Result, Default_Value);
```

### Pattern 3: Transform and Chain

```ada
Final := Int_Result.Map (Result, Transform'Access)
  .And_Then (Validate'Access)
  .Or_Else (Recover'Access);
```

### Error Kinds

| Error Kind | Meaning |
|------------|---------|
| `Validation_Error` | Input failed validation |
| `Parse_Error` | Failed to parse input |
| `Not_Found_Error` | Resource not found |
| `IO_Error` | I/O operation failed |
| `Internal_Error` | Unexpected internal error |

### Why No Exceptions?

- **Explicit**: Error paths visible in type signatures
- **Composable**: Chain operations without try/catch blocks
- **SPARK Compatible**: Formal verification possible
- **Type Safe**: Compiler enforces handling

---

## Running Tests

```bash
# Run all tests
make test-all

# Run unit tests only
make test-unit

# Run SPARK verification
make spark-prove
```

**Expected Output:**
```
========================================
Test Summary: Functional.Result
========================================
Total tests: 84
Passed: 84
Failed: 0
========================================
All tests passed!
```

---

## Common Issues

### Q: "Cannot find package Functional"

**A:** Ensure you added the dependency:
```bash
alr with functional
```

### Q: "Type mismatch in Result instantiation"

**A:** Ensure your error type matches:
```ada
--  Error type must be definite
type Error_Kind is (Error_A, Error_B);  --  Good
type Error is ...;                       --  Must be definite
```

### Q: "SPARK verification fails"

**A:** Ensure you're only using SPARK-compatible operations in SPARK_Mode regions. Use `SPARK_Mode => Off` for infrastructure code.

### Q: "Map_To_Result requires Error_Kind_Type to be discrete"

**A:** The Error_Kind_Type must be an enumeration:
```ada
type Error_Kind is (IO_Error, Parse_Error);  --  Good (discrete)
type Error is record ... end record;          --  Won't work
```

---

## Next Steps

- **[User Guide](guides/user_guide.md)** - Complete API reference with examples
- **[Cheatsheet](guides/cheatsheet.md)** - Quick reference card
- **[Software Design Specification](formal/software_design_specification.md)** - Architecture deep dive
- **[Error Handling Strategy](common/guides/error_handling_strategy.md)** - Best practices

---

**License:** BSD-3-Clause<br>
**Copyright:** © 2025 Michael Gardner, A Bit of Help, Inc.
