# Functional Library User Guide

**Project:** Functional - Type-Safe Error Handling Library for Ada 2022
**Version:** 4.0.0  
**Date:** December 12, 2025  
**Author:** Michael Gardner, A Bit of Help, Inc.
**Status:** Released  

This guide explains the design philosophy, architecture decisions, and best practices for using the Functional library effectively.

---

## Table of Contents

1. [Design Philosophy](#design-philosophy)
2. [Exception Boundary Pattern](#exception-boundary-pattern)
3. [SPARK Compatibility](#spark-compatibility)
4. [Embedded Systems Usage](#embedded-systems-usage)
5. [Best Practices](#best-practices)
6. [Common Pitfalls](#common-pitfalls)
7. [Integration Patterns](#integration-patterns)

---

## Design Philosophy

### Railway-Oriented Programming

The Functional library implements **railway-oriented programming** (ROP), a pattern where operations form a "railway track" with two paths:

- **Success track**: Values flow through transformations
- **Error track**: Errors short-circuit and propagate unchanged

```
Input → [Parse] → [Validate] → [Transform] → [Save] → Output
           ↓          ↓            ↓           ↓
        Error ────────────────────────────────────→ Error
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
-- Implicit: caller doesn't know this can fail
function Parse (S : String) return Integer;

-- Explicit: failure is part of the contract
function Parse (S : String) return Int_Result.Result;
```

### Type Safety Through Discriminants

All types use Ada's **discriminated records** to encode state:

```ada
type Result (Is_Ok : Boolean := True) is record
   case Is_Ok is
      when True  => Ok_Value    : T;
      when False => Error_Value : E;
   end case;
end record;
```

This provides:
- Compile-time guarantees about which field is valid
- Runtime discriminant checks prevent invalid access
- SPARK-compatible representation

---

## Exception Boundary Pattern

### The Core Principle

**Exceptions should only be handled at system boundaries**, not scattered throughout application code.

```
┌─────────────────────────────────────────────────────────────┐
│  External APIs (Ada.Text_IO, GNAT.Sockets, third-party)     │
│  - May raise exceptions                                      │
└─────────────────────────────────────────────────────────────┘
                              ↓
┌─────────────────────────────────────────────────────────────┐
│  Functional.Try (SPARK_Mode => Off)                         │
│  - Catches ALL exceptions                                    │
│  - Converts to Result/Option                                 │
│  - ~5% of code, manually audited                             │
└─────────────────────────────────────────────────────────────┘
                              ↓
┌─────────────────────────────────────────────────────────────┐
│  Application Code using Result/Option/Either                 │
│  - SPARK_Mode => On (formally verifiable)                    │
│  - No exception handling needed                              │
│  - ~95% of code, provably correct                            │
└─────────────────────────────────────────────────────────────┘
```

### Why This Design?

1. **SPARK Compatibility**: Exception handlers are prohibited in SPARK. By isolating them to Try, the rest of the library is formally verifiable.

2. **Predictable Control Flow**: Result types make error paths explicit and visible.

3. **Composability**: Result operations chain naturally; exceptions break composition.

4. **Testability**: Pure functions with Result returns are easier to test than exception-throwing code.

### The Try Module

`Functional.Try` is the **only** package with `SPARK_Mode => Off`. It provides five functions to bridge exception-based APIs:

```ada
-- No parameters
function Try_To_Result return Result_Type;
function Try_To_Functional_Result return Result_Pkg.Result;
function Try_To_Functional_Option return Option_Pkg.Option;

-- With parameters (supports indefinite types like String)
function Try_To_Result_With_Param (P : Param) return Result_Pkg.Result;
function Try_To_Option_With_Param (P : Param) return Option_Pkg.Option;
```

### Correct Usage Pattern

```ada
-- WRONG: Exception can escape from Map
function Parse (S : String) return Integer;  -- May raise!
function Do_Parse is new Int_Result.Map (F => Parse);  -- Exception escapes!

-- CORRECT: Wrap with Try FIRST
function Safe_Parse is new Functional.Try.Try_To_Functional_Result
  (T => Integer, E => Error, Result_Pkg => Int_Result,
   Map_Exception => To_Error, Action => Parse);

-- Now use Result operations safely
R := Safe_Parse;  -- Never raises, returns Result
R := Transform (R);  -- Safe, R is already a Result
```

### User-Provided Functions

Generic formal functions passed to `Map`, `And_Then`, etc. **must not raise exceptions**:

```ada
-- Map calls F(value) - if F raises, exception propagates uncaught
generic
   with function F (X : T) return T;  -- Must not raise!
function Map (R : Result) return Result;
```

This is by design:
- `SPARK_Mode => On` prohibits exception handlers
- If your function can raise, wrap it with Try first
- Exceptions in user callbacks indicate bugs, not expected errors

---

## SPARK Compatibility

### What is SPARK?

SPARK is a subset of Ada designed for **formal verification**. SPARK code can be mathematically proven correct at compile-time, not just tested at runtime.

### Library SPARK Status

| Package | SPARK_Mode | Rationale |
|---------|------------|-----------|
| `Functional.Option` | On | Pure transformations, no exceptions |
| `Functional.Result` | On | Pure transformations, no exceptions |
| `Functional.Either` | On | Pure transformations, no exceptions |
| `Functional.Try` | Off | Exception boundary (by design) |
| `Functional.Version` | N/A | Pure constants only |

### Postconditions for Provers

All transform operations include postconditions to help SPARK provers reason about code:

```ada
function Map (R : Result) return Result
with
  Post => (if R.Is_Ok then Map'Result.Is_Ok
           else not Map'Result.Is_Ok);
```

These contracts guarantee:
- `Map` preserves Ok/Error status
- `And_Then` short-circuits on Error
- `Filter` short-circuits on None
- Transform operations maintain type invariants

### Preconditions Prevent Invalid Access

Extractors have preconditions that prevent discriminant check failures:

```ada
function Value (R : Result) return T
with Pre => R.Is_Ok;  -- Caller must verify first

function Error (R : Result) return E
with Pre => not R.Is_Ok;  -- Caller must verify first
```

SPARK provers ensure these preconditions are satisfied at every call site.

### Using in Safety-Critical Projects

For DO-178C, ISO 26262, or IEC 61508 projects:

1. **Proven Code**: Option, Result, Either are formally verifiable
2. **Trusted Boundary**: Try module requires manual audit (~5% of complexity)
3. **Traceability**: Postconditions document behavioral guarantees
4. **Certification**: SPARK proofs provide evidence for certification

### SPARK Design Trade-offs

Some operations common in Rust/Haskell are intentionally excluded to maintain SPARK compatibility:

| Excluded Operation | Reason | Alternative |
|-------------------|--------|-------------|
| `Option.Replace` | Functions cannot have `in out` parameters in SPARK | Use explicit assignment and save old value first |

**Replace Example (excluded)**:
```ada
-- This would require:
function Replace (O : in out Option; New_Value : T) return Option;
-- SPARK prohibition: functions must be side-effect free
```

**Alternative Pattern**:
```ada
-- Pure SPARK-compatible approach
Old_Option : constant Option := Current_Option;
Current_Option := New_Some (New_Value);
-- Now Old_Option holds the previous value
```

This design choice prioritizes:
1. **Formal verification** over convenience for mutation patterns
2. **Mathematical purity** of functions (no side effects)
3. **Compile-time provability** of correctness properties

For mutation-heavy code, consider using procedures with explicit `out` parameters instead of functional-style operations.

---

## Embedded Systems Usage

### Library Characteristics

| Property | Status | Benefit |
|----------|--------|---------|
| Preelaborate/Pure | All packages | Static elaboration, predictable startup |
| Zero heap allocation | All types | No malloc/free, deterministic memory |
| No controlled types | All types | No finalization overhead |
| No tasking | All packages | Ravenscar compatible |
| Bounded memory | Discriminated records | Fixed-size, stack-allocated |

### Memory Model

All types are **stack-allocated discriminated records**:

```ada
-- Option[Integer] is approximately:
-- 1 byte discriminant + 4 bytes value = 5 bytes (+ alignment)

-- Result[Integer, Error] is approximately:
-- 1 byte discriminant + max(4 bytes, sizeof(Error)) = varies
```

No heap allocation ever occurs within the library.

### Ravenscar Profile Compatibility

The library is compatible with `pragma Profile (Ravenscar)`:

- No dynamic task creation
- No unprotected shared data
- No blocking operations in protected objects
- Deterministic execution

### Resource-Constrained Environments

For systems with limited resources:

1. **Stack Analysis**: All operations have bounded stack usage
2. **No Recursion**: All operations are iterative
3. **Deterministic**: No unbounded loops or allocations
4. **Code Size**: Generic instantiation controls code footprint

---

## Best Practices

### Choosing the Right Type

| Scenario | Type | Example |
|----------|------|---------|
| Operation can fail with error info | `Result[T, E]` | Parsing, I/O, validation |
| Value may be absent (not an error) | `Option[T]` | Config lookup, search |
| Two valid alternatives | `Either[L, R]` | Parse number or keep string |
| Exception-based API at boundary | `Try` | Ada.Text_IO, network |

### Error Type Design

Design error types with bounded strings for embedded compatibility:

```ada
-- Good: Bounded, SPARK-compatible
type Error_Kind is (IO_Error, Parse_Error, Validation_Error);
type Error is record
   Kind    : Error_Kind;
   Message : String (1 .. 100);
   Length  : Natural range 0 .. 100;
end record;

-- Avoid: Unbounded, not SPARK-compatible
type Error is record
   Message : Ada.Strings.Unbounded.Unbounded_String;
end record;
```

### Effective Chaining

Use `And_Then` for operations that can fail:

```ada
-- Each step can fail, errors short-circuit
Result :=
  And_Then_Validate (
    And_Then_Parse (
      Load_File (Path)));

-- Or with operator syntax
Config := Load_Primary or Load_Backup or Default_Config;
```

### Error Context Breadcrumbs

Add context at each layer for debugging:

```ada
function Add_Context (E : Error; Msg : String) return Error is ...;
function With_Context is new Result.With_Context (Append => Add_Context);

-- Each layer adds context
R := With_Context (Inner_Operation, "in Process_Order");
R := With_Context (R, "for customer " & Customer_ID);
-- Error: "File not found :: in Process_Order :: for customer 12345"
```

---

## Common Pitfalls

### Pitfall 1: Passing Exception-Raising Functions

```ada
-- WRONG: Integer'Value can raise Constraint_Error
function Parse (S : String) return Integer is (Integer'Value (S));
function Do_Parse is new Int_Result.Map (F => Parse);  -- Exception escapes!

-- CORRECT: Wrap with Try first
function Safe_Parse is new Try.Try_To_Functional_Result (...);
```

### Pitfall 2: Forgetting to Check Before Extract

```ada
-- WRONG: May raise discriminant check error
Value := Int_Result.Value (R);  -- What if R is Error?

-- CORRECT: Check first (or use Unwrap_Or)
if Int_Result.Is_Ok (R) then
   Value := Int_Result.Value (R);
end if;

-- BETTER: Use Unwrap_Or for default
Value := Int_Result.Unwrap_Or (R, 0);
-- Or with operator
Value := R or 0;
```

### Pitfall 3: Using Either When Result is Appropriate

```ada
-- WRONG: Using Either for error handling
package Parse_Either is new Either (L => String, R => Integer);
-- Semantically unclear: is Left the error or the alternative?

-- CORRECT: Use Result for errors
package Parse_Result is new Result (T => Integer, E => Parse_Error);
-- Clear: Ok is success, Error is failure
```

### Pitfall 4: Ignoring Postconditions

```ada
-- Postcondition tells you: if input was Error, output is Error
function Map (R : Result) return Result
with Post => (if not R.Is_Ok then not Map'Result.Is_Ok);

-- You can rely on this guarantee without checking
R := Map (Input);
-- If Input was Error, R is definitely Error (no need to re-check)
```

---

## Integration Patterns

### Layered Architecture

```
┌─────────────────────────────────────────────────────────────┐
│  API Layer (Facade)                                          │
│  - Receives Result/Option from Application                   │
│  - Converts to HTTP responses, CLI output, etc.              │
└─────────────────────────────────────────────────────────────┘
                              ↓
┌─────────────────────────────────────────────────────────────┐
│  Application Layer                                           │
│  - Orchestrates domain operations                            │
│  - All operations return Result/Option                       │
│  - No exception handling here                                │
└─────────────────────────────────────────────────────────────┘
                              ↓
┌─────────────────────────────────────────────────────────────┐
│  Domain Layer                                                │
│  - Pure business logic                                       │
│  - Result/Option for all fallible operations                 │
│  - SPARK_Mode => On (100% provable)                          │
└─────────────────────────────────────────────────────────────┘
                              ↓
┌─────────────────────────────────────────────────────────────┐
│  Infrastructure Layer                                        │
│  - Adapters for external systems                             │
│  - Try wrappers at I/O boundaries                            │
│  - Converts exceptions to Result/Option                      │
└─────────────────────────────────────────────────────────────┘
```

### Repository Pattern with Result

```ada
package User_Repository is
   function Find_By_ID (ID : User_ID) return User_Result.Result;
   function Save (User : User_Type) return Unit_Result.Result;
   function Delete (ID : User_ID) return Unit_Result.Result;
end User_Repository;

-- Implementation wraps database exceptions
function Find_By_ID (ID : User_ID) return User_Result.Result is
   function Do_Query return User_Type is ...;  -- May raise
   function Safe_Query is new Try.Try_To_Functional_Result
     (T => User_Type, E => DB_Error, Result_Pkg => User_Result,
      Map_Exception => To_DB_Error, Action => Do_Query);
begin
   return Safe_Query;
end Find_By_ID;
```

### Combining with Ada.Containers

```ada
-- Process a list, collecting all errors
function Process_All (Items : Item_Vector) return Results_Vector is
   Results : Results_Vector;
begin
   for Item of Items loop
      Results.Append (Process_Item (Item));
   end loop;
   return Results;
end Process_All;

-- Filter to only successful results
function Successes_Only (Results : Results_Vector) return Item_Vector is
   Items : Item_Vector;
begin
   for R of Results loop
      if Item_Result.Is_Ok (R) then
         Items.Append (Item_Result.Value (R));
      end if;
   end loop;
   return Items;
end Successes_Only;
```

---

## Migration from v2.x to v3.0.0

### Overview of Breaking Changes

Version 3.0.0 introduces several breaking changes to improve clarity, SPARK compatibility, and API consistency. This section provides comprehensive migration guidance.

### 8.1 Discriminant Changes

**What Changed**: Enumeration discriminants replaced with Boolean discriminants for SPARK compatibility and clarity.

| Type | Old (2.x) | New (3.0.0) |
|------|-----------|-------------|
| Option | `Kind : Kind_Type` | `Has_Value : Boolean` |
| Result | `Kind : Kind_Type` | `Is_Ok : Boolean` |
| Either | `Kind : Kind_Type` | `Is_Left : Boolean` |

**Migration Pattern**:

```ada
-- Old (2.x)
if O.Kind = K_Some then ...
if R.Kind = K_Ok then ...
if E.Kind = K_Left then ...

-- New (3.0.0)
if O.Has_Value then ...
if R.Is_Ok then ...
if E.Is_Left then ...
```

**Search/Replace Patterns**:
```
.Kind = K_Some    →  .Has_Value
.Kind = K_None    →  not .Has_Value
.Kind = K_Ok      →  .Is_Ok
.Kind = K_Err     →  not .Is_Ok
.Kind = K_Left    →  .Is_Left
.Kind = K_Right   →  not .Is_Left
```

### 8.2 Result API Renames

**What Changed**: Error-related names standardized for clarity.

| Category | Old (2.x) | New (3.0.0) |
|----------|-----------|-------------|
| Constructor | `Err(e)` | `New_Error(e)` |
| Predicate | `Is_Err(r)` | `Is_Error(r)` |
| Transform | `Map_Err(r)` | `Map_Error(r)` |
| Field access | `Err_Value` | `Error_Value` |

**Migration Pattern**:

```ada
-- Old (2.x)
R := Str_Result.Err (Parse_Error);
if Str_Result.Is_Err (R) then
   Put_Line (Exception_Message (R.Err_Value.Ex));
end if;
function Transform is new Str_Result.Map_Err (F => Add_Context);

-- New (3.0.0)
R := Str_Result.New_Error (Parse_Error);
if Str_Result.Is_Error (R) then
   Put_Line (Exception_Message (R.Error_Value.Ex));
end if;
function Transform is new Str_Result.Map_Error (F => Add_Context);
```

**Search/Replace Patterns**:
```
\.Err (          →  .New_Error (
\.Is_Err (       →  .Is_Error (
\.Map_Err (      →  .Map_Error (
\.Err_Value      →  .Error_Value
```

### 8.3 Try Module Changes

**What Changed**: Generic formal parameter renamed.

```ada
-- Old (2.x)
function Try_Read is new Functional.Try.Try_To_Result
  (..., Err => Domain_Result.From_Error, ...);

-- New (3.0.0)
function Try_Read is new Functional.Try.Try_To_Result
  (..., New_Error => Domain_Result.From_Error, ...);
```

### 8.4 New Operators (Non-Breaking)

Version 3.0.0 adds operator aliases that don't require migration but offer cleaner syntax:

```ada
-- Result operators
Val := R or Default;           -- Unwrap_Or
R := Primary or Backup;        -- Fallback

-- Option operators
Val := O or Default;           -- Unwrap_Or
O := Primary or Backup;        -- Or_Else
O := A and B;                  -- Returns B when both have values
O := A xor B;                  -- Returns one when exactly one has value
```

### 8.5 New Operations (Non-Breaking)

**Result** (7 new):
- `Is_Ok_Or` - Error or predicate holds on Ok value (lenient)
- `Is_Error_Or` - Ok or predicate holds on Error value (lenient)
- `Zip_With` - Combine two Results with a function
- `Flatten` - Unwrap nested `Result[Result[T,E],E]`
- `To_Option` - Convert `Ok(v)` to `Some(v)`, `Error(_)` to `None`

**Option** (9 new):
- `Is_None_Or` - None or predicate holds on Some value (lenient)
- `Zip_With` - Combine two Options with a function
- `Flatten` - Unwrap nested `Option[Option[T]]`
- `Ok_Or` - Convert to Result with eager error
- `Ok_Or_Else` - Convert to Result with lazy error

**Either** (7 new):
- `Is_Left_And` - Left and predicate holds on Left value (strict)
- `Is_Right_And` - Right and predicate holds on Right value (strict)
- `Is_Left_Or` - Right or predicate holds on Left value (lenient)
- `Is_Right_Or` - Left or predicate holds on Right value (lenient)
- `Map` - Right-biased transform (convenience)
- `Swap` - Exchange Left and Right
- `And_Then` - Right-biased monadic bind

### 8.6 Migration Checklist

Use this checklist to verify complete migration:

- [ ] **Discriminant access**: Search for `.Kind =` patterns
- [ ] **Result constructor**: Search for `\.Err (` (with backslash escape)
- [ ] **Result predicate**: Search for `Is_Err`
- [ ] **Result transform**: Search for `Map_Err`
- [ ] **Result field**: Search for `Err_Value`
- [ ] **Try generics**: Search for `Err =>` in Try instantiations
- [ ] **Compile test**: Build should succeed with no errors
- [ ] **Run tests**: All existing tests should pass

### 8.7 Automated Migration Script

For large codebases, consider this sed-based approach:

```bash
#!/bin/bash
# migrate_functional_3.sh - Migrate from v2.x to v3.0.0

find . -name "*.ads" -o -name "*.adb" | while read file; do
  sed -i '' \
    -e 's/\.Kind = K_Some/.Has_Value/g' \
    -e 's/\.Kind = K_None/not .Has_Value/g' \
    -e 's/\.Kind = K_Ok/.Is_Ok/g' \
    -e 's/\.Kind = K_Err/not .Is_Ok/g' \
    -e 's/\.Kind = K_Left/.Is_Left/g' \
    -e 's/\.Kind = K_Right/not .Is_Left/g' \
    -e 's/\.Err (/.New_Error (/g' \
    -e 's/\.Is_Err (/.Is_Error (/g' \
    -e 's/\.Map_Err (/.Map_Error (/g' \
    -e 's/\.Err_Value/.Error_Value/g' \
    -e 's/Err => /New_Error => /g' \
    "$file"
done
```

**Note**: Always review changes after running automated migration.

### 8.8 Testing Migration Completeness

After migration, verify with:

```bash
# Compile check
alr build 2>&1 | grep -i "error\|warning"

# Run tests
alr run test_runner

# Search for remaining old patterns
grep -r "\.Kind = K_" src/
grep -r "\.Err (" src/
grep -r "Is_Err" src/
grep -r "Map_Err" src/
grep -r "Err_Value" src/
```

If any patterns remain, they indicate incomplete migration.

---

## Summary

The Functional library provides:

1. **Type-safe error handling** through Result, Option, and Either
2. **SPARK compatibility** for formal verification
3. **Embedded readiness** with zero heap allocation
4. **Clean exception boundaries** via the Try module
5. **Composable operations** for railway-oriented programming

Follow these principles:
- Wrap exceptions with Try at boundaries
- Use Result for errors, Option for absence
- Check before extracting, or use Unwrap_Or
- Let postconditions guide your reasoning
- Keep domain logic pure and provable

---

## See Also

- **[Cheatsheet](cheatsheet.md)** - Quick reference for all operations
- **[Quick Start Guide](../quick_start.md)** - Get started in minutes
- **[CHANGELOG](../../CHANGELOG.md)** - Version history
