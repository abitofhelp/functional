# Functional Library User Guide

**Version:** 3.0.0
**Date:** December 06, 2025
**SPDX-License-Identifier:** BSD-3-Clause
**Copyright:** 2025 Michael Gardner, A Bit of Help, Inc.

This guide explains the design philosophy, architecture decisions, and best practices for using the Functional library effectively.

---

## Table of Contents

1. [Design Philosophy](#1-design-philosophy)
2. [Exception Boundary Pattern](#2-exception-boundary-pattern)
3. [SPARK Compatibility](#3-spark-compatibility)
4. [Embedded Systems Usage](#4-embedded-systems-usage)
5. [Best Practices](#5-best-practices)
6. [Common Pitfalls](#6-common-pitfalls)
7. [Integration Patterns](#7-integration-patterns)

---

## 1. Design Philosophy

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

## 2. Exception Boundary Pattern

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

## 3. SPARK Compatibility

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

---

## 4. Embedded Systems Usage

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

## 5. Best Practices

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

## 6. Common Pitfalls

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

## 7. Integration Patterns

### Layered Architecture

```
┌─────────────────────────────────────────────────────────────┐
│  Presentation Layer                                          │
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
