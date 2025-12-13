# Software Design Specification (SDS)

**Project:** Functional - Type-Safe Error Handling Library for Ada 2022
**Version:** 4.0.0
**Date:** December 12, 2025
**Author:** Michael Gardner, A Bit of Help, Inc.
**Status:** Released  

---

## 1. Introduction

### 1.1 Purpose

This Software Design Specification (SDS) describes the architectural and detailed design of the Functional library, a utility library providing type-safe functional programming abstractions for Ada 2022.

### 1.2 Scope

This document covers:
- Package organization and dependencies
- Type definitions and invariants
- Generic design patterns
- Operation semantics and contracts
- Memory model and performance considerations
- SPARK compatibility design decisions

**Note:** Functional is a **nonhybrid utility library** - it does not use hexagonal/clean architecture layers. It provides foundational types consumed by projects that do use layered architectures.

## 2. Architectural Design

### 2.1 Architecture Style

Functional follows a **flat package hierarchy** design:
- Single parent package (`Functional`) as namespace anchor
- Four primary child packages (`Result`, `Option`, `Either`, `Try`)
- Two backwards-compatibility child packages (`Try.To_Result`, `Try.To_Option`)
- One version package (`Version`)

```
Functional (Pure, namespace only)
├── Functional.Result      (Generic package, Preelaborate, SPARK_Mode => On)
├── Functional.Option      (Generic package, Preelaborate, SPARK_Mode => On)
├── Functional.Either      (Generic package, Preelaborate, SPARK_Mode => On)
├── Functional.Try         (Non-generic, SPARK_Mode => Off, exception boundary)
│   ├── Functional.Try.To_Result  (Backwards-compat wrapper)
│   └── Functional.Try.To_Option  (Backwards-compat wrapper)
└── Functional.Version     (Pure, version constants)
```

### 2.2 Design Principles

| Principle | Application |
|-----------|-------------|
| **Zero-Cost Abstraction** | All operations marked `Inline`; discriminated records have no overhead |
| **Explicit Error Handling** | Errors encoded in types, not hidden in exceptions |
| **Composition over Conditionals** | `And_Then`, `Map`, `Fallback` replace nested if/case |
| **Contract-First** | Pre/Post conditions document and enforce invariants |
| **Generic Reusability** | Package-level generics work with any `private` type |
| **SPARK Compatibility** | All core types are `SPARK_Mode => On` for formal verification |

### 2.3 Package Dependencies

```
                    ┌─────────────────┐
                    │   Functional    │ (Pure namespace)
                    └────────┬────────┘
           ┌─────────────────┼─────────────────┐
           │                 │                 │
    ┌──────┴──────┐   ┌──────┴──────┐   ┌──────┴──────┐
    │   Result    │   │   Option    │   │   Either    │
    │ SPARK: On   │   │ SPARK: On   │   │ SPARK: On   │
    └─────────────┘   └─────────────┘   └─────────────┘
           │                 │
           └────────┬────────┘
                    │
             ┌──────┴──────┐
             │    Try      │ ← with Ada.Exceptions
             │ SPARK: Off  │
             └──────┬──────┘
           ┌────────┴────────┐
    ┌──────┴──────┐   ┌──────┴──────┐
    │ To_Result   │   │  To_Option  │ (child packages)
    └─────────────┘   └─────────────┘
```

### 2.4 SPARK Design Decisions

The library is designed to be SPARK-compatible, enabling formal verification:

| Package | SPARK_Mode | Categorization |
|---------|------------|----------------|
| `Functional.Option` | On | Preelaborate |
| `Functional.Result` | On | Preelaborate |
| `Functional.Either` | On | Preelaborate |
| `Functional.Try` | Off | Exception boundary (by design) |
| `Functional.Version` | N/A | Pure |

#### 2.4.1 SPARK Trade-offs

Some operations are intentionally excluded to maintain SPARK compatibility:

| Excluded Operation | SPARK Prohibition | Alternative Pattern |
|-------------------|-------------------|---------------------|
| `Option.Replace` | Functions cannot have `in out` mode parameters | Use explicit assignment: `Old := Current; Current := New_Some(V);` |

**Rationale:**

SPARK prohibits functions with `out` or `in out` parameters because functions must be mathematically pure (no side effects). The `Replace` operation would require:

```ada
function Replace (O : in out Option; New_Value : T) return Option;
--  Returns old option, modifies O to hold new value
```

This violates the SPARK principle that functions only compute and return values.

**Design Decision:** Maintain `SPARK_Mode => On` for all core packages at the cost of excluding mutation-oriented operations. For mutation-heavy code, use explicit assignment sequences instead of functional-style Replace.

## 3. Detailed Design

### 3.1 Functional.Result

#### 3.1.1 Type Definition

```ada
generic
   type T is private;  -- Success value type
   type E is private;  -- Error value type
package Functional.Result
  with Preelaborate, SPARK_Mode => On
is
   type Result (Is_Ok : Boolean := True) is record
      case Is_Ok is
         when True  => Ok_Value    : T;
         when False => Error_Value : E;
      end case;
   end record;
```

**Design Rationale:**
- Discriminated record ensures only one value present at a time
- Boolean discriminant `Is_Ok` enables SPARK compatibility (vs enumeration)
- Default discriminant `True` allows uninitialized declarations (Ada requirement)
- `T` and `E` are separate types; caller decides error representation

#### 3.1.2 Invariants

| Invariant | Enforcement |
|-----------|-------------|
| `Value` only valid when `Is_Ok` | `Pre => R.Is_Ok` |
| `Error` only valid when not `Is_Ok` | `Pre => not R.Is_Ok` |
| `Map` preserves Error unchanged | Implementation + Postcondition |
| `And_Then` short-circuits on Error | Implementation + Postcondition |

#### 3.1.3 Operation Categories (36 operations)

| Category | Operations | Pattern |
|----------|------------|---------|
| Constructors | `Ok`, `New_Error`, `From_Error` | Create Result from value |
| Predicates | `Is_Ok`, `Is_Error`, `Is_Ok_And`, `Is_Error_And`, `Is_Ok_Or`, `Is_Error_Or`, `Contains` | Query state |
| Extractors | `Value`, `Error`, `Expect`, `Expect_Error`, `Unwrap_Error` | Extract with precondition |
| Defaults | `Unwrap_Or`, `Unwrap_Or_With` | Extract with fallback |
| Transforms | `Map`, `Map_Or`, `Map_Or_Else`, `And_Then`, `And_Then_Into`, `Map_Error`, `Bimap`, `Zip_With`, `Flatten`, `To_Option` | Value transformation |
| Recovery | `Fallback`, `Fallback_With`, `Recover`, `Recover_With` | Error recovery |
| Validation | `Ensure`, `With_Context` | Ok value validation |
| Side Effects | `Tap`, `Tap_Ok`, `Tap_Error` | Logging/debugging hooks |
| Operators | `"or"` (Unwrap_Or, Fallback), `"="` (Contains) | Operator aliases |

### 3.2 Functional.Option

#### 3.2.1 Type Definition

```ada
generic
   type T is private;
package Functional.Option
  with Preelaborate, SPARK_Mode => On
is
   type Option (Has_Value : Boolean := False) is record
      case Has_Value is
         when True  => Value : T;
         when False => null;
      end case;
   end record;
```

**Design Rationale:**
- Boolean discriminant `Has_Value` enables SPARK compatibility
- `Preelaborate` allows instantiation in preelaborable contexts (domain layers)
- Default `False` represents "no value" naturally
- Simpler than Result (no error type needed)

#### 3.2.2 Invariants

| Invariant | Enforcement |
|-----------|-------------|
| `Value` only valid when `Has_Value` | `Pre => O.Has_Value` |
| `Map` preserves None unchanged | Implementation + Postcondition |
| `Filter` converts Some to None on predicate failure | Implementation + Postcondition |

#### 3.2.3 Operation Categories (26 operations)

| Category | Operations | Pattern |
|----------|------------|---------|
| Constructors | `New_Some`, `None` | Create Option |
| Predicates | `Is_Some`, `Is_None`, `Is_Some_And`, `Is_None_Or`, `Contains` | Query state |
| Extractors | `Value`, `Expect` | Extract with precondition |
| Defaults | `Unwrap_Or`, `Unwrap_Or_With` | Extract with fallback |
| Transforms | `Map`, `Map_Or`, `Map_Or_Else`, `And_Then`, `Filter`, `Zip_With`, `Flatten` | Value transformation |
| Fallback | `Or_Else`, `Or_Else_With`, `Fallback` | Alternative on None |
| Side Effects | `Tap` | Logging/debugging hooks |
| Conversion | `Ok_Or`, `Ok_Or_Else` | Option to Result |
| Operators | `"or"`, `"and"`, `"xor"`, `"="` | Operator aliases |

### 3.3 Functional.Either

#### 3.3.1 Type Definition

```ada
generic
   type L is private;  -- Left type
   type R is private;  -- Right type
package Functional.Either
  with Preelaborate, SPARK_Mode => On
is
   type Either (Is_Left : Boolean := True) is record
      case Is_Left is
         when True  => Left_Value  : L;
         when False => Right_Value : R;
      end case;
   end record;
```

**Design Rationale:**
- Boolean discriminant `Is_Left` enables SPARK compatibility
- Symmetric union (neither side is "error" or "success")
- Useful for parsing, validation with multiple outcomes
- `Fold` reduces to single value via handler functions

#### 3.3.2 Operation Categories (20 operations)

| Category | Operations | Pattern |
|----------|------------|---------|
| Constructors | `Left`, `Right` | Create Either |
| Predicates | `Is_Left`, `Is_Right`, `Is_Left_And`, `Is_Right_And`, `Is_Left_Or`, `Is_Right_Or`, `Contains` | Query state |
| Extractors | `Left_Value`, `Right_Value`, `Get_Or_Else` | Extract with precondition |
| Transforms | `Map`, `Map_Left`, `Map_Right`, `Bimap`, `Swap`, `And_Then` | Value transformation |
| Reduction | `Fold`, `Merge` | Reduce to single value |
| Conversion | `To_Option`, `To_Result` | Either to Option/Result |
| Operators | `"="` (Contains) | Operator alias |

### 3.4 Functional.Try

#### 3.4.1 Design Pattern

Try bridges convert exception-based APIs to typed error handling:

```
Exception World          │         Result/Option World
                         │
  action() ──────────────┼──────────→ Result[T, E]
  raises Exception       │            or Option[T]
                         │
                    Try Bridge
```

#### 3.4.2 Generic Functions

| Function | Generics | Purpose |
|----------|----------|---------|
| `Try_To_Result` | `T`, `E`, `Result_Type`, `Ok`, `New_Error`, `Map_Exception`, `Action` | Full control over result type |
| `Try_To_Functional_Result` | `T`, `E`, `Result_Pkg`, `Map_Exception`, `Action` | Convenience for Functional.Result |
| `Try_To_Functional_Option` | `T`, `Option_Pkg`, `Action` | Convenience for Functional.Option |
| `Try_To_Result_With_Param` | `T`, `E`, `Param (<>)`, `Result_Pkg`, `Map_Exception`, `Action` | Parameterized Result bridge |
| `Try_To_Option_With_Param` | `T`, `Param (<>)`, `Option_Pkg`, `Action` | Parameterized Option bridge |

**Param (<>) Pattern:**
The `type Param (<>) is private` allows indefinite types like `String`:
```ada
function Try_Parse is new Try_To_Result_With_Param
  (T => Integer, E => Error, Param => String, ...);

Result := Try_Parse ("42");  -- String passed directly
```

## 4. Design Patterns

### 4.1 Railway-Oriented Programming

Operations compose along two tracks:

```
Ok Track:    Ok(v1) ──Map──→ Ok(v2) ──And_Then──→ Ok(v3) ──→ final Ok
                                          │
Error Track:                Error(e) ←────┘ (short-circuit)
```

**Implementation:** Each transform checks discriminant first:
```ada
function Map (R : Result) return Result is
begin
   if R.Is_Ok then
      return Ok (F (R.Ok_Value));
   else
      return R;  -- Pass through unchanged
   end if;
end Map;
```

### 4.2 Lazy Evaluation Pattern

`*_With` variants use generic function parameters for lazy evaluation:

```ada
generic
   with function F return T;
function Unwrap_Or_With (O : Option) return T;
```

The function `F` is only called if needed (Option is None).

### 4.3 Monadic Bind (And_Then)

The core composition operation chains fallible operations:

```ada
generic
   with function F (X : T) return Result;
function And_Then (R : Result) return Result;
```

**Semantics:**
- If `R` is Ok, apply `F` to the value (which may return Ok or Error)
- If `R` is Error, return Error unchanged (short-circuit)

### 4.4 Lenient vs Strict Predicates

Version 3.0.0 introduces predicate pairs for different use cases:

**Strict predicates** (`Is_Ok_And`, `Is_Some_And`, etc.):
- Return True only if the value exists AND the predicate holds
- Short-circuit: if no value, return False without calling predicate

**Lenient predicates** (`Is_Ok_Or`, `Is_None_Or`, etc.):
- Return True if no value OR the predicate holds on the value
- Useful for validation: "pass if empty or valid"

```ada
-- Strict: Must be Some AND positive
if Is_Some_And (Age, Is_Positive'Access) then ...

-- Lenient: Pass if None OR positive (optional field validation)
if Is_None_Or (Age, Is_Positive'Access) then ...
```

## 5. Data Flow

### 5.1 Result Composition Flow

```
Input ──→ Validate ──→ Transform ──→ Persist ──→ Output
           │             │             │
           ▼             ▼             ▼
        Result        Result        Result
         │               │             │
         └───And_Then────┴─And_Then───┘
                         │
                    Final Result
```

### 5.2 Error Propagation

Errors propagate automatically through `And_Then` chains:
1. First `Error` short-circuits the chain
2. Original error preserved (or enriched via `With_Context`)
3. `Recover` or `Recover_With` can intercept and handle

## 6. Memory Model

### 6.1 Stack-Based Types

All types are stack-allocated discriminated records:
- No dynamic allocation
- No access types or pointers
- Suitable for embedded/restricted runtimes

### 6.2 Size Considerations

Result and Either sizes depend on the larger of their type parameters:
```
Size(Result[T, E]) = max(Size(T), Size(E)) + discriminant overhead
```

For large types, consider using access types or limited types with explicit lifetime management.

## 7. Performance Design

### 7.1 Inline Optimization

All operations marked `with Inline`:
```ada
function Is_Ok (R : Result) return Boolean
with Inline;
```

Expected optimization: direct discriminant check, no function call overhead.

### 7.2 Zero-Cost Abstraction

Generated code should be equivalent to manual pattern matching:
```ada
-- This Result code:
Result := Chain (Validate (Transform (Parse (Input))));

-- Should compile to equivalent of:
if Parse_Result.Is_Ok then
   Transform_Result := Transform (Parse_Result.Ok_Value);
   if Transform_Result.Is_Ok then
      Result := Validate (Transform_Result.Ok_Value);
   else
      Result := Transform_Result;
   end if;
else
   Result := Parse_Result;
end if;
```

## 8. Testing Strategy

### 8.1 Unit Tests

Each package has dedicated test file:
| File | Target | Tests |
|------|--------|-------|
| `test_result.adb` | Functional.Result | 84 |
| `test_option.adb` | Functional.Option | 65 |
| `test_either.adb` | Functional.Either | 58 |
| `test_try.adb` | Functional.Try | 14 |
| `test_try_option.adb` | Try Option bridges | 6 |
| **Total** | | **227** |

### 8.2 Coverage Target

- Statement coverage: 90%+
- Decision coverage: 90%+
- Current: 95%+

## 9. Build Configuration

### 9.1 Library Modes

```ada
type Library_Type_Type is ("relocatable", "static", "static-pic");
Library_Type : Library_Type_Type := external ("LIBRARY_TYPE", "static");
for Library_Kind use Library_Type;
```

### 9.2 Build Profiles

| Profile | Optimization | Debug | Assertions |
|---------|--------------|-------|------------|
| development | -O0 | Full | Enabled |
| release | -O3 | None | Disabled |
| validation | -O1 | Full | Enabled |

## 10. Project Structure

```
functional/
├── alire.toml              # Alire manifest (v3.0.0)
├── functional.gpr          # GPRbuild project
├── src/
│   ├── functional.ads      # Pure namespace
│   ├── functional-result.ads/adb
│   ├── functional-option.ads/adb
│   ├── functional-either.ads/adb
│   ├── functional-try.ads/adb
│   ├── functional-try-to_result.ads/adb
│   ├── functional-try-to_option.ads/adb
│   └── version/
│       └── functional-version.ads  # Auto-generated
├── test/
│   ├── alire.toml          # Test crate (v3.0.0)
│   ├── unit/               # Unit tests (227 total)
│   └── common/             # Test framework
├── config/                 # Build configuration
└── docs/
    ├── formal/             # SRS, SDS, STG
    ├── guides/             # User guide, cheatsheet
    └── index.md            # Documentation hub
```

## 11. API Quick Reference

### 11.1 Operation Totals

| Package | Operations |
|---------|------------|
| Result | 36 |
| Option | 26 |
| Either | 20 |
| Try | 5 |
| **Total** | **87** |

### 11.2 SPARK Status

| Package | SPARK_Mode | Categorization |
|---------|------------|----------------|
| Option | On | Preelaborate |
| Result | On | Preelaborate |
| Either | On | Preelaborate |
| Try | Off | Exception boundary |
| Version | N/A | Pure |
