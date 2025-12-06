# Software Design Specification (SDS)

**Project:** Functional - Type-Safe Error Handling Library for Ada 2022
**Version:** 2.3.0
**Date:** December 05, 2025
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
├── Functional.Result      (Generic package)
├── Functional.Option      (Generic package, Preelaborate)
├── Functional.Either      (Generic package)
├── Functional.Try         (Non-generic, contains generic functions)
│   ├── Functional.Try.To_Result  (Backwards-compat wrapper)
│   └── Functional.Try.To_Option  (Backwards-compat wrapper)
└── Functional.Version     (Version constants)
```

### 2.2 Design Principles

| Principle | Application |
|-----------|-------------|
| **Zero-Cost Abstraction** | All operations marked `Inline`; discriminated records have no overhead |
| **Explicit Error Handling** | Errors encoded in types, not hidden in exceptions |
| **Composition over Conditionals** | `And_Then`, `Map`, `Fallback` replace nested if/case |
| **Contract-First** | Pre/Post conditions document and enforce invariants |
| **Generic Reusability** | Package-level generics work with any `private` type |

### 2.3 Package Dependencies

```
                    ┌─────────────────┐
                    │   Functional    │ (Pure namespace)
                    └────────┬────────┘
           ┌─────────────────┼─────────────────┐
           │                 │                 │
    ┌──────┴──────┐   ┌──────┴──────┐   ┌──────┴──────┐
    │   Result    │   │   Option    │   │   Either    │
    └─────────────┘   └─────────────┘   └─────────────┘
           │                 │
           └────────┬────────┘
                    │
             ┌──────┴──────┐
             │    Try      │ ← with Ada.Exceptions
             └──────┬──────┘
           ┌────────┴────────┐
    ┌──────┴──────┐   ┌──────┴──────┐
    │ To_Result   │   │  To_Option  │ (child packages)
    └─────────────┘   └─────────────┘
```

## 3. Detailed Design

### 3.1 Functional.Result

#### 3.1.1 Type Definition

```ada
generic
   type T is private;  -- Success value type
   type E is private;  -- Error value type
package Functional.Result is

   type Result_Kind is (K_Ok, K_Err);

   type Result (Kind : Result_Kind := K_Ok) is record
      case Kind is
         when K_Ok  => Ok_Value  : T;
         when K_Err => Err_Value : E;
      end case;
   end record;
```

**Design Rationale:**
- Discriminated record ensures only one value present at a time
- Default discriminant `K_Ok` allows uninitialized declarations (Ada requirement)
- `T` and `E` are separate types; caller decides error representation

#### 3.1.2 Invariants

| Invariant | Enforcement |
|-----------|-------------|
| `Value` only valid when `Is_Ok` | `Pre => R.Kind = K_Ok` |
| `Error` only valid when `Is_Err` | `Pre => R.Kind = K_Err` |
| `Map` preserves Err unchanged | Implementation pattern |
| `And_Then` short-circuits on Err | Implementation pattern |

#### 3.1.3 Operation Categories

| Category | Operations | Pattern |
|----------|------------|---------|
| Constructors | `Ok`, `Err`, `From_Error` | Create Result from value |
| Predicates | `Is_Ok`, `Is_Err` | Query state without extraction |
| Extractors | `Value`, `Error`, `Expect` | Extract with precondition |
| Defaults | `Unwrap_Or`, `Unwrap_Or_With` | Extract with fallback |
| Transforms | `Map`, `And_Then`, `And_Then_Into`, `Map_Err`, `Bimap` | Value transformation |
| Recovery | `Fallback`, `Fallback_With`, `Recover`, `Recover_With` | Error recovery |
| Validation | `Ensure`, `With_Context` | Ok value validation |
| Side Effects | `Tap` | Logging/debugging hooks |

### 3.2 Functional.Option

#### 3.2.1 Type Definition

```ada
generic
   type T is private;
package Functional.Option with Preelaborate is

   type Option_Kind is (K_Some, K_None);

   type Option (Kind : Option_Kind := K_None) is record
      case Kind is
         when K_Some => Value : T;
         when K_None => null;
      end case;
   end record;
```

**Design Rationale:**
- `Preelaborate` allows instantiation in preelaborable contexts (domain layers)
- Default `K_None` represents "no value" naturally
- Simpler than Result (no error type needed)

#### 3.2.2 Invariants

| Invariant | Enforcement |
|-----------|-------------|
| `Value` only valid when `Is_Some` | `Pre => O.Kind = K_Some` |
| `Map` preserves None unchanged | Implementation pattern |
| `Filter` converts Some to None on predicate failure | Implementation pattern |

### 3.3 Functional.Either

#### 3.3.1 Type Definition

```ada
generic
   type L is private;  -- Left type
   type R is private;  -- Right type
package Functional.Either is

   type Either_Kind is (K_Left, K_Right);

   type Either (Kind : Either_Kind := K_Left) is record
      case Kind is
         when K_Left  => Left_Value  : L;
         when K_Right => Right_Value : R;
      end case;
   end record;
```

**Design Rationale:**
- Symmetric union (neither side is "error" or "success")
- Useful for parsing, validation with multiple outcomes
- `Fold` reduces to single value via handler functions

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
| `Try_To_Result` | `T`, `E`, `Result_Type`, `Ok`, `Err`, `Map_Exception`, `Action` | Full control over result type |
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
Err Track:                    Err(e) ←────┘ (short-circuit)
```

**Implementation:** Each transform checks `Kind` first:
```ada
function Map (R : Result) return Result is
begin
   case R.Kind is
      when K_Ok  => return Ok (F (R.Ok_Value));
      when K_Err => return R;  -- Pass through unchanged
   end case;
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
- If `R` is Ok, apply `F` to the value (which may return Ok or Err)
- If `R` is Err, return Err unchanged (short-circuit)

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
1. First `Err` short-circuits the chain
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
Result := Input.And_Then (Validate'Access).And_Then (Transform'Access);

-- Should compile to equivalent of:
if Input.Kind = K_Ok then
   Temp := Validate (Input.Ok_Value);
   if Temp.Kind = K_Ok then
      Result := Transform (Temp.Ok_Value);
   else
      Result := Temp;
   end if;
else
   Result := Input;
end if;
```

## 8. Testing Strategy

### 8.1 Unit Tests

Each package has dedicated test file:
| File | Target | Tests |
|------|--------|-------|
| `test_result.adb` | Functional.Result | 35 |
| `test_option.adb` | Functional.Option | 22 |
| `test_either.adb` | Functional.Either | 16 |
| `test_try.adb` | Functional.Try | 14 |
| `test_try_option.adb` | Try Option bridges | 6 |

### 8.2 Coverage Target

- Statement coverage: 90%+
- Decision coverage: 90%+
- Current: 95% (152/160 lines)

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
├── alire.toml              # Alire manifest
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
│   ├── unit/               # Unit tests
│   ├── common/             # Test framework
│   └── alire.toml          # Test crate
├── config/                 # Build configuration
└── docs/
    ├── formal/             # SRS, SDS, STG
    └── index.md            # Documentation hub
```
