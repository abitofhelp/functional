# Functional Library Documentation

**Version:** 4.1.0<br>
**Date:** 2025-12-18<br>
**SPDX-License-Identifier:** BSD-3-Clause<br>
**License File:** See the LICENSE file in the project root<br>
**Copyright:** © 2025 Michael Gardner, A Bit of Help, Inc.<br>
**Status:** Released

---

## Overview

The Functional library provides type-safe functional programming abstractions for Ada 2022. It enables railway-oriented programming with explicit error handling through Result, Option, Either, and Try types - eliminating the need for exceptions in business logic.

**Key Capabilities:**

- **Result[T, E]** - Success/failure with typed errors
- **Option[T]** - Presence/absence without null
- **Either[L, R]** - Neutral disjunction (both sides valid)
- **Try** - Exception-to-Result/Option bridges at boundaries
- **Scoped** - RAII guards for automatic resource cleanup
- **SPARK Verified** - Domain/Application layers formally proved

---

## Quick Navigation

### Getting Started

- [Quick Start Guide](quick_start.md) - Installation, first program, basic usage
- [Build Profiles](common/guides/build_profiles.md) - Development, validation, release configurations

### Formal Documentation

- [Software Requirements Specification](formal/software_requirements_specification.md) - Functional and non-functional requirements
- [Software Design Specification](formal/software_design_specification.md) - Architecture, patterns, design decisions
- [Software Test Guide](formal/software_test_guide.md) - Test strategy, execution, writing tests

### Developer Guides

- [User Guide](guides/user_guide.md) - Comprehensive API usage guide
- [Cheatsheet](guides/cheatsheet.md) - Quick reference for all operations
- [Error Handling Strategy](common/guides/error_handling_strategy.md) - Result monad patterns

### Reference

- [CHANGELOG](../CHANGELOG.md) - Release history and version notes
- [README](../README.md) - Project overview

---

## Architecture

```
┌─────────────────────────────────────────────────────┐
│                    Your Application                  │
├─────────────────────────────────────────────────────┤
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐  │
│  │   Result    │  │   Option    │  │   Either    │  │
│  │   [T, E]    │  │     [T]     │  │   [L, R]    │  │
│  └─────────────┘  └─────────────┘  └─────────────┘  │
│  ┌─────────────────────────────────────────────────┐│
│  │                     Try                          ││
│  │  Map_To_Result | Map_To_Result_With_Param       ││
│  │  To_Result | To_Option (child packages)         ││
│  └─────────────────────────────────────────────────┘│
│  ┌─────────────────────────────────────────────────┐│
│  │                   Scoped                         ││
│  │  Guard_For | Conditional_Guard_For              ││
│  └─────────────────────────────────────────────────┘│
└─────────────────────────────────────────────────────┘
```

**Design Principles:**

- **No Exceptions in Domain Logic** - All errors are values
- **Type Safety** - Compiler enforces error handling
- **Composability** - Chain operations with Map, And_Then, Or_Else
- **SPARK Compatible** - Domain/Application layers provable
- **Zero Runtime Cost** - Static dispatch, no heap allocation

---

## API Summary

| Module | Operations | Purpose |
|--------|------------|---------|
| **Result** | Ok, Error, Is_Ok, Is_Error, Value, Get_Error, Map, Map_Error, And_Then, Or_Else, Recover, Unwrap_Or, Match | Success/failure with typed errors |
| **Option** | Some, None, Is_Some, Is_None, Value, Map, And_Then, Or_Else, Unwrap_Or, Match, Filter | Optional values without null |
| **Either** | Left, Right, Is_Left, Is_Right, Left_Value, Right_Value, Map_Left, Map_Right, Fold | Neutral disjunction |
| **Try** | Map_To_Result, Map_To_Result_With_Param, To_Result, To_Option | Exception boundaries |
| **Scoped** | Guard_For, Conditional_Guard_For | RAII resource cleanup |

---

## Platform Support

| Platform | Status | Notes |
|----------|--------|-------|
| Linux (x86_64) | Supported | Primary development platform |
| macOS (x86_64) | Supported | Tested with GNAT FSF |
| macOS (ARM64) | Supported | Apple Silicon via Rosetta or native |
| Windows (x86_64) | Supported | Tested with GNAT FSF |

---

## Quick Example

```ada
with Functional.Result;
with Functional.Option;

--  Define your error type
type Error_Kind is (Validation_Error, Not_Found);

--  Instantiate Result for your types
package User_Result is new Functional.Result
  (T => User_Type, E => Error_Kind);

--  Use railway-oriented programming
function Find_User (ID : User_ID) return User_Result.Result is
begin
   if not Valid_ID (ID) then
      return User_Result.Error (Validation_Error);
   end if;

   declare
      User : constant User_Type := Database.Lookup (ID);
   begin
      return User_Result.Ok (User);
   end;
end Find_User;

--  Chain operations
Result := Find_User (ID)
  .And_Then (Validate_User'Access)
  .Map (Format_User'Access);
```

---

## Need Help?

- **Getting Started** - See [Quick Start Guide](quick_start.md)
- **Running Tests** - See [Software Test Guide](formal/software_test_guide.md)
- **Error Handling** - See [Error Handling Strategy](common/guides/error_handling_strategy.md)
- **API Reference** - See [User Guide](guides/user_guide.md) and [Cheatsheet](guides/cheatsheet.md)

---

**License:** BSD-3-Clause<br>
**Copyright:** © 2025 Michael Gardner, A Bit of Help, Inc.
