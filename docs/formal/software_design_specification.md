# Software Design Specification

**Version:** 4.1.0<br>
**Date:** 2025-12-18<br>
**SPDX-License-Identifier:** BSD-3-Clause<br>
**License File:** See the LICENSE file in the project root<br>
**Copyright:** © 2025 Michael Gardner, A Bit of Help, Inc.<br>
**Status:** Released

---

## 1. Introduction

### 1.1 Purpose

This Software Design Specification (SDS) describes the architecture, design patterns, and implementation decisions for the Functional library.

### 1.2 Scope

This document covers:
- Package structure and dependencies
- Type definitions and their design rationale
- Design patterns employed
- Error handling strategy
- Build configuration

### 1.3 References

- [Software Requirements Specification](software_requirements_specification.md)
- [Software Test Guide](software_test_guide.md)
- Ada 2022 Language Reference Manual

---

## 2. Architectural Overview

### 2.1 Package Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                      Functional (Root)                       │
│                    [SPARK_Mode => On]                        │
├─────────────────────────────────────────────────────────────┤
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐         │
│  │   Result    │  │   Option    │  │   Either    │         │
│  │ SPARK: On   │  │ SPARK: On   │  │ SPARK: On   │         │
│  └─────────────┘  └─────────────┘  └─────────────┘         │
│                                                              │
│  ┌──────────────────────────────────────────────────────┐  │
│  │                        Try                            │  │
│  │                   [SPARK_Mode => Off]                 │  │
│  │  ┌────────────────┐  ┌─────────────────────────────┐ │  │
│  │  │ Map_To_Result  │  │ Map_To_Result_With_Param    │ │  │
│  │  └────────────────┘  └─────────────────────────────┘ │  │
│  │  ┌────────────────┐  ┌─────────────────────────────┐ │  │
│  │  │   To_Result    │  │        To_Option            │ │  │
│  │  └────────────────┘  └─────────────────────────────┘ │  │
│  └──────────────────────────────────────────────────────┘  │
│                                                              │
│  ┌─────────────────────────────────────────────────────┐   │
│  │                      Scoped                          │   │
│  │                  [SPARK_Mode => Off]                 │   │
│  └─────────────────────────────────────────────────────┘   │
└─────────────────────────────────────────────────────────────┘
```

### 2.2 Dependency Rules

| Package | Dependencies | SPARK_Mode |
|---------|--------------|------------|
| Functional | None | On |
| Functional.Result | Functional | On |
| Functional.Option | Functional | On |
| Functional.Either | Functional | On |
| Functional.Try | Ada.Exceptions, Result, Option | Off |
| Functional.Try.Map_To_Result | Try | Off |
| Functional.Try.Map_To_Result_With_Param | Try | Off |
| Functional.Try.To_Result | Try, Result | Off |
| Functional.Try.To_Option | Try, Option | Off |
| Functional.Scoped | Ada.Finalization | Off |

### 2.3 SPARK Boundary

- **SPARK_Mode => On**: Result, Option, Either (pure functional types)
- **SPARK_Mode => Off**: Try (exception handling), Scoped (finalization)

This boundary ensures core types are formally verifiable while allowing exception handling at system boundaries.

---

## 3. Package Structure

### 3.1 Directory Layout

```
src/
├── functional.ads                           # Root package
├── functional-result.ads                    # Result type
├── functional-result.adb
├── functional-option.ads                    # Option type
├── functional-option.adb
├── functional-either.ads                    # Either type
├── functional-either.adb
├── functional-try.ads                       # Try exception bridges
├── functional-try.adb
├── functional-try-map_to_result.ads         # Declarative mapping
├── functional-try-map_to_result.adb
├── functional-try-map_to_result_with_param.ads
├── functional-try-map_to_result_with_param.adb
├── functional-try-to_result.ads             # Child package wrapper
├── functional-try-to_result.adb
├── functional-try-to_option.ads             # Child package wrapper
├── functional-try-to_option.adb
├── functional-scoped.ads                    # RAII guards
├── functional-scoped.adb
└── version/
    └── functional-version.ads               # Version info
```

### 3.2 Package Descriptions

| Package | Purpose | SPARK_Mode |
|---------|---------|------------|
| `Functional` | Root namespace, common declarations | On |
| `Functional.Result` | Success/failure discriminated union | On |
| `Functional.Option` | Presence/absence discriminated union | On |
| `Functional.Either` | Left/right discriminated union | On |
| `Functional.Try` | Exception-to-Result/Option bridges | Off |
| `Functional.Try.Map_To_Result` | Declarative exception mapping | Off |
| `Functional.Try.Map_To_Result_With_Param` | Parameterized declarative mapping | Off |
| `Functional.Try.To_Result` | Procedural exception mapping | Off |
| `Functional.Try.To_Option` | Exception-to-Option for probes | Off |
| `Functional.Scoped` | RAII guard generics | Off |
| `Functional.Version` | Library version constants | On |

---

## 4. Type Definitions

### 4.1 Result Type

```ada
generic
   type T is private;
   type E is private;
package Functional.Result with SPARK_Mode => On is
   type Result (Is_Ok : Boolean := False) is record
      case Is_Ok is
         when True  => Value : T;
         when False => Error : E;
      end case;
   end record;
end Functional.Result;
```

**Design Rationale:**
- Discriminated union ensures exactly one of Value or Error
- Default discriminant `False` allows uninitialized declarations
- Generic parameters allow any success/error types

### 4.2 Option Type

```ada
generic
   type T is private;
package Functional.Option with SPARK_Mode => On is
   type Option (Is_Some : Boolean := False) is record
      case Is_Some is
         when True  => Value : T;
         when False => null;
      end case;
   end record;
end Functional.Option;
```

**Design Rationale:**
- Discriminated union eliminates null references
- Preelaborate pragma for domain layer usage
- No heap allocation

### 4.3 Either Type

```ada
generic
   type L is private;
   type R is private;
package Functional.Either with SPARK_Mode => On is
   type Either (Is_Left : Boolean := True) is record
      case Is_Left is
         when True  => Left_Value  : L;
         when False => Right_Value : R;
      end case;
   end record;
end Functional.Either;
```

**Design Rationale:**
- Neither side designated as "error" (unlike Result)
- Useful for union types, parsing, validation branches

---

## 5. Design Patterns

### 5.1 Discriminated Union Pattern

All core types use Ada discriminated records:
- Compile-time enforcement of valid access
- Zero heap allocation
- SPARK compatible

### 5.2 Generic Instantiation Pattern

```ada
--  User instantiates with their types
package User_Result is new Functional.Result
  (T => User_Type, E => Error_Kind);
```

Benefits:
- Type safety (can't mix different Result instantiations)
- Monomorphization (no runtime dispatch)
- Clear at call site what types are involved

### 5.3 Railway-Oriented Programming

Operations chain through Map/And_Then/Or_Else:

```ada
Result := User_Result.Ok (User)
  .Map (Validate'Access)
  .And_Then (Save'Access)
  .Or_Else (Log_And_Default'Access);
```

Error propagation is automatic - no explicit checking between operations.

### 5.4 Declarative Exception Mapping

Map_To_Result uses data (mapping arrays) instead of code (if/elsif):

```ada
Mappings : constant Try_Read.Mapping_Array :=
  [(Name_Error'Identity,  Not_Found),
   (Use_Error'Identity,   Permission_Error)];

Result := Try_Read.Run (Path, Mappings);
```

Benefits:
- Self-documenting (exception table is visible data)
- No procedural Map_Exception function to maintain
- Easy to add/remove mappings

### 5.5 RAII Guard Pattern

Scoped uses Ada finalization for automatic cleanup:

```ada
declare
   Guard : File_Guard.Guard (File'Access);
begin
   --  Work with file
end;  --  Guard.Finalize automatically closes file
```

---

## 6. Error Handling Strategy

### 6.1 Core Principle

**Errors are values, not exceptions.**

Domain and Application layers use Result/Option exclusively. Exceptions are caught at boundaries (Infrastructure layer) using Try and converted to Result.

### 6.2 Layer Responsibilities

| Layer | Error Handling |
|-------|----------------|
| Domain | Return Result/Option, never raise |
| Application | Propagate Result/Option, compose with And_Then |
| Infrastructure | Catch exceptions with Try, return Result |

### 6.3 Try Usage Guidelines

| Scenario | Use |
|----------|-----|
| Multiple exception types → multiple errors | Map_To_Result_With_Param |
| Catch-all with default error | Map_To_Result.Run_Catch_All |
| Probe/check operations | To_Option + Unwrap_Or |
| Legacy procedural mapping | To_Result |

---

## 7. Build Configuration

### 7.1 GPR Projects

| Project | Purpose |
|---------|---------|
| `functional.gpr` | Main library build |
| `test/functional_test.gpr` | Test suite build |

### 7.2 Build Profiles

| Profile | Optimization | Assertions | Debug | Use Case |
|---------|--------------|------------|-------|----------|
| Development | -O0 | Enabled | -g | Daily development |
| Validation | -O1 | Enabled | -g | Pre-release testing |
| Release | -O2 | Disabled | None | Production |

---

## 8. Design Decisions

### 8.1 No Tagged Types in Core

**Decision:** Core types (Result, Option, Either) are not tagged.

**Rationale:**
- Static dispatch eliminates runtime overhead
- SPARK compatibility (tagged types have limitations)
- Generic instantiation provides needed polymorphism

### 8.2 Deprecation of Procedural Try Functions

**Decision:** Deprecate Try_To_Result, Try_To_Functional_Result, etc. in favor of Map_To_Result.

**Rationale:**
- Declarative mappings are more maintainable
- Exception tables are self-documenting
- Reduces procedural Map_Exception boilerplate

**Exception:** Option-based Try functions (Try_To_Functional_Option, Try_To_Option_With_Param) are NOT deprecated - they serve valid use cases for probe operations.

### 8.3 Exception Suppression in Scoped

**Decision:** Scoped guards catch and suppress exceptions from Release.

**Rationale:**
- Prevents double-exception scenarios
- Ensures cleanup completes even if Release fails
- Consistent with RAII semantics in other languages

---

## 9. Appendices

### Appendix A: Package Dependency Graph

```
Functional (root)
├── Functional.Result
├── Functional.Option
├── Functional.Either
├── Functional.Try
│   ├── Functional.Try.Map_To_Result
│   ├── Functional.Try.Map_To_Result_With_Param
│   ├── Functional.Try.To_Result
│   └── Functional.Try.To_Option
├── Functional.Scoped
└── Functional.Version
```

### Appendix B: Change History

| Version | Date | Changes |
|---------|------|---------|
| 4.1.0 | 2025-12-18 | Deprecated procedural Try functions; kept Option functions |
| 4.0.0 | 2025-12-12 | Removed Expect functions; added Map_To_Result packages |
| 3.0.0 | 2025-11-30 | Initial architecture with Result, Option, Either, Try |

---

**Document Control:**
- Version: 4.1.0
- Last Updated: 2025-12-18
- Status: Released
