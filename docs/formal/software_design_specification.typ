// ============================================================================
// File: software_design_specification.typ
// Purpose: Software Design Specification for the Functional library.
// Scope: Project-specific SDS content plus invocation of shared formal-document
//   functionality from core.typ.
// Usage: This is an authoritative Typst source document. The generated PDF is
//   the distribution artifact.
// Modification Policy:
//   - Edit this file for project-specific SDS content.
//   - Keep shared presentation logic in core.typ.
// Table Ordering:
//   Sort any table whose rows a reader might scan to locate a specific
//   entry — definitions, acronyms, constraints, packages, interfaces,
//   and similar reference tables.  Sort alphabetically by the first
//   column.  Tables with an inherent sequence (requirement IDs within
//   a section, change history, workflow steps) retain their logical order.
// SPDX-License-Identifier: BSD-3-Clause
// ============================================================================

#import "core.typ": formal_doc

#let doc = (
  authors: ("Michael Gardner",),
  copyright: "© 2026 Michael Gardner, A Bit of Help, Inc.",
  license_file: "See the LICENSE file in the project root",
  project_name: "FUNCTIONAL",
  spdx_license: "BSD-3-Clause",
  status: "Released",
  status_date: "2026-04-27",
  title: "Software Design Specification",
  version: "4.1.0",
  applies_to: "^4.1",
)

#let profile = (
  app_role: none,
  assurance: "spark-targeted",
  deployment: "native",
  execution: "sequential",
  execution_environment: ("linux", "macos", "windows", "ada-runtime"),
  library_role: "utility",
  parallelism: "none",
  platform: ("desktop", "server", "embedded"),
  processor_architecture: ("amd64", "arm64", "arm32"),
  variant: "library",
)

#let change_history = (
  (
    version: "4.1.0",
    date: "2025-12-18",
    author: "Michael Gardner",
    changes: "Deprecate procedural Try functions; keep Option-based Try functions. Migrate to Typst format.",
  ),
  (
    version: "4.0.0",
    date: "2025-12-12",
    author: "Michael Gardner",
    changes: "Remove Expect functions; add Map_To_Result packages.",
  ),
  (
    version: "3.0.0",
    date: "2025-11-30",
    author: "Michael Gardner",
    changes: "Initial architecture with Result, Option, Either, Try, Scoped.",
  ),
)

#show: formal_doc.with(doc, profile, change_history)

= Introduction

== Purpose

This Software Design Specification (SDS) describes the architecture, package structure, type definitions, design patterns, and implementation decisions for the *Functional* library.

== Scope

This document covers:
- package hierarchy and dependency rules,
- SPARK boundary design,
- core type definitions and their rationale,
- design patterns employed (discriminated unions, generic instantiation, railway-oriented programming, declarative exception mapping, RAII guards),
- error handling strategy, and
- build configuration.

== References

- Software Requirements Specification (SRS).
- Ada 2022 Language Reference Manual (ISO/IEC 8652:2023).
- SPARK 2014 Reference Manual.

= Architectural Overview

== Architecture Style

*Functional* is a flat utility library, not a layered DDD/Clean/Hexagonal project. It is organized by module rather than by architectural layer. The primary structural concern is the *SPARK boundary*: core types (Result, Option, Either) are SPARK-compatible, while exception-handling infrastructure (Try, Scoped) is not.

#table(
  columns: (auto, 1fr, auto),
  table.header([*Module Family*], [*Purpose*], [*SPARK_Mode*]),
  [Result, Option, Either], [Pure functional types — discriminated unions with combinator operations.], [On],
  [Try], [Exception-to-Result/Option bridges for architectural boundaries.], [Off],
  [Scoped], [RAII guards using Ada.Finalization.], [Off],
  [Version], [Library version constants.], [On],
)

== Dependency Rules

// Sort rows alphabetically by the first column.
#table(
  columns: (auto, 1fr, auto),
  table.header([*Package*], [*Dependencies*], [*SPARK_Mode*]),
  [Functional], [None], [On],
  [Functional.Either], [Functional], [On],
  [Functional.Option], [Functional], [On],
  [Functional.Result], [Functional], [On],
  [Functional.Scoped], [Ada.Finalization], [Off],
  [Functional.Try], [Ada.Exceptions, Result, Option], [Off],
  [Functional.Try.Map_To_Result], [Try], [Off],
  [Functional.Try.Map_To_Result_With_Param], [Try], [Off],
  [Functional.Try.To_Option], [Try, Option], [Off],
  [Functional.Try.To_Result], [Try, Result], [Off],
  [Functional.Version], [None], [On],
)

== SPARK Boundary

- *SPARK_Mode On*: Result, Option, Either (pure functional types with no side effects).
- *SPARK_Mode Off*: Try (exception handling), Scoped (finalization).

This boundary ensures core types are formally verifiable while allowing exception handling at system boundaries.

= Package Structure

== Directory Layout

```text
src/
├── functional.ads                               # Root package
├── functional-result.ads / .adb                 # Result type
├── functional-option.ads / .adb                 # Option type
├── functional-either.ads / .adb                 # Either type
├── functional-try.ads / .adb                    # Try exception bridges
├── functional-try-map_to_result.ads / .adb      # Declarative mapping
├── functional-try-map_to_result_with_param.ads / .adb
├── functional-try-to_result.ads / .adb          # Procedural mapping
├── functional-try-to_option.ads / .adb          # Exception-to-Option
├── functional-scoped.ads / .adb                 # RAII guards
└── version/
    └── functional-version.ads                   # Version info
```

== Package Descriptions

// Sort rows alphabetically by the first column.
#table(
  columns: (auto, 1fr, auto),
  table.header([*Package*], [*Purpose*], [*SPARK*]),
  [`Functional`], [Root namespace, common declarations.], [On],
  [`Functional.Either`], [Left/right discriminated union with combinators.], [On],
  [`Functional.Option`], [Presence/absence discriminated union with combinators.], [On],
  [`Functional.Result`], [Success/failure discriminated union with combinators.], [On],
  [`Functional.Scoped`], [RAII guard generics.], [Off],
  [`Functional.Try`], [Exception-to-Result/Option bridge infrastructure.], [Off],
  [`Functional.Try.Map_To_Result`], [Declarative exception mapping (no extra parameter).], [Off],
  [`Functional.Try.Map_To_Result_With_Param`], [Declarative exception mapping (parameterized action).], [Off],
  [`Functional.Try.To_Option`], [Exception-to-Option for probe operations.], [Off],
  [`Functional.Try.To_Result`], [Procedural exception mapping (legacy).], [Off],
  [`Functional.Version`], [Library version constants.], [On],
)

= Type Definitions

== Result Type

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

*Design Rationale:*
- Discriminated union ensures exactly one of Value or Error is present.
- Default discriminant `False` allows uninitialized declarations.
- Generic parameters allow any success/error type pair.
- Zero heap allocation.

== Option Type

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

*Design Rationale:*
- Discriminated union eliminates null references.
- Preelaborate-safe for domain layer usage.
- Zero heap allocation.

== Either Type

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

*Design Rationale:*
- Neither side is designated as "error" (distinguishes Either from Result).
- Useful for union types, parsing, and validation branches.

= Design Patterns

== Discriminated Union Pattern

All core types use Ada discriminated records:
- Compile-time enforcement of valid access via discriminant checks.
- Zero heap allocation.
- SPARK compatible.

== Generic Instantiation Pattern

```ada
--  Consumer instantiates with their own types
package User_Result is new Functional.Result
  (T => User_Type, E => Error_Kind);
```

Benefits:
- Type safety (different instantiations cannot be mixed).
- Monomorphization (no runtime dispatch).
- Clear at call site what types are involved.

== Railway-Oriented Programming

Operations chain through Map / And_Then / Or_Else:

```ada
Result := User_Result.Ok (User)
  .Map (Validate'Access)
  .And_Then (Save'Access)
  .Or_Else (Log_And_Default'Access);
```

Error propagation is automatic — no explicit checking between operations.

== Declarative Exception Mapping

Map_To_Result uses data (mapping arrays) instead of procedural code:

```ada
Mappings : constant Try_Read.Mapping_Array :=
  [(Name_Error'Identity,  Not_Found),
   (Use_Error'Identity,   Permission_Error)];

Result := Try_Read.Run (Path, Mappings);
```

Benefits:
- Self-documenting (exception table is visible data).
- No procedural Map_Exception boilerplate.
- Easy to add or remove mappings.

== RAII Guard Pattern

Scoped uses Ada finalization for automatic cleanup:

```ada
declare
   Guard : File_Guard.Guard (File'Access);
begin
   --  Work with file
end;  --  Guard.Finalize automatically closes file
```

= Error Handling Strategy

== Core Principle

*Errors are values, not exceptions.*

Consumers of the Functional library use Result and Option types for all expected failure paths. Exceptions are caught at boundaries (the consuming project's infrastructure layer) using Try and converted to Result.

== Module Responsibilities

// Sort rows alphabetically by the first column.
#table(
  columns: (auto, 1fr),
  table.header([*Module*], [*Error Handling Role*]),
  [Either], [Carry left/right values through combinators.],
  [Option], [Carry present/absent values through combinators.],
  [Result], [Carry success/failure values through combinators.],
  [Scoped], [Suppress exceptions from cleanup actions to prevent double-exception scenarios.],
  [Try], [Catch exceptions at boundaries and convert to Result or Option.],
)

== Try Usage Guidelines

#table(
  columns: (auto, 1fr),
  table.header([*Scenario*], [*Use*]),
  [Multiple exception types to multiple errors], [Map_To_Result_With_Param],
  [Catch-all with default error], [Map_To_Result.Run_Catch_All],
  [Probe/check operations], [To_Option + Unwrap_Or],
  [Legacy procedural mapping], [To_Result],
)

= Concurrency Design

== Thread Safety

*Functional* is a sequential library. All types are value types (discriminated records) and can be safely used in concurrent contexts without synchronization, provided each task operates on its own instances.

== Future Concurrency Support

Concurrency is out of scope for the current design. The value-type nature of all core types means no design changes are anticipated for concurrent use.

= Performance and Memory Design

== Zero-Overhead Abstractions

- All core types are stack-allocated discriminated records.
- Static dispatch via generics (no tagged types, no dynamic dispatch).
- No runtime type information overhead.

== Memory Management

- No heap allocation in any module.
- All types are bounded by their discriminant and generic parameters.
- Suitable for embedded and memory-constrained environments.

= Build and Deployment

== Project Structure

#table(
  columns: (auto, 1fr),
  table.header([*Project*], [*Purpose*]),
  [`functional.gpr`], [Main library build.],
  [`test/functional_test.gpr`], [Test suite build.],
)

== Build Profiles

#table(
  columns: (auto, auto, auto, auto, 1fr),
  table.header([*Profile*], [*Optimization*], [*Assertions*], [*Debug*], [*Use Case*]),
  [Development], [-O0], [Enabled], [-g], [Daily development.],
  [Validation], [-O1], [Enabled], [-g], [Pre-release testing.],
  [Release], [-O2], [Disabled], [None], [Production.],
)

== Platform Support

The library supports all platforms and processor architectures listed in the project profile. No platform-specific code exists in the library.

= SPARK Readiness Assessment

== Overview

Functional targets a *spark-targeted* assurance posture. Core types (Result, Option, Either) are fully SPARK-compatible. Infrastructure modules (Try, Scoped) are outside SPARK due to their use of Ada.Exceptions and Ada.Finalization.

== Module Assessment

// Sort rows alphabetically by the first column.
#table(
  columns: (auto, auto, 1fr),
  table.header([*Module*], [*SPARK*], [*Notes*]),
  [Either], [On], [Pure functional type, fully provable.],
  [Option], [On], [Pure functional type, fully provable.],
  [Result], [On], [Pure functional type, fully provable.],
  [Scoped], [Off], [Ada.Finalization is outside SPARK.],
  [Try], [Off], [Exception handling is inherently outside SPARK.],
  [Version], [On], [Constants only.],
)

== SPARK-Compatible Components

All operations on Result, Option, and Either (constructors, discriminant checks, Map, And_Then, Or_Else, Unwrap_Or, Match, Fold) are SPARK-compatible and provable.

= Testing Strategy

== Test Organization

Tests are organized by module in `test/unit/` with a shared framework in `test/common/`. SPARK legality tests are in `test/spark/`.

== Testing Approach

- *Unit tests*: Exercise all public operations for each module.
- *SPARK tests*: Verify that core types can be instantiated under SPARK_Mode On.
- *Integration tests*: Verify Try with real exception scenarios.

= Design Decisions

== No Tagged Types in Core

*Decision:* Core types (Result, Option, Either) are not tagged.

*Rationale:*
- Static dispatch eliminates runtime overhead.
- SPARK compatibility (tagged types have limitations in SPARK).
- Generic instantiation provides the needed polymorphism.

== Deprecation of Procedural Try Functions

*Decision:* Deprecate Try_To_Result in favor of Map_To_Result.

*Rationale:*
- Declarative mappings are more maintainable than procedural Map_Exception functions.
- Exception tables are self-documenting.

*Exception:* Option-based Try functions (To_Option) are NOT deprecated — they serve valid use cases for probe operations.

== Exception Suppression in Scoped

*Decision:* Scoped guards catch and suppress exceptions from Release.

*Rationale:*
- Prevents double-exception scenarios.
- Ensures cleanup completes even if Release fails.
- Consistent with RAII semantics in other languages.
