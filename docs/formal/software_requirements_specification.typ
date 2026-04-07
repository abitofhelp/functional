// ============================================================================
// File: software_requirements_specification.typ
// Purpose: Software Requirements Specification for the Functional library.
// Scope: Project-specific SRS content plus invocation of shared formal-document
//   functionality from core.typ.
// Usage: This is an authoritative Typst source document. The generated PDF is
//   the distribution artifact.
// Modification Policy:
//   - Edit this file for project-specific SRS content.
//   - Keep shared presentation logic in core.typ.
// SPDX-License-Identifier: BSD-3-Clause
// ============================================================================

#import "core.typ": change_history_table, formal_doc

#let doc = (
  authors: ("Michael Gardner",),
  copyright: "© 2025 Michael Gardner, A Bit of Help, Inc.",
  license_file: "See the LICENSE file in the project root",
  project_name: "FUNCTIONAL",
  spdx_license: "BSD-3-Clause",
  status: "Released",
  status_date: "2025-12-18",
  title: "Software Requirements Specification",
  version: "4.1.0",
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
    changes: "Remove Expect functions; add Map_To_Result and Map_To_Result_With_Param packages.",
  ),
  (
    version: "3.0.0",
    date: "2025-11-30",
    author: "Michael Gardner",
    changes: "Initial architecture with Result, Option, Either, Try, and Scoped modules.",
  ),
)

#show: formal_doc.with(doc, profile, change_history)

= Introduction

== Purpose

This Software Requirements Specification (SRS) defines the functional and non-functional requirements for *Functional*, a type-safe functional programming library for Ada 2022.

== Scope

*Functional* provides:

- Result type for explicit success/failure handling.
- Option type for null-safe optional values.
- Either type for neutral disjunction without error semantics.
- Try bridges for declarative exception-to-Result/Option conversion.
- Scoped guards for RAII resource management.

== Definitions and Acronyms

#table(
  columns: (auto, 1fr),
  table.header([*Term*], [*Definition*]),
  [Result], [A discriminated union representing success (Ok) or failure (Error).],
  [Option], [A discriminated union representing presence (Some) or absence (None).],
  [Either], [A discriminated union representing one of two valid values (Left or Right).],
  [Railway-Oriented Programming], [A pattern where operations are chained, with errors automatically propagating through the pipeline.],
  [RAII], [Resource Acquisition Is Initialization — automatic cleanup pattern using Ada finalization.],
  [SPARK], [A formally verifiable subset of Ada.],
  [Discriminated Union], [Ada record type with a discriminant controlling the variant.],
  [Monad], [Abstraction for sequencing computations (Map, And_Then).],
  [Functor], [Abstraction for transforming wrapped values (Map).],
)

== References

- Ada 2022 Language Reference Manual (ISO/IEC 8652:2023).
- SPARK 2014 Reference Manual.

= Overall Description

== Product Perspective

*Functional* is a standalone Ada 2022 library designed to enable functional programming patterns in Ada applications. It integrates with the Ada ecosystem through Alire package management and is consumed by projects such as *Hybrid_Lib_Ada*, *Hybrid_App_Ada*, and all downstream Ada projects in this organization.

Unlike the typical hybrid DDD/Clean/Hexagonal project, Functional does not have a layered architecture. It is a flat utility library organized by module (Result, Option, Either, Try, Scoped), with a SPARK boundary separating pure functional types from exception-handling infrastructure.

#table(
  columns: (1fr,),
  align: center + horizon,
  inset: 10pt,
  stroke: 0.8pt,
  [
    *Consumer Application / Library* \
    `with Functional.Result;` \
    `with Functional.Try.Map_To_Result;`
  ],
  [*↓*],
  [
    *Functional Library* \
    Pure Types (SPARK On): Result, Option, Either \
    Exception Bridges (SPARK Off): Try, Scoped
  ],
)

== Product Features

1. *Result[T, E]*: Type-safe success/failure handling via discriminated union.
2. *Option[T]*: Null-safe optional values via discriminated union.
3. *Either[L, R]*: Neutral disjunction without error semantics.
4. *Try*: Declarative exception boundary adapters (Map_To_Result, Map_To_Result_With_Param, To_Result, To_Option).
5. *Scoped*: RAII guards for automatic resource cleanup using Ada finalization.

== User Classes

#table(
  columns: (auto, 1fr),
  table.header([*User Class*], [*Description*]),
  [Library Consumers], [Ada developers using Functional types in their projects.],
  [Domain Developers], [Developers implementing business logic with Result/Option.],
  [Infrastructure Developers], [Developers implementing I/O boundaries with Try bridges.],
)

== Operating Environment

#table(
  columns: (auto, 1fr),
  table.header([*Requirement*], [*Specification*]),
  [Platforms], [#profile.platform.join(", ")],
  [Execution Environment], [#profile.execution_environment.join(", ")],
  [Processor Architecture], [#profile.processor_architecture.join(", ")],
  [Ada Version], [Ada 2022],
  [Compiler], [GNAT FSF 14+ or GNAT Pro],
  [Package Manager], [Alire 2.0+],
  [SPARK Prover], [GNATprove 14+ (optional, for verification)],
)

== Constraints

#table(
  columns: (auto, 1fr),
  table.header([*Constraint*], [*Rationale*]),
  [Ada 2022], [Required for modern language features.],
  [No Heap Allocation], [Stack-based discriminated unions ensure embedded safety.],
  [SPARK Subset], [Core types must remain formally verifiable.],
  [No Runtime Dependencies], [Only the Ada standard library is permitted.],
)

= Interface Requirements

== User Interfaces

None. This is a library, not an application.

== Software Interfaces

#table(
  columns: (auto, 1fr),
  table.header([*Interface*], [*Description*]),
  [Ada.Exceptions], [Used by the Try module for exception identity and message extraction.],
  [Ada.Finalization], [Used by the Scoped module for RAII guard finalization.],
)

=== Ada API

```ada
with Functional.Result;
with Functional.Try.Map_To_Result_With_Param;

--  Instantiate with project-specific types
package My_Result is new Functional.Result (My_Type, Error_Kind);
```

== Hardware Interfaces

None. The library is hardware-agnostic.

= Functional Requirements

== Result Module (FR-R)

*Priority:* Must
*Description:* Provide a type-safe success/failure discriminated union.

#table(
  columns: (auto, 1fr, auto),
  table.header([*ID*], [*Requirement*], [*Priority*]),
  [FR-R01], [Provide Ok constructor to create success values.], [Must],
  [FR-R02], [Provide Error constructor to create failure values.], [Must],
  [FR-R03], [Provide Is_Ok/Is_Error discriminant checks.], [Must],
  [FR-R04], [Provide Value accessor with precondition Is_Ok.], [Must],
  [FR-R05], [Provide Get_Error accessor with precondition Is_Error.], [Must],
  [FR-R06], [Provide Map to transform success values.], [Must],
  [FR-R07], [Provide Map_Error to transform error values.], [Must],
  [FR-R08], [Provide And_Then for chaining fallible operations.], [Must],
  [FR-R09], [Provide Or_Else for error recovery.], [Must],
  [FR-R10], [Provide Unwrap_Or for default extraction.], [Must],
  [FR-R11], [Provide Match for pattern matching.], [Should],
  [FR-R12], [Provide Recover for error-to-success conversion.], [Should],
)

*Acceptance Criteria:*

- Result is exactly one of Ok(value) or Error(error).
- Value extraction is type-safe via discriminant checks.
- No exceptions are raised for expected error conditions.

== Option Module (FR-O)

*Priority:* Must
*Description:* Provide a null-safe optional value discriminated union.

#table(
  columns: (auto, 1fr, auto),
  table.header([*ID*], [*Requirement*], [*Priority*]),
  [FR-O01], [Provide Some constructor for present values.], [Must],
  [FR-O02], [Provide None constructor for absent values.], [Must],
  [FR-O03], [Provide Is_Some/Is_None discriminant checks.], [Must],
  [FR-O04], [Provide Value accessor with precondition Is_Some.], [Must],
  [FR-O05], [Provide Map to transform present values.], [Must],
  [FR-O06], [Provide And_Then for chaining optional operations.], [Must],
  [FR-O07], [Provide Or_Else for absence recovery.], [Must],
  [FR-O08], [Provide Unwrap_Or for default extraction.], [Must],
  [FR-O09], [Provide Filter to conditionally keep values.], [Should],
  [FR-O10], [Provide Match for pattern matching.], [Should],
)

*Acceptance Criteria:*

- Option eliminates null references.
- Absent values produce no runtime error on access through safe combinators.

== Either Module (FR-E)

*Priority:* Must
*Description:* Provide a neutral disjunction discriminated union.

#table(
  columns: (auto, 1fr, auto),
  table.header([*ID*], [*Requirement*], [*Priority*]),
  [FR-E01], [Provide Left constructor.], [Must],
  [FR-E02], [Provide Right constructor.], [Must],
  [FR-E03], [Provide Is_Left/Is_Right discriminant checks.], [Must],
  [FR-E04], [Provide Left_Value accessor with precondition Is_Left.], [Must],
  [FR-E05], [Provide Right_Value accessor with precondition Is_Right.], [Must],
  [FR-E06], [Provide Map_Left to transform left values.], [Must],
  [FR-E07], [Provide Map_Right to transform right values.], [Must],
  [FR-E08], [Provide Fold for consuming either side.], [Should],
)

*Acceptance Criteria:*

- Neither side is designated as "error" (distinguishes Either from Result).
- Useful for union types, parsing, and validation branches.

== Try Module (FR-T)

*Priority:* Must
*Description:* Provide declarative exception-to-Result/Option bridges for architectural boundaries.

#table(
  columns: (auto, 1fr, auto),
  table.header([*ID*], [*Requirement*], [*Priority*]),
  [FR-T01], [Provide Map_To_Result for declarative exception mapping.], [Must],
  [FR-T02], [Provide Map_To_Result_With_Param for parameterized actions.], [Must],
  [FR-T03], [Support empty mappings for catch-all behavior.], [Must],
  [FR-T04], [Support multiple exception-to-error mappings.], [Must],
  [FR-T05], [Provide To_Result child package for procedural mapping.], [Should],
  [FR-T06], [Provide To_Option child package for probe operations.], [Should],
  [FR-T07], [Include exception message in error construction.], [Must],
)

*Acceptance Criteria:*

- Exception mapping uses data (mapping arrays) rather than procedural code.
- All exceptions at boundaries are caught and converted to Result or Option values.
- No exceptions escape through the Try boundary.

== Scoped Module (FR-S)

*Priority:* Must
*Description:* Provide RAII guards for automatic resource cleanup.

#table(
  columns: (auto, 1fr, auto),
  table.header([*ID*], [*Requirement*], [*Priority*]),
  [FR-S01], [Provide Guard_For for unconditional RAII.], [Must],
  [FR-S02], [Provide Conditional_Guard_For for conditional RAII.], [Must],
  [FR-S03], [Catch and suppress exceptions from Release.], [Must],
  [FR-S04], [Support user-defined Release procedures.], [Must],
)

*Acceptance Criteria:*

- Cleanup always executes, even when the guarded block raises an exception.
- Exceptions from Release are suppressed to prevent double-exception scenarios.

= Quality and Cross-Cutting Requirements

== Performance (NFR-01)

#table(
  columns: (auto, 1fr),
  table.header([*ID*], [*Requirement*]),
  [NFR-01.1], [Zero heap allocation in core operations.],
  [NFR-01.2], [Use static dispatch (no tagged types in core).],
)

== Reliability (NFR-02)

#table(
  columns: (auto, 1fr),
  table.header([*ID*], [*Requirement*]),
  [NFR-02.1], [Handle all exceptions at Try boundaries without leaking.],
  [NFR-02.2], [Do not propagate exceptions from Scoped guards.],
)

== Portability (NFR-03)

#table(
  columns: (auto, 1fr),
  table.header([*ID*], [*Requirement*]),
  [NFR-03.1], [Compile on GNAT 14+ across Linux, macOS, and Windows.],
  [NFR-03.2], [Contain no platform-specific code in the library.],
)

== Maintainability (NFR-04)

#table(
  columns: (auto, 1fr),
  table.header([*ID*], [*Requirement*]),
  [NFR-04.1], [Achieve at least 90% test coverage.],
  [NFR-04.2], [Maintain zero compiler warnings.],
)

== Usability (NFR-05)

#table(
  columns: (auto, 1fr),
  table.header([*ID*], [*Requirement*]),
  [NFR-05.1], [Provide clear error messages from all operations.],
  [NFR-05.2], [Provide comprehensive documentation and usage examples.],
)

== Platform Abstraction (NFR-06)

#table(
  columns: (auto, 1fr),
  table.header([*ID*], [*Requirement*]),
  [NFR-06.1], [Isolate exception handling to the Try module.],
  [NFR-06.2], [Isolate finalization to the Scoped module.],
)

== SPARK Formal Verification (NFR-07)

#table(
  columns: (auto, 1fr),
  table.header([*ID*], [*Requirement*]),
  [NFR-07.1], [Verify core types (Result, Option, Either) with SPARK_Mode On.],
  [NFR-07.2], [Mark Try and Scoped with SPARK_Mode Off.],
  [NFR-07.3], [Provide instantiation tests for SPARK compatibility.],
)

#table(
  columns: (auto, auto, 1fr),
  table.header([*Module*], [*SPARK_Mode*], [*Rationale*]),
  [Result], [On], [Pure functional type, no side effects.],
  [Option], [On], [Pure functional type, no side effects.],
  [Either], [On], [Pure functional type, no side effects.],
  [Try], [Off], [Exception handling.],
  [Scoped], [Off], [Ada.Finalization.],
)

== Testability (NFR-08)

#table(
  columns: (auto, 1fr),
  table.header([*ID*], [*Requirement*]),
  [NFR-08.1], [Support unit testing of all public operations.],
  [NFR-08.2], [Provide SPARK instantiation tests for compatibility verification.],
)

= Design and Implementation Constraints

== System Requirements

=== Hardware Requirements

No specific hardware requirements. The library operates within standard Ada runtime constraints.

=== Software Requirements

#table(
  columns: (auto, 1fr),
  table.header([*Category*], [*Requirement*]),
  [Compiler], [GNAT FSF 14+ or GNAT Pro (Ada 2022 support).],
  [Package Manager], [Alire 2.0+.],
  [SPARK Prover], [GNATprove 14+ (optional, for formal verification).],
)

= Verification and Traceability

== Verification Methods

#table(
  columns: (auto, 1fr),
  table.header([*Method*], [*Description*]),
  [Unit Testing], [All public operations of Result, Option, Either, Try, Scoped.],
  [SPARK Proof], [Result, Option, Either core operations.],
  [Integration Testing], [Try with real exception scenarios.],
  [Code Review], [All changes reviewed before merge.],
)

== Traceability Matrix

#table(
  columns: (auto, 1fr, 1fr),
  table.header([*Requirement*], [*Module*], [*Test*]),
  [FR-R01..R12], [Functional.Result], [test_result.adb],
  [FR-O01..O10], [Functional.Option], [test_option.adb],
  [FR-E01..E08], [Functional.Either], [test_either.adb],
  [FR-T01..T07], [Functional.Try.\*], [test_try\*.adb],
  [FR-S01..S04], [Functional.Scoped], [test_scoped.adb],
  [NFR-07], [SPARK instantiations], [spark_instantiations.ads],
)
