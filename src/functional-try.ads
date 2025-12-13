pragma Ada_2022;
--  ===========================================================================
--  Functional.Try - Exception-to-Result/Option Bridges
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Bridges exception-based APIs to Result/Option-based error handling.
--    Use at I/O boundaries to convert exception-prone operations into
--    railway-oriented programming flow.
--
--  When to Use Try_To_Result (this package):
--    1:1 mapping - one anticipated exception to one domain error
--    - Single anticipated exception transforms to specific domain error
--    - General catch-all with procedural Map_Exception function
--    - Need to inspect Exception_Occurrence for custom error building
--
--  When to Use Map_To_Result (child package):
--    1..N:1..N mapping - multiple exceptions to multiple domain errors
--    - Declarative mapping table (data, not if/elsif chains)
--    - Empty mapping = catch-all (like Try_To_Result with generic error)
--    - 1 mapping = single exception discrimination
--    - N mappings = multiple exception discrimination
--    - RECOMMENDED for new code due to declarative, self-documenting nature
--
--  When to Use Map_To_Result_With_Param (child package):
--    Same as Map_To_Result but when action needs input context
--    - File paths, user IDs, configuration, etc.
--    - Supports indefinite types like String
--
--  Functions:
--    Try_To_Result                   - No-param bridge to any Result type
--    Try_To_Any_Result_With_Param    - Parameterized bridge to any Result type
--    Try_To_Functional_Result        - Convenience for Functional.Result
--    Try_To_Functional_Option        - Convenience for Functional.Option
--    Try_To_Result_With_Param        - Parameterized Functional.Result bridge
--    Try_To_Option_With_Param        - Parameterized Functional.Option bridge
--
--  Design Pattern:
--    Exception handling at boundaries only. Domain/Application layers use
--    Result/Option types throughout. Boundary adapters use Try to convert
--    exception-based external APIs into Result/Option types.
--
--  Dependencies:
--    Ada.Exceptions, Functional.Result, Functional.Option
--
--  ===========================================================================

with Ada.Exceptions;
with Functional.Result;
with Functional.Option;

--  SPARK_Mode => Off: This package is the exception boundary adapter.
--  It converts exception-based external APIs to Result/Option types.
--  By design, this is the ONLY place exceptions are handled in the library.
package Functional.Try
  with SPARK_Mode => Off
is

   --  ========================================================================
   --  Try_To_Result - General bridge to any Result type
   --  ========================================================================
   --  Use this when bridging to custom domain Result types or when you need
   --  full control over error mapping.

   generic
      type T is private;
      type E is private;
      type Result_Type is private;
      with function Ok (Value : T) return Result_Type is <>;
      with function New_Error (Error : E) return Result_Type is <>;
      with
        function Map_Exception
          (Occ : Ada.Exceptions.Exception_Occurrence) return E is <>;
      with function Action return T;
   function Try_To_Result return Result_Type;

   --  ========================================================================
   --  Try_To_Any_Result_With_Param - Parameterized bridge to any Result type
   --  ========================================================================
   --  Use when the action needs input context AND you're using a custom
   --  domain Result type (not Functional.Result). Combines the flexibility
   --  of Try_To_Result with parameter support.
   --  Supports indefinite types like String via type Param (<>) is private.

   generic
      type T is private;
      type E is private;
      type Param (<>) is private;
      type Result_Type is private;
      with function Ok (Value : T) return Result_Type is <>;
      with function New_Error (Error : E) return Result_Type is <>;
      with
        function Map_Exception
          (Occ : Ada.Exceptions.Exception_Occurrence) return E;
      with function Action (P : Param) return T;
   function Try_To_Any_Result_With_Param (P : Param) return Result_Type;

   --  ========================================================================
   --  Try_To_Functional_Result - Convenience for Functional.Result
   --  ========================================================================
   --  Thin wrapper around Try_To_Result for Functional.Result usage.
   --  Saves you from manually passing Ok/Err/Result_Type.

   generic
      type T is private;
      type E is private;
      with package Result_Pkg is new Functional.Result (T => T, E => E);
      with
        function Map_Exception
          (Occ : Ada.Exceptions.Exception_Occurrence) return E is <>;
      with function Action return T;
   function Try_To_Functional_Result return Result_Pkg.Result;

   --  ========================================================================
   --  Try_To_Functional_Option - Convenience for Functional.Option
   --  ========================================================================
   --  Wraps an action that may raise. Returns Some(value) on success,
   --  None on any exception. Use when exceptions represent absence,
   --  not detailed errors.

   generic
      type T is private;
      with package Option_Pkg is new Functional.Option (T => T);
      with function Action return T;
   function Try_To_Functional_Option return Option_Pkg.Option;

   --  ========================================================================
   --  Try_To_Result_With_Param - Parameterized Result bridge
   --  ========================================================================
   --  Use when the action needs input context (e.g., console message,
   --  file configuration, user ID, etc.).
   --  Supports indefinite types like String via type Param (<>) is private.

   generic
      type T is private;
      type E is private;
      type Param (<>) is private;
      with package Result_Pkg is new Functional.Result (T => T, E => E);
      with
        function Map_Exception
          (Occ : Ada.Exceptions.Exception_Occurrence) return E;
      with function Action (P : Param) return T;
   function Try_To_Result_With_Param (P : Param) return Result_Pkg.Result;

   --  ========================================================================
   --  Try_To_Option_With_Param - Parameterized Option bridge
   --  ========================================================================
   --  Use when the action needs input context and you only care about
   --  success/failure (not error details).
   --  Returns Some(value) on success, None on any exception.

   generic
      type T is private;
      type Param (<>) is private;
      with package Option_Pkg is new Functional.Option (T => T);
      with function Action (P : Param) return T;
   function Try_To_Option_With_Param (P : Param) return Option_Pkg.Option;

end Functional.Try;
