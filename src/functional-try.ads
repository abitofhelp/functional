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
--  Functions:
--    Try_To_Result                - No-param Result bridge
--    Try_To_Functional_Result     - Convenience wrapper for Functional.Result
--    Try_To_Functional_Option     - Convenience wrapper for Functional.Option
--    Try_To_Result_With_Param     - Parameterized Result bridge (NEW)
--    Try_To_Option_With_Param     - Parameterized Option bridge (NEW)
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

package Functional.Try is

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
      with function Err (Error : E) return Result_Type is <>;
      with
        function Map_Exception
          (Occ : Ada.Exceptions.Exception_Occurrence) return E is <>;
      with function Action return T;
   function Try_To_Result return Result_Type;

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
