pragma Ada_2022;
--  ===========================================================================
--  Functional.Try
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Try interface and type definitions.
--
--  Key Types:
--    T
--    E
--    Result_Type
--    T
--    E
--    ... and 1 more
--
--  Dependencies:
--    Functional.Result
--    Functional.Option
--    function Ok (Value : T) return Result_Type is <>
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

end Functional.Try;
