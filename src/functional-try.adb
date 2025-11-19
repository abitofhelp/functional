pragma Ada_2022;
--  ===========================================================================
--  Functional.Try - Implementation
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Implements exception-to-Result/Option conversion bridges.
--
--  Implementation Notes:
--    - All variants follow same pattern: try Action, catch exceptions
--    - Parameterized variants (With_Param) support indefinite types via (<>)
--    - Result variants require Map_Exception to convert to domain errors
--    - Option variants simply return None on any exception
--
--  ===========================================================================

package body Functional.Try is

   --  ========================================================================
   --  Try_To_Result - General bridge implementation
   --  ========================================================================

   function Try_To_Result return Result_Type is
   begin
      return Ok (Action);
   exception
      when Occ : others =>
         return Err (Map_Exception (Occ));
   end Try_To_Result;

   --  ========================================================================
   --  Try_To_Functional_Result - Convenience wrapper
   --  ========================================================================

   function Try_To_Functional_Result return Result_Pkg.Result is
      --  Instantiate the general Try_To_Result with Functional.Result types
      function Impl is new
        Try_To_Result
          (T             => T,
           E             => E,
           Result_Type   => Result_Pkg.Result,
           Ok            => Result_Pkg.Ok,
           Err           => Result_Pkg.Err,
           Map_Exception => Map_Exception,
           Action        => Action);
   begin
      return Impl;
   end Try_To_Functional_Result;

   --  ========================================================================
   --  Try_To_Functional_Option - Convenience wrapper
   --  ========================================================================

   function Try_To_Functional_Option return Option_Pkg.Option is
   begin
      return Option_Pkg.New_Some (Action);
   exception
      when others =>
         return Option_Pkg.None;
   end Try_To_Functional_Option;

   --  ========================================================================
   --  Try_To_Result_With_Param - Parameterized Result bridge
   --  ========================================================================

   function Try_To_Result_With_Param (P : Param)
     return Result_Pkg.Result
   is
   begin
      return Result_Pkg.Ok (Action (P));
   exception
      when Occ : others =>
         return Result_Pkg.Err (Map_Exception (Occ));
   end Try_To_Result_With_Param;

   --  ========================================================================
   --  Try_To_Option_With_Param - Parameterized Option bridge
   --  ========================================================================

   function Try_To_Option_With_Param (P : Param)
     return Option_Pkg.Option
   is
   begin
      return Option_Pkg.New_Some (Action (P));
   exception
      when others =>
         return Option_Pkg.None;
   end Try_To_Option_With_Param;

end Functional.Try;
