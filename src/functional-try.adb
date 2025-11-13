pragma Ada_2022;
--  ===========================================================================
--  Functional.Try
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Try implementation.
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

end Functional.Try;
