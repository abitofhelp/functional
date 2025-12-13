pragma Ada_2022;
--  ===========================================================================
--  Functional.Try.Map_To_Result_With_Param - Implementation
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  ===========================================================================

with Ada.Exceptions; use Ada.Exceptions;

package body Functional.Try.Map_To_Result_With_Param is

   --  ========================================================================
   --  Find_Error_Kind - Internal Helper
   --  ========================================================================
   --  Searches mappings for matching exception, returns mapped error kind
   --  or Default_Error_Kind if not found.

   function Find_Error_Kind
     (Mappings : Mapping_Array;
      Occ      : Exception_Occurrence) return Error_Kind_Type
   is
      Ex_Id : constant Exception_Id := Exception_Identity (Occ);
   begin
      for M of Mappings loop
         if M.Exception_Id = Ex_Id then
            return M.Error_Kind;
         end if;
      end loop;
      return Default_Error_Kind;
   end Find_Error_Kind;

   --  ========================================================================
   --  Run
   --  ========================================================================

   function Run (P : Param_Type; Mappings : Mapping_Array) return Result_Type is
   begin
      return Action (P);
   exception
      when Occ : others =>
         return Make_Error
           (Kind    => Find_Error_Kind (Mappings, Occ),
            Message => Exception_Message (Occ));
   end Run;

   --  ========================================================================
   --  Run_Catch_All
   --  ========================================================================

   function Run_Catch_All (P : Param_Type) return Result_Type is
   begin
      return Run (P, Empty_Mappings);
   end Run_Catch_All;

end Functional.Try.Map_To_Result_With_Param;
