pragma Ada_2022;
--  ===========================================================================
--  Functional.Try.Map_To_Result - Implementation
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Implementation Notes:
--    - Uses Ada.Exceptions.Exception_Identity for matching
--    - Linear search through mappings (typically small arrays)
--    - First match wins (order matters if duplicates exist)
--
--  ===========================================================================

package body Functional.Try.Map_To_Result is

   use Ada.Exceptions;

   --  ========================================================================
   --  Find_Error_Kind - Look up exception in mappings
   --  ========================================================================
   --  Returns the mapped Error_Kind if found, Default_Error_Kind otherwise.

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

      --  Not found in mappings - use default
      return Default_Error_Kind;
   end Find_Error_Kind;

   --  ========================================================================
   --  Run - Run action with exception mapping
   --  ========================================================================

   function Run (Mappings : Mapping_Array) return Result_Type is
   begin
      return Action;
   exception
      when Occ : others =>
         return Make_Error
           (Kind    => Find_Error_Kind (Mappings, Occ),
            Message => Exception_Message (Occ));
   end Run;

   --  ========================================================================
   --  Run_Catch_All - Run action with default error only
   --  ========================================================================

   function Run_Catch_All return Result_Type is
   begin
      return Action;
   exception
      when Occ : others =>
         return Make_Error
           (Kind    => Default_Error_Kind,
            Message => Exception_Message (Occ));
   end Run_Catch_All;

end Functional.Try.Map_To_Result;
