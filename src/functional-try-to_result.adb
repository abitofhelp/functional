pragma Ada_2022;
--  ===========================================================================
--  Functional.Try.To_Result - Implementation
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  ===========================================================================

package body Functional.Try.To_Result is

   ---------
   -- Run --
   ---------

   function Run return T_Result.Result is
   begin
      return T_Result.Ok (Action);
   exception
      when Occ : others =>
         return T_Result.New_Error (Map_Exception (Occ));
   end Run;

end Functional.Try.To_Result;
