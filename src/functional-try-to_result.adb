pragma Ada_2022;
--  ==========================================================================
--  Functional.Try.To_Result - Implementation
--  ==========================================================================
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  ==========================================================================

package body Functional.Try.To_Result is

   function Run return T_Result.Result is
   begin
      return T_Result.Ok (Action);
   exception
      when Exc : others =>
         return T_Result.Err (Map_Exception (Exc));
   end Run;

end Functional.Try.To_Result;
