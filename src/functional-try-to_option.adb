pragma Ada_2022;
--  ===========================================================================
--  Functional.Try.To_Option - Implementation
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  ===========================================================================

package body Functional.Try.To_Option is

   ---------
   -- Run --
   ---------

   function Run return T_Option.Option is
   begin
      return T_Option.New_Some (Action);
   exception
      when others =>
         return T_Option.None;
   end Run;

end Functional.Try.To_Option;
