pragma Ada_2022;
--  ===========================================================================
--  Functional.Try.To_Option - Simple Exception-to-Option Bridge
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Bridges exception-based code to Option types. Returns Some(value) on
--    success, None on any exception.
--
--  When to Use:
--    - "Probe" operations where you have a sensible default (Is_TZif_File, etc.)
--    - "Best effort" reads with Unwrap_Or fallback (Read_Version, etc.)
--    - Recursive traversals that skip on access errors (Count_Files, etc.)
--    - When failure is expected/normal, not an error condition
--
--  When NOT to Use:
--    - When error details matter for logging/debugging
--    - When different exceptions need different handling
--    - For critical operations where failures should be explicit
--
--  ===========================================================================

generic
   type T is private;
   with package T_Option is new Functional.Option (T => T);
   with function Action return T;
--  SPARK_Mode => Off: Exception boundary (inherited from parent, explicit for visibility)
package Functional.Try.To_Option
  with SPARK_Mode => Off
is

   --  Run the action and convert exceptions to Option
   function Run return T_Option.Option;

end Functional.Try.To_Option;
