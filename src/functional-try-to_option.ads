pragma Ada_2022;
--  ===========================================================================
--  Functional.Try.To_Option - Child package for backwards compatibility
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Provides backwards-compatible API for existing tests.
--    Wraps the generic function Functional.Try.Try_To_Functional_Option.
--
--  Note:
--    New code should use Functional.Try.Try_To_Functional_Option directly
--    for cleaner syntax. This child package exists for test compatibility.
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
