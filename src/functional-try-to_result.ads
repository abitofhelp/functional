pragma Ada_2022;
--  ===========================================================================
--  Functional.Try.To_Result - Child package for backwards compatibility
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Provides backwards-compatible API for existing tests.
--    Wraps the generic function Functional.Try.Try_To_Result.
--
--  Note:
--    New code should use Functional.Try.Try_To_Functional_Result directly
--    for cleaner syntax. This child package exists for test compatibility.
--
--  ===========================================================================

with Ada.Exceptions;

generic
   type T is private;
   type E is private;
   with package T_Result is new Functional.Result (T => T, E => E);
   with function Action return T;
   with
     function Map_Exception
       (Occ : Ada.Exceptions.Exception_Occurrence) return E is <>;
package Functional.Try.To_Result is

   --  Run the action and convert exceptions to Result
   function Run return T_Result.Result;

end Functional.Try.To_Result;
