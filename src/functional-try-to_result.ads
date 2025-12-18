pragma Ada_2022;
--  ===========================================================================
--  Functional.Try.To_Result - Simple Exception-to-Result Bridge
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Bridges exception-based code to Result types using a procedural
--    Map_Exception function for exception-to-error conversion.
--
--  When to Use:
--    - Simple 1:1 exception-to-error mapping with procedural logic
--    - When you need to inspect Exception_Occurrence for custom error building
--    - Legacy code integration
--
--  When to Use Map_To_Result Instead:
--    - Declarative exception mappings (data, not code)
--    - Multiple exception types mapping to different error kinds
--    - Self-documenting mapping tables
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
--  SPARK_Mode => Off: Exception boundary (inherited from parent, explicit for visibility)
package Functional.Try.To_Result
  with SPARK_Mode => Off
is

   --  Run the action and convert exceptions to Result
   function Run return T_Result.Result;

end Functional.Try.To_Result;
