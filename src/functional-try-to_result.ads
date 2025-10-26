pragma Ada_2022;
--  ==========================================================================
--  Functional.Try.To_Result
--  ==========================================================================
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Bridge from exception-based code to Result-based error handling.
--    Wraps an action that may raise and converts to Result<T,E>.
--
--  Usage:
--    function Load return String is ... end Load;  -- may raise
--    function To_Error (Exc : Exception_Occurrence) return Error;
--    package Str_Result is new Functional.Result (T => String, E => Error);
--
--    package Load_Try is new Functional.Try.To_Result
--      (T => String, E => Error, T_Result => Str_Result,
--       Action => Load, Map_Exception => To_Error);
--
--    R : constant Str_Result.Result := Load_Try.Run;
--
--  Design Notes:
--    Use at boundaries to convert exception-based APIs to Result-based flow.
--    Catch all exceptions and map to domain errors.
--
--  See Also:
--    Functional.Result - Result<T,E> type
--  ==========================================================================

with Ada.Exceptions;
with Functional.Result;

generic
   type T is private;
   type E is private;
   with package T_Result is new Functional.Result (T => T, E => E);
   with function Action return T;
   with
     function Map_Exception
       (Exc : Ada.Exceptions.Exception_Occurrence) return E;
package Functional.Try.To_Result is

   function Run return T_Result.Result;

end Functional.Try.To_Result;
