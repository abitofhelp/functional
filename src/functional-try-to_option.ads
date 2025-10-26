pragma Ada_2022;
--  ============================================================================
--  Functional.Try.To_Option
--  ============================================================================
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Bridge from exception-based code to Option-based handling.
--    Wraps an action that may raise and converts to Option<T>.
--
--  Usage:
--    function Parse_Int return Integer is ... end Parse_Int;  -- may raise
--    package Int_Option is new Functional.Option (T => Integer);
--
--    package Parse_Try is new Functional.Try.To_Option
--      (T => Integer, T_Option => Int_Option, Action => Parse_Int);
--
--    O : constant Int_Option.Option := Parse_Try.Run;
--
--  Design Notes:
--    Use when exceptions represent absence, not errors. For error details,
--    prefer Try.To_Result. On success returns Some(value), on exception
--    returns None.
--
--  See Also:
--    Functional.Option      - Option<T> type
--    Functional.Try.To_Result - exception to Result conversion
--  ============================================================================

with Functional.Option;

generic
   type T is private;
   with package T_Option is new Functional.Option (T => T);
   with function Action return T;
package Functional.Try.To_Option is

   function Run return T_Option.Option;

end Functional.Try.To_Option;
