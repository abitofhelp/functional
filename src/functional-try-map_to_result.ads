pragma Ada_2022;
--  ===========================================================================
--  Functional.Try.Map_To_Result - Exception-to-Result with Declarative Mappings
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Provides exception-to-Result conversion with declarative exception
--    mappings. Supports 1..N:1..N mapping (multiple anticipated exceptions
--    to multiple domain error kinds).
--
--  Mapping Modes:
--    Empty mapping (or Run_Catch_All):
--      - Catch-all: any exception -> Default_Error_Kind
--      - Equivalent to Try_To_Result with generic error
--      - Use when you just want a safety net with context
--
--    Single mapping:
--      - 1:1 discrimination: one specific exception -> one specific error
--      - Unmapped exceptions fall through to Default_Error_Kind
--
--    Multiple mappings:
--      - 1..N:1..N discrimination: each exception -> its specific error
--      - Unmapped exceptions fall through to Default_Error_Kind
--      - Declarative table is self-documenting (data, not code)
--
--  Usage (multiple mappings):
--    package Try_Read is new Functional.Try.Map_To_Result
--      (Error_Kind_Type    => Domain.Error.Error_Kind,
--       Result_Type        => Read_Result.Result,
--       Make_Error         => Make_Read_Error,
--       Default_Error_Kind => Internal_Error,
--       Action             => Raw_Read);
--
--    Mappings : constant Try_Read.Mapping_Array :=
--      [(Name_Error'Identity,  Not_Found_Error),
--       (Use_Error'Identity,   IO_Error)];
--
--    Result := Try_Read.Run (Mappings);
--
--  Usage (catch-all):
--    Result := Try_Read.Run_Catch_All;  --  or Run(Empty_Mappings)
--
--  ===========================================================================

with Ada.Exceptions;

generic
   --  Error enumeration type (domain-specific)
   type Error_Kind_Type is (<>);

   --  Result type being returned
   type Result_Type is private;

   --  Constructor for error Results
   --  Takes error kind and message, returns error Result
   with function Make_Error
     (Kind : Error_Kind_Type; Message : String) return Result_Type;

   --  Default error kind for unmapped (unexpected) exceptions
   Default_Error_Kind : Error_Kind_Type;

   --  The action to execute (returns Result_Type directly)
   with function Action return Result_Type;

package Functional.Try.Map_To_Result with
  SPARK_Mode => Off
is

   --  ========================================================================
   --  Exception Mapping Types
   --  ========================================================================

   --  Maps an exception identity to a domain error kind
   type Exception_Mapping is record
      Exception_Id : Ada.Exceptions.Exception_Id;
      Error_Kind   : Error_Kind_Type;
   end record;

   --  Array of mappings (order doesn't matter - first match wins)
   type Mapping_Array is array (Positive range <>) of Exception_Mapping;

   --  Empty mapping array constant for convenience
   Empty_Mappings : constant Mapping_Array (1 .. 0) := [others => <>];

   --  ========================================================================
   --  Run with Mappings
   --  ========================================================================
   --  Runs Action. If an exception occurs:
   --    1. Search Mappings for matching Exception_Id
   --    2. If found: return Make_Error with mapped Error_Kind
   --    3. If not found: return Make_Error with Default_Error_Kind

   function Run (Mappings : Mapping_Array) return Result_Type;

   --  ========================================================================
   --  Run Catch-All (no specific mappings)
   --  ========================================================================
   --  Equivalent to Run(Empty_Mappings).
   --  All exceptions map to Default_Error_Kind.
   --  Use when you just want a safety net without specific discrimination.

   function Run_Catch_All return Result_Type;

end Functional.Try.Map_To_Result;
