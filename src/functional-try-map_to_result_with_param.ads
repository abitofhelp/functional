pragma Ada_2022;
--  ===========================================================================
--  Functional.Try.Map_To_Result_With_Param - Parameterized Exception Mapping
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Parameterized version of Map_To_Result for actions that require input
--    context. Provides exception-to-Result conversion with declarative
--    exception mappings when the action needs a parameter.
--
--  When to Use:
--    - Action needs input context (file path, user ID, configuration, etc.)
--    - You want declarative 1..N:1..N exception-to-error mapping
--    - Parameter type can be indefinite (e.g., String)
--
--  Usage:
--    function Make_Read_Error (Kind : Error_Kind; Message : String)
--      return Read_Result.Result is
--    begin
--       return Read_Result.Error (Kind, Message);
--    end Make_Read_Error;
--
--    function Raw_Read (Path : String) return Read_Result.Result is
--    begin
--       --  File reading logic that may raise
--       return Read_Result.Ok (Data);
--    end Raw_Read;
--
--    package Try_Read is new Functional.Try.Map_To_Result_With_Param
--      (Error_Kind_Type    => Domain.Error.Error_Kind,
--       Param_Type         => String,
--       Result_Type        => Read_Result.Result,
--       Make_Error         => Make_Read_Error,
--       Default_Error_Kind => Internal_Error,
--       Action             => Raw_Read);
--
--    Mappings : constant Try_Read.Mapping_Array :=
--      [(Name_Error'Identity,  Not_Found_Error),
--       (Use_Error'Identity,   IO_Error)];
--
--    Result := Try_Read.Run ("/path/to/file", Mappings);
--
--  ===========================================================================

with Ada.Exceptions;

generic
   --  Error enumeration type (domain-specific)
   type Error_Kind_Type is (<>);

   --  Parameter type for the action (supports indefinite types like String)
   type Param_Type (<>) is private;

   --  Result type being returned
   type Result_Type is private;

   --  Constructor for error Results
   --  Takes error kind and message, returns error Result
   with function Make_Error
     (Kind : Error_Kind_Type; Message : String) return Result_Type;

   --  Default error kind for unmapped (unexpected) exceptions
   Default_Error_Kind : Error_Kind_Type;

   --  The action to execute (takes parameter, returns Result_Type)
   with function Action (P : Param_Type) return Result_Type;

package Functional.Try.Map_To_Result_With_Param with
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
   --  Runs Action(P). If an exception occurs:
   --    1. Search Mappings for matching Exception_Id
   --    2. If found: return Make_Error with mapped Error_Kind
   --    3. If not found: return Make_Error with Default_Error_Kind

   function Run (P : Param_Type; Mappings : Mapping_Array) return Result_Type;

   --  ========================================================================
   --  Run Catch-All (no specific mappings)
   --  ========================================================================
   --  Equivalent to Run(P, Empty_Mappings).
   --  All exceptions map to Default_Error_Kind.
   --  Use when you just want a safety net without specific discrimination.

   function Run_Catch_All (P : Param_Type) return Result_Type;

end Functional.Try.Map_To_Result_With_Param;
