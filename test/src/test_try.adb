pragma Ada_2022;
--  ============================================================================
--  Test_Try - Comprehensive unit tests for Functional.Try.To_Result
--  ============================================================================
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Target: 90%+ code coverage
--  Tests Try.To_Result exception conversion
--  ============================================================================

with Ada.Text_IO;    use Ada.Text_IO;
with Ada.Command_Line;
with Ada.Exceptions; use Ada.Exceptions;
with Functional.Result;
with Functional.Try.To_Result;
with Test_Framework;

procedure Test_Try is

   type Error_Kind is (Exception_Error);

   subtype Error_Message is String (1 .. 100);

   type Error is record
      Kind    : Error_Kind;
      Message : Error_Message;
      Len     : Natural := 0;
   end record;

   package Int_Result is new Functional.Result (T => Integer, E => Error);

   Test_Count : Natural := 0;
   Pass_Count : Natural := 0;

   procedure Assert (Condition : Boolean; Test_Name : String) is
   begin
      Test_Count := Test_Count + 1;
      if Condition then
         Pass_Count := Pass_Count + 1;
         Put_Line ("[PASS] " & Test_Name);
      else
         Put_Line ("[FAIL] " & Test_Name);
      end if;
   end Assert;

   --  ==========================================================================
   --  Test: Try with successful action
   --  ==========================================================================

   procedure Test_Try_Success is
      function Get_Value return Integer
      is (42);

      function To_Error (Exc : Exception_Occurrence) return Error is
         Msg : constant String := Exception_Message (Exc);
      begin
         return
           (Exception_Error, Msg & [Msg'Length + 1 .. 100 => ' '], Msg'Length);
      end To_Error;

      package Get_Try is new
        Functional.Try.To_Result
          (T             => Integer,
           E             => Error,
           T_Result      => Int_Result,
           Action        => Get_Value,
           Map_Exception => To_Error);

      Result : constant Int_Result.Result := Get_Try.Run;
   begin
      Put_Line ("Testing Try with successful action...");
      Assert
        (Int_Result.Is_Ok (Result), "Try returns Ok for successful action");
      Assert (Int_Result.Value (Result) = 42, "Try returns correct value");
   end Test_Try_Success;

   --  ==========================================================================
   --  Test: Try with failing action (exception)
   --  ==========================================================================

   procedure Test_Try_Exception is
      function Raise_Error return Integer is
      begin
         raise Constraint_Error with "test exception";
         return 0;  -- Never reached, satisfies compiler
      end Raise_Error;

      function To_Error (Exc : Exception_Occurrence) return Error is
         Msg : constant String := Exception_Message (Exc);
      begin
         return
           (Exception_Error, Msg & [Msg'Length + 1 .. 100 => ' '], Msg'Length);
      end To_Error;

      package Raise_Try is new
        Functional.Try.To_Result
          (T             => Integer,
           E             => Error,
           T_Result      => Int_Result,
           Action        => Raise_Error,
           Map_Exception => To_Error);

      Result : constant Int_Result.Result := Raise_Try.Run;
      Err    : Error;
   begin
      Put_Line ("Testing Try with exception...");
      Assert
        (Int_Result.Is_Err (Result), "Try returns Err when action raises");

      Err := Int_Result.Error (Result);
      Assert
        (Err.Message (1 .. Err.Len) = "test exception",
         "Try preserves exception message");
   end Test_Try_Exception;

   --  ==========================================================================
   --  Test: Try with Integer parse (real-world example)
   --  ==========================================================================

   procedure Test_Try_Parse_Int is
      Input : constant String := "not_a_number";

      function Parse return Integer is
      begin
         return Integer'Value (Input);
      end Parse;

      function To_Error (Exc : Exception_Occurrence) return Error is
         Msg : constant String := "Parse error: " & Exception_Name (Exc);
      begin
         return
           (Exception_Error, Msg & [Msg'Length + 1 .. 100 => ' '], Msg'Length);
      end To_Error;

      package Parse_Try is new
        Functional.Try.To_Result
          (T             => Integer,
           E             => Error,
           T_Result      => Int_Result,
           Action        => Parse,
           Map_Exception => To_Error);

      Result : constant Int_Result.Result := Parse_Try.Run;
      Err    : Error;
   begin
      Put_Line ("Testing Try with parse failure...");
      Assert (Int_Result.Is_Err (Result), "Try catches parse exception");

      Err := Int_Result.Error (Result);
      Assert
        (Err.Message (1 .. 12) = "Parse error:",
         "Try error includes custom message prefix");
   end Test_Try_Parse_Int;

   --  ==========================================================================
   --  Test: Try with successful integer parse
   --  ==========================================================================

   procedure Test_Try_Parse_Success is
      Input : constant String := "42";

      function Parse return Integer is
      begin
         return Integer'Value (Input);
      end Parse;

      function To_Error (Exc : Exception_Occurrence) return Error is
         Msg : constant String := "Parse error: " & Exception_Message (Exc);
      begin
         return
           (Exception_Error, Msg & [Msg'Length + 1 .. 100 => ' '], Msg'Length);
      end To_Error;

      package Parse_Try is new
        Functional.Try.To_Result
          (T             => Integer,
           E             => Error,
           T_Result      => Int_Result,
           Action        => Parse,
           Map_Exception => To_Error);

      Result : constant Int_Result.Result := Parse_Try.Run;
   begin
      Put_Line ("Testing Try with successful parse...");
      Assert
        (Int_Result.Is_Ok (Result), "Try returns Ok for successful parse");
      Assert
        (Int_Result.Value (Result) = 42, "Try returns parsed integer value");
   end Test_Try_Parse_Success;

begin
   Put_Line ("======================================");
   Put_Line ("  Functional.Try.To_Result Tests");
   Put_Line ("  Target: 90%+ Coverage");
   Put_Line ("======================================");
   New_Line;

   Test_Try_Success;
   Test_Try_Exception;
   Test_Try_Parse_Int;
   Test_Try_Parse_Success;

   New_Line;
   Put_Line ("======================================");
   Put_Line
     ("  Results: " & Pass_Count'Image & " /" & Test_Count'Image & " passed");
   if Pass_Count = Test_Count then
      Put_Line ("  Status: ALL TESTS PASSED");
   else
      Put_Line ("  Status: FAILURES DETECTED");
   end if;
   Put_Line ("======================================");

   --  Register results with test framework
   Test_Framework.Register_Results (Test_Count, Pass_Count);

   if Pass_Count /= Test_Count then
      Ada.Command_Line.Set_Exit_Status (1);
   end if;
end Test_Try;
