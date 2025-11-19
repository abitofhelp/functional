pragma Ada_2022;
--  ============================================================================
--  Test_Try_Option - Comprehensive unit tests for Functional.Try.To_Option
--  ============================================================================
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Target: 90%+ code coverage
--  Tests Try.To_Option exception conversion
--  ============================================================================

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with Functional.Option;
with Functional.Try.To_Option;
with Test_Framework;

procedure Test_Try_Option is

   package Int_Option is new Functional.Option (T => Integer);

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

      package Get_Try is new
        Functional.Try.To_Option
          (T        => Integer,
           T_Option => Int_Option,
           Action   => Get_Value);

      Result : constant Int_Option.Option := Get_Try.Run;
   begin
      Put_Line ("Testing Try with successful action...");
      Assert
        (Int_Option.Is_Some (Result),
         "Try returns Some for successful action");
      Assert (Int_Option.Value (Result) = 42, "Try returns correct value");
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

      package Raise_Try is new
        Functional.Try.To_Option
          (T        => Integer,
           T_Option => Int_Option,
           Action   => Raise_Error);

      Result : constant Int_Option.Option := Raise_Try.Run;
   begin
      Put_Line ("Testing Try with exception...");
      Assert
        (Int_Option.Is_None (Result), "Try returns None when action raises");
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

      package Parse_Try is new
        Functional.Try.To_Option
          (T        => Integer,
           T_Option => Int_Option,
           Action   => Parse);

      Result : constant Int_Option.Option := Parse_Try.Run;
   begin
      Put_Line ("Testing Try with parse failure...");
      Assert (Int_Option.Is_None (Result), "Try catches parse exception");
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

      package Parse_Try is new
        Functional.Try.To_Option
          (T        => Integer,
           T_Option => Int_Option,
           Action   => Parse);

      Result : constant Int_Option.Option := Parse_Try.Run;
   begin
      Put_Line ("Testing Try with successful parse...");
      Assert
        (Int_Option.Is_Some (Result), "Try returns Some for successful parse");
      Assert
        (Int_Option.Value (Result) = 42, "Try returns parsed integer value");
   end Test_Try_Parse_Success;

begin
   Put_Line ("======================================");
   Put_Line ("  Functional.Try.To_Option Tests");
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
end Test_Try_Option;
