pragma Ada_2022;
--  ============================================================================
--  Test_Runner - Main test harness for Functional library
--  ============================================================================
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Runs all test suites for comprehensive coverage
--  Target: 90%+ code coverage across all modules
--  ============================================================================

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with Test_Framework;

--  Test procedures
with Test_Result;
with Test_Option;
with Test_Either;
with Test_Try;
with Test_Try_Option;

procedure Test_Runner is
   Overall_Failed : Boolean := False;
   Grand_Total    : Natural;
   Grand_Passed   : Natural;

   procedure Run_Test (Name : String; Test : access procedure) is
   begin
      New_Line;
      Put_Line ("====================================================");
      Put_Line ("  Running: " & Name);
      Put_Line ("====================================================");
      begin
         Test.all;
      exception
         when others =>
            Put_Line ("[EXCEPTION] Test suite crashed!");
            Overall_Failed := True;
      end;
   end Run_Test;

begin
   Put_Line ("========================================================");
   Put_Line ("  Functional Library - Comprehensive Test Suite");
   Put_Line ("  Target: 90%+ Code Coverage");
   Put_Line ("========================================================");

   --  Reset framework counters
   Test_Framework.Reset;

   --  Run all test suites
   Run_Test ("Result Tests", Test_Result'Access);
   Run_Test ("Option Tests", Test_Option'Access);
   Run_Test ("Either Tests", Test_Either'Access);
   Run_Test ("Try.To_Result Tests", Test_Try'Access);
   Run_Test ("Try.To_Option Tests", Test_Try_Option'Access);

   --  Get grand totals
   Grand_Total := Test_Framework.Grand_Total_Tests;
   Grand_Passed := Test_Framework.Grand_Total_Passed;

   --  Print grand summary
   New_Line;
   Put_Line ("========================================================");
   Put_Line ("        GRAND TOTAL - ALL FUNCTIONAL TESTS");
   Put_Line ("========================================================");
   Put_Line ("Total tests:  " & Grand_Total'Image);
   Put_Line ("Passed:       " & Grand_Passed'Image);
   Put_Line ("Failed:       " & Natural'Image (Grand_Total - Grand_Passed));

   --  Print professional color-coded summary and get exit status
   declare
      Exit_Code : Integer :=
        Test_Framework.Print_Category_Summary
          ("FUNCTIONAL", Grand_Total, Grand_Passed);
   begin
      --  Account for crashed suites
      if Overall_Failed then
         Exit_Code := 1;
      end if;
      Ada.Command_Line.Set_Exit_Status
        (if Exit_Code = 0 then Ada.Command_Line.Success
         else Ada.Command_Line.Failure);
   end;
end Test_Runner;
