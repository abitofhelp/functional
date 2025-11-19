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

   --  Final summary with visual indicator
   New_Line;
   Put_Line ("========================================================");
   if Grand_Passed = Grand_Total and then not Overall_Failed then
      Put_Line
        ("  [PASS] GRAND TOTAL: "
         & Grand_Passed'Image
         & " /"
         & Grand_Total'Image
         & " TESTS PASSED");
      Put_Line ("========================================================");
      Ada.Command_Line.Set_Exit_Status (0);
   else
      Put_Line
        ("  [FAIL] GRAND TOTAL: "
         & Grand_Passed'Image
         & " /"
         & Grand_Total'Image
         & " TESTS (FAILURES DETECTED)");
      Put_Line ("========================================================");
      Ada.Command_Line.Set_Exit_Status (1);
   end if;
end Test_Runner;
