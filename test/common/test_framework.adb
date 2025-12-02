pragma Ada_2022;
--  ======================================================================
--  Test_Framework
--  ======================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Reusable test framework for professional test suites.
--    Provides test result tracking and standardized output formatting.
--
--  ======================================================================

with Ada.Text_IO;

package body Test_Framework is

   use Ada.Text_IO;

   Total_Tests  : Natural := 0;
   Total_Passed : Natural := 0;

   --  ANSI color codes for professional output
   Color_Green : constant String := ASCII.ESC & "[1;92m";  --  Bright green
   Color_Red   : constant String := ASCII.ESC & "[1;91m";  --  Bright red
   Color_Reset : constant String := ASCII.ESC & "[0m";     --  Reset to default

   procedure Register_Results (Total : Natural; Passed : Natural) is
   begin
      Total_Tests := Total_Tests + Total;
      Total_Passed := Total_Passed + Passed;
   end Register_Results;

   function Grand_Total_Tests return Natural
   is (Total_Tests);

   function Grand_Total_Passed return Natural
   is (Total_Passed);

   procedure Reset is
   begin
      Total_Tests := 0;
      Total_Passed := 0;
   end Reset;

   function Print_Category_Summary
     (Category_Name : String;
      Total         : Natural;
      Passed        : Natural) return Integer
   is
   begin
      Put_Line ("");

      if Passed = Total then
         --  Success: Bright green box
         Put_Line (Color_Green & "########################################");
         Put_Line ("###");
         Put_Line ("###    " & Category_Name & ": SUCCESS");
         Put_Line ("###    All " & Total'Image & " tests passed!");
         Put_Line ("###");
         Put_Line ("########################################" & Color_Reset);
         Put_Line ("");
         return 0;  --  Success exit code

      else
         --  Failure: Bright red box
         Put_Line (Color_Red & "########################################");
         Put_Line ("###");
         Put_Line ("###    " & Category_Name & ": FAILURE");
         Put_Line
           ("###   " & Natural'Image (Total - Passed) & " of" &
            Total'Image & " tests failed");
         Put_Line ("###");
         Put_Line ("########################################" & Color_Reset);
         Put_Line ("");
         return 1;  --  Failure exit code
      end if;
   end Print_Category_Summary;

end Test_Framework;
