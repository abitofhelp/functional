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

package Test_Framework is

   --  Track grand totals across all test suites
   procedure Register_Results (Total : Natural; Passed : Natural);

   --  Get cumulative results
   function Grand_Total_Tests return Natural;
   function Grand_Total_Passed return Natural;

   --  Reset counters (for test runner)
   procedure Reset;

   --  Print professional color-coded category summary box
   --  - Bright green box for success (all tests passed)
   --  - Bright red box for failure (any tests failed)
   --  - Returns exit status: 0 for success, 1 for failure
   function Print_Category_Summary
     (Category_Name : String;
      Total         : Natural;
      Passed        : Natural) return Integer;

end Test_Framework;
