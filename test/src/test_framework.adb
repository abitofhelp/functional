pragma Ada_2022;
--  ============================================================================
--  Test_Framework - Implementation
--  ============================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  ============================================================================

package body Test_Framework is

   Total_Tests  : Natural := 0;
   Total_Passed : Natural := 0;

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

end Test_Framework;
