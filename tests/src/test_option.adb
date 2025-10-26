pragma Ada_2022;
--  ============================================================================
--  Test_Option - Comprehensive unit tests for Functional.Option
--  ============================================================================
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Target: 90%+ code coverage
--  Tests all 9 Option functions
--  ============================================================================

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with Functional.Option;
with Test_Framework;

procedure Test_Option is

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
   --  Test: Constructors and Predicates
   --  ==========================================================================

   procedure Test_Constructors is
      O_Some : constant Int_Option.Option := Int_Option.New_Some (42);
      O_None : constant Int_Option.Option := Int_Option.None;
   begin
      Put_Line ("Testing Constructors and Predicates...");
      Assert (Int_Option.Is_Some (O_Some), "Some creates Some option");
      Assert (Int_Option.Is_None (O_None), "None creates None option");
      Assert (not Int_Option.Is_None (O_Some), "Some option is not None");
      Assert (not Int_Option.Is_Some (O_None), "None option is not Some");
   end Test_Constructors;

   --  ==========================================================================
   --  Test: Value Extractor
   --  ==========================================================================

   procedure Test_Value is
      O_Some : constant Int_Option.Option := Int_Option.New_Some (42);
      Val    : Integer;
   begin
      Put_Line ("Testing Value extractor...");
      Val := Int_Option.Value (O_Some);
      Assert (Val = 42, "Value extractor returns correct value");
   end Test_Value;

   --  ==========================================================================
   --  Test: Unwrap_Or and Unwrap_Or_With
   --  ==========================================================================

   procedure Test_Unwrap is
      O_Some : constant Int_Option.Option := Int_Option.New_Some (42);
      O_None : constant Int_Option.Option := Int_Option.None;

      function Get_Default return Integer
      is (999);
      function Unwrap_With is new Int_Option.Unwrap_Or_With (F => Get_Default);
   begin
      Put_Line ("Testing Unwrap_Or and Unwrap_Or_With...");
      Assert
        (Int_Option.Unwrap_Or (O_Some, 0) = 42,
         "Unwrap_Or returns value for Some");
      Assert
        (Int_Option.Unwrap_Or (O_None, 99) = 99,
         "Unwrap_Or returns default for None");

      Assert
        (Unwrap_With (O_Some) = 42, "Unwrap_Or_With returns value for Some");
      Assert
        (Unwrap_With (O_None) = 999,
         "Unwrap_Or_With calls lazy default for None");
   end Test_Unwrap;

   --  ==========================================================================
   --  Test: Map (transform Some value)
   --  ==========================================================================

   procedure Test_Map is
      O_Some : constant Int_Option.Option := Int_Option.New_Some (5);
      O_None : constant Int_Option.Option := Int_Option.None;

      function Double (X : Integer) return Integer
      is (X * 2);

      function Transform is new Int_Option.Map (F => Double);

      Result : Int_Option.Option;
   begin
      Put_Line ("Testing Map...");
      Result := Transform (O_Some);
      Assert
        (Int_Option.Is_Some (Result) and then Int_Option.Value (Result) = 10,
         "Map transforms Some value");

      Result := Transform (O_None);
      Assert (Int_Option.Is_None (Result), "Map leaves None unchanged");
   end Test_Map;

   --  ==========================================================================
   --  Test: And_Then (monadic bind/chain)
   --  ==========================================================================

   procedure Test_And_Then is
      O_Some : constant Int_Option.Option := Int_Option.New_Some (5);
      O_None : constant Int_Option.Option := Int_Option.None;

      function Double (X : Integer) return Int_Option.Option
      is (Int_Option.New_Some (X * 2));

      function To_None_If_Negative (X : Integer) return Int_Option.Option is
      begin
         if X < 0 then
            return Int_Option.None;
         else
            return Int_Option.New_Some (X);
         end if;
      end To_None_If_Negative;

      function Chain_Double is new Int_Option.And_Then (F => Double);
      function Chain_Validate is new
        Int_Option.And_Then (F => To_None_If_Negative);

      Result : Int_Option.Option;
   begin
      Put_Line ("Testing And_Then...");
      Result := Chain_Double (O_Some);
      Assert
        (Int_Option.Is_Some (Result) and then Int_Option.Value (Result) = 10,
         "And_Then chains Some -> Some");

      Result := Chain_Double (O_None);
      Assert (Int_Option.Is_None (Result), "And_Then short-circuits on None");

      Result := Chain_Validate (Int_Option.New_Some (-5));
      Assert
        (Int_Option.Is_None (Result),
         "And_Then propagates None from function");
   end Test_And_Then;

   --  ==========================================================================
   --  Test: Filter
   --  ==========================================================================

   procedure Test_Filter is
      function Is_Positive (X : Integer) return Boolean
      is (X > 0);
      function Keep_Positive is new Int_Option.Filter (Pred => Is_Positive);

      Result : Int_Option.Option;
   begin
      Put_Line ("Testing Filter...");
      Result := Keep_Positive (Int_Option.New_Some (5));
      Assert
        (Int_Option.Is_Some (Result), "Filter keeps Some if predicate holds");

      Result := Keep_Positive (Int_Option.New_Some (-5));
      Assert
        (Int_Option.Is_None (Result),
         "Filter converts to None if predicate fails");

      Result := Keep_Positive (Int_Option.None);
      Assert (Int_Option.Is_None (Result), "Filter leaves None unchanged");
   end Test_Filter;

   --  ==========================================================================
   --  Test: Or_Else and Or_Else_With
   --  ==========================================================================

   procedure Test_Or_Else is
      O_Some1 : constant Int_Option.Option := Int_Option.New_Some (10);
      O_Some2 : constant Int_Option.Option := Int_Option.New_Some (20);
      O_None  : constant Int_Option.Option := Int_Option.None;

      function Get_Backup return Int_Option.Option
      is (Int_Option.New_Some (99));
      function Or_Else_Lazy is new Int_Option.Or_Else_With (F => Get_Backup);
   begin
      Put_Line ("Testing Or_Else and Or_Else_With...");
      Assert
        (Int_Option.Value (Int_Option.Or_Else (O_Some1, O_Some2)) = 10,
         "Or_Else returns first Some");
      Assert
        (Int_Option.Value (Int_Option.Or_Else (O_None, O_Some2)) = 20,
         "Or_Else returns second if first is None");
      Assert
        (Int_Option.Is_None (Int_Option.Or_Else (O_None, O_None)),
         "Or_Else returns None if both are None");

      Assert
        (Int_Option.Value (Or_Else_Lazy (O_Some1)) = 10,
         "Or_Else_With returns Some without calling function");
      Assert
        (Int_Option.Value (Or_Else_Lazy (O_None)) = 99,
         "Or_Else_With calls lazy backup on None");
   end Test_Or_Else;

begin
   Put_Line ("======================================");
   Put_Line ("  Functional.Option Unit Tests");
   Put_Line ("  Target: 90%+ Coverage");
   Put_Line ("======================================");
   New_Line;

   Test_Constructors;
   Test_Value;
   Test_Unwrap;
   Test_Map;
   Test_And_Then;
   Test_Filter;
   Test_Or_Else;

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
end Test_Option;
