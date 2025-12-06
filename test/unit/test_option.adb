pragma Ada_2022;
--  ======================================================================
--  Test_Option
--  ======================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  Purpose:
--    Comprehensive unit tests for Functional.Option monad.
--    Tests all 9 Option functions. Target: 90%+ code coverage.
--  ======================================================================

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with Functional.Option;
with Functional.Result;
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

   --  ==========================================================================
   --  Test: "and" operator
   --  ==========================================================================

   procedure Test_And_Operator is
      use Int_Option;
      O_Some1 : constant Option := New_Some (10);
      O_Some2 : constant Option := New_Some (20);
      O_None  : constant Option := None;
      Result  : Option;
   begin
      Put_Line ("Testing ""and"" operator...");
      --  Both have values: returns B
      Result := O_Some1 and O_Some2;
      Assert (Is_Some (Result) and then Value (Result) = 20,
              """and"" returns second when both have values");

      --  First is None: returns None
      Result := O_None and O_Some2;
      Assert (Is_None (Result), """and"" returns None when first is None");

      --  Second is None: returns None
      Result := O_Some1 and O_None;
      Assert (Is_None (Result), """and"" returns None when second is None");

      --  Both are None: returns None
      Result := O_None and O_None;
      Assert (Is_None (Result), """and"" returns None when both are None");
   end Test_And_Operator;

   --  ==========================================================================
   --  Test: "xor" operator
   --  ==========================================================================

   procedure Test_Xor_Operator is
      use Int_Option;
      O_Some1 : constant Option := New_Some (10);
      O_Some2 : constant Option := New_Some (20);
      O_None  : constant Option := None;
      Result  : Option;
   begin
      Put_Line ("Testing ""xor"" operator...");
      --  First has value, second is None: returns first
      Result := O_Some1 xor O_None;
      Assert (Is_Some (Result) and then Value (Result) = 10,
              """xor"" returns first when only first has value");

      --  First is None, second has value: returns second
      Result := O_None xor O_Some2;
      Assert (Is_Some (Result) and then Value (Result) = 20,
              """xor"" returns second when only second has value");

      --  Both have values: returns None
      Result := O_Some1 xor O_Some2;
      Assert (Is_None (Result), """xor"" returns None when both have values");

      --  Both are None: returns None
      Result := O_None xor O_None;
      Assert (Is_None (Result), """xor"" returns None when both are None");
   end Test_Xor_Operator;

   --  ==========================================================================
   --  Test: Zip_With
   --  ==========================================================================

   procedure Test_Zip_With is
      use Int_Option;

      --  Second Integer option for the second operand
      package Int_Option2 is new Functional.Option (T => Integer);

      function Has_Value_2 (O : Int_Option2.Option) return Boolean
      is (Int_Option2.Is_Some (O));

      function Value_2 (O : Int_Option2.Option) return Integer
      is (Int_Option2.Value (O));

      --  Combine two integers by multiplication
      function Multiply (A : Integer; B : Integer) return Integer
      is (A * B);

      function Zip_Ints is new Int_Option.Zip_With
        (U => Integer,
         Option_U => Int_Option2.Option,
         Has_Value_U => Has_Value_2,
         Value_U => Value_2,
         Combine => Multiply);

      Int_Some1 : constant Option := New_Some (3);
      Int_Some2 : constant Int_Option2.Option := Int_Option2.New_Some (4);
      Int_None1 : constant Option := None;
      Int_None2 : constant Int_Option2.Option := Int_Option2.None;
      Result    : Option;
   begin
      Put_Line ("Testing Zip_With...");
      --  Both have values
      Result := Zip_Ints (Int_Some1, Int_Some2);
      Assert (Is_Some (Result) and then Value (Result) = 12,
              "Zip_With combines values when both present");

      --  First is None
      Result := Zip_Ints (Int_None1, Int_Some2);
      Assert (Is_None (Result), "Zip_With returns None when first is None");

      --  Second is None
      Result := Zip_Ints (Int_Some1, Int_None2);
      Assert (Is_None (Result), "Zip_With returns None when second is None");
   end Test_Zip_With;

   --  ==========================================================================
   --  Test: Flatten
   --  ==========================================================================

   procedure Test_Flatten is
      --  Option[Option[Integer]]
      package Nested_Option is new Functional.Option (T => Int_Option.Option);
      use Nested_Option;

      --  Identity conversion (T = Inner_Option in this case)
      function Convert (V : Int_Option.Option) return Int_Option.Option
      is (V);

      function Has_Value_Inner (O : Int_Option.Option) return Boolean
      is (Int_Option.Is_Some (O));

      function None_Inner return Int_Option.Option
      is (Int_Option.None);

      function Do_Flatten is new Nested_Option.Flatten
        (Inner_Option => Int_Option.Option,
         Convert => Convert,
         Has_Value_Inner => Has_Value_Inner,
         None_Inner => None_Inner);

      Inner_Some  : constant Int_Option.Option := Int_Option.New_Some (42);
      Inner_None  : constant Int_Option.Option := Int_Option.None;
      Outer_Some1 : constant Nested_Option.Option := New_Some (Inner_Some);
      Outer_Some2 : constant Nested_Option.Option := New_Some (Inner_None);
      Outer_None  : constant Nested_Option.Option := None;
      Result      : Int_Option.Option;
   begin
      Put_Line ("Testing Flatten...");
      --  Some(Some(42)) -> Some(42)
      Result := Do_Flatten (Outer_Some1);
      Assert (Int_Option.Is_Some (Result) and then Int_Option.Value (Result) = 42,
              "Flatten extracts inner value from Some(Some)");

      --  Some(None) -> None
      Result := Do_Flatten (Outer_Some2);
      Assert (Int_Option.Is_None (Result),
              "Flatten returns None for Some(None)");

      --  None -> None
      Result := Do_Flatten (Outer_None);
      Assert (Int_Option.Is_None (Result),
              "Flatten returns None for None");
   end Test_Flatten;

   --  ==========================================================================
   --  Test: Ok_Or and Ok_Or_Else
   --  ==========================================================================

   procedure Test_Ok_Or is
      use Int_Option;

      type Error_Code is (Missing_Value, Invalid_Input, Not_Found);

      package Int_Result is new Functional.Result (T => Integer, E => Error_Code);

      function Make_Ok (V : Integer) return Int_Result.Result
      is (Int_Result.Ok (V));

      function Make_Error (E : Error_Code) return Int_Result.Result
      is (Int_Result.New_Error (E));

      function Opt_To_Result is new Int_Option.Ok_Or
        (Error_Type => Error_Code,
         Result_Type => Int_Result.Result,
         Make_Ok => Make_Ok,
         Make_Error => Make_Error);

      function Produce_Error return Error_Code
      is (Missing_Value);

      function Opt_To_Result_Lazy is new Int_Option.Ok_Or_Else
        (Error_Type => Error_Code,
         Result_Type => Int_Result.Result,
         Make_Ok => Make_Ok,
         Make_Error => Make_Error,
         Produce_Error => Produce_Error);

      O_Some : constant Option := New_Some (42);
      O_None : constant Option := None;
      Result : Int_Result.Result;
   begin
      Put_Line ("Testing Ok_Or and Ok_Or_Else...");
      --  Some -> Ok
      Result := Opt_To_Result (O_Some, Not_Found);
      Assert (Int_Result.Is_Ok (Result) and then Int_Result.Value (Result) = 42,
              "Ok_Or converts Some to Ok");

      --  None -> Error with provided error code
      Result := Opt_To_Result (O_None, Invalid_Input);
      Assert (Int_Result.Is_Error (Result) and then
              Int_Result.Error (Result) = Invalid_Input,
              "Ok_Or converts None to Error");

      --  Ok_Or_Else: Some -> Ok (lazy not called)
      Result := Opt_To_Result_Lazy (O_Some);
      Assert (Int_Result.Is_Ok (Result) and then Int_Result.Value (Result) = 42,
              "Ok_Or_Else converts Some to Ok");

      --  Ok_Or_Else: None -> Error (lazy called)
      Result := Opt_To_Result_Lazy (O_None);
      Assert (Int_Result.Is_Error (Result) and then
              Int_Result.Error (Result) = Missing_Value,
              "Ok_Or_Else converts None to Error using lazy producer");
   end Test_Ok_Or;

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
   Test_And_Operator;
   Test_Xor_Operator;
   Test_Zip_With;
   Test_Flatten;
   Test_Ok_Or;

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
