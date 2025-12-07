pragma Ada_2022;
--  ======================================================================
--  Test_Either
--  ======================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  Purpose:
--    Comprehensive unit tests for Functional.Either type.
--    Tests all 20 Either operations. Target: 90%+ code coverage.
--  ======================================================================

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with Functional.Either;
with Functional.Option;
with Functional.Result;
with Test_Framework;

procedure Test_Either is

   --  Use fixed-size string to avoid unconstrained type
   subtype Fixed_String is String (1 .. 20);

   package Str_Int_Either is new
     Functional.Either (L => Fixed_String, R => Integer);

   --  Swapped version for Swap test
   package Int_Str_Either is new
     Functional.Either (L => Integer, R => Fixed_String);

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
      E_Left  : constant Str_Int_Either.Either :=
        Str_Int_Either.Left ("error" & [6 .. 20 => ' ']);
      E_Right : constant Str_Int_Either.Either := Str_Int_Either.Right (42);
   begin
      Put_Line ("Testing Constructors and Predicates...");
      Assert
        (Str_Int_Either.Is_Left (E_Left),
         "Left constructor creates Left either");
      Assert
        (Str_Int_Either.Is_Right (E_Right),
         "Right constructor creates Right either");
      Assert
        (not Str_Int_Either.Is_Right (E_Left), "Left either is not Right");
      Assert
        (not Str_Int_Either.Is_Left (E_Right), "Right either is not Left");
   end Test_Constructors;

   --  ==========================================================================
   --  Test: Extractors
   --  ==========================================================================

   procedure Test_Extractors is
      E_Left  : constant Str_Int_Either.Either :=
        Str_Int_Either.Left ("error message" & [14 .. 20 => ' ']);
      E_Right : constant Str_Int_Either.Either := Str_Int_Either.Right (42);
      L_Val   : Fixed_String;
      R_Val   : Integer;
   begin
      Put_Line ("Testing Extractors...");
      L_Val := Str_Int_Either.Left_Value (E_Left);
      Assert
        (L_Val (1 .. 13) = "error message",
         "Left_Value extractor returns correct value");

      R_Val := Str_Int_Either.Right_Value (E_Right);
      Assert (R_Val = 42, "Right_Value extractor returns correct value");
   end Test_Extractors;

   --  ==========================================================================
   --  Test: Practical use case - Parse or keep as string
   --  ==========================================================================

   procedure Test_Parse_Or_Keep is
      function Parse_Or_Keep (Input : String) return Str_Int_Either.Either is
         Padded : Fixed_String := (others => ' ');
      begin
         return Str_Int_Either.Right (Integer'Value (Input));
      exception
         when Constraint_Error =>
            Padded (1 .. Input'Length) := Input;
            return Str_Int_Either.Left (Padded);
      end Parse_Or_Keep;

      E1 : constant Str_Int_Either.Either := Parse_Or_Keep ("42");
      E2 : constant Str_Int_Either.Either := Parse_Or_Keep ("not_a_number");
   begin
      Put_Line ("Testing Parse or Keep use case...");
      Assert
        (Str_Int_Either.Is_Right (E1)
         and then Str_Int_Either.Right_Value (E1) = 42,
         "Parse succeeds for valid integer");

      Assert
        (Str_Int_Either.Is_Left (E2)
         and then Str_Int_Either.Left_Value (E2) (1 .. 12) = "not_a_number",
         "Parse fails and keeps original string");
   end Test_Parse_Or_Keep;

   --  ==========================================================================
   --  Test: Map_Left (transform Left value only)
   --  ==========================================================================

   procedure Test_Map_Left is
      E_Left  : constant Str_Int_Either.Either :=
        Str_Int_Either.Left ("hello" & [6 .. 20 => ' ']);
      E_Right : constant Str_Int_Either.Either := Str_Int_Either.Right (42);

      function To_Upper (S : Fixed_String) return Fixed_String is
         Result : Fixed_String := S;
      begin
         for I in Result'Range loop
            if Result (I) >= 'a' and then Result (I) <= 'z' then
               Result (I) := Character'Val (Character'Pos (Result (I)) - 32);
            end if;
         end loop;
         return Result;
      end To_Upper;

      function Transform is new Str_Int_Either.Map_Left (F => To_Upper);

      Result_L : Str_Int_Either.Either;
      Result_R : Str_Int_Either.Either;
   begin
      Put_Line ("Testing Map_Left...");
      Result_L := Transform (E_Left);
      Assert
        (Str_Int_Either.Is_Left (Result_L)
         and then Str_Int_Either.Left_Value (Result_L) (1 .. 5) = "HELLO",
         "Map_Left transforms Left value");

      Result_R := Transform (E_Right);
      Assert
        (Str_Int_Either.Is_Right (Result_R)
         and then Str_Int_Either.Right_Value (Result_R) = 42,
         "Map_Left passes through Right value unchanged");
   end Test_Map_Left;

   --  ==========================================================================
   --  Test: Map_Right (transform Right value only)
   --  ==========================================================================

   procedure Test_Map_Right is
      E_Left  : constant Str_Int_Either.Either :=
        Str_Int_Either.Left ("hello" & [6 .. 20 => ' ']);
      E_Right : constant Str_Int_Either.Either := Str_Int_Either.Right (5);

      function Double (X : Integer) return Integer is (X * 2);

      function Transform is new Str_Int_Either.Map_Right (F => Double);

      Result_L : Str_Int_Either.Either;
      Result_R : Str_Int_Either.Either;
   begin
      Put_Line ("Testing Map_Right...");
      Result_L := Transform (E_Left);
      Assert
        (Str_Int_Either.Is_Left (Result_L)
         and then Str_Int_Either.Left_Value (Result_L) (1 .. 5) = "hello",
         "Map_Right passes through Left value unchanged");

      Result_R := Transform (E_Right);
      Assert
        (Str_Int_Either.Is_Right (Result_R)
         and then Str_Int_Either.Right_Value (Result_R) = 10,
         "Map_Right transforms Right value");
   end Test_Map_Right;

   --  ==========================================================================
   --  Test: Bimap (transform both Left and Right)
   --  ==========================================================================

   procedure Test_Bimap is
      E_Left  : constant Str_Int_Either.Either :=
        Str_Int_Either.Left ("hello" & [6 .. 20 => ' ']);
      E_Right : constant Str_Int_Either.Either := Str_Int_Either.Right (5);

      function To_Upper (S : Fixed_String) return Fixed_String is
         Result : Fixed_String := S;
      begin
         for I in Result'Range loop
            if Result (I) >= 'a' and then Result (I) <= 'z' then
               Result (I) := Character'Val (Character'Pos (Result (I)) - 32);
            end if;
         end loop;
         return Result;
      end To_Upper;

      function Double (X : Integer) return Integer
      is (X * 2);

      function Transform is new
        Str_Int_Either.Bimap (Map_L => To_Upper, Map_R => Double);

      Result_L : Str_Int_Either.Either;
      Result_R : Str_Int_Either.Either;
   begin
      Put_Line ("Testing Bimap...");
      Result_L := Transform (E_Left);
      Assert
        (Str_Int_Either.Is_Left (Result_L)
         and then Str_Int_Either.Left_Value (Result_L) (1 .. 5) = "HELLO",
         "Bimap transforms Left value");

      Result_R := Transform (E_Right);
      Assert
        (Str_Int_Either.Is_Right (Result_R)
         and then Str_Int_Either.Right_Value (Result_R) = 10,
         "Bimap transforms Right value");
   end Test_Bimap;

   --  ==========================================================================
   --  Test: Fold (reduce to single value)
   --  ==========================================================================

   procedure Test_Fold is
      E_Left  : constant Str_Int_Either.Either :=
        Str_Int_Either.Left ("hello" & [6 .. 20 => ' ']);
      E_Right : constant Str_Int_Either.Either := Str_Int_Either.Right (42);

      function String_Length (S : Fixed_String) return Natural is
         Len : Natural := 0;
      begin
         for C of S loop
            exit when C = ' ';
            Len := Len + 1;
         end loop;
         return Len;
      end String_Length;

      function Int_To_Nat (I : Integer) return Natural
      is (abs I);

      function To_Natural is new
        Str_Int_Either.Fold
          (U        => Natural,
           On_Left  => String_Length,
           On_Right => Int_To_Nat);

      Result_L : Natural;
      Result_R : Natural;
   begin
      Put_Line ("Testing Fold...");
      Result_L := To_Natural (E_Left);
      Assert (Result_L = 5, "Fold handles Left value");

      Result_R := To_Natural (E_Right);
      Assert (Result_R = 42, "Fold handles Right value");
   end Test_Fold;

   --  ==========================================================================
   --  Test: Map (Right-biased convenience transform)
   --  ==========================================================================

   procedure Test_Map is
      E_Left  : constant Str_Int_Either.Either :=
        Str_Int_Either.Left ("hello" & [6 .. 20 => ' ']);
      E_Right : constant Str_Int_Either.Either := Str_Int_Either.Right (5);

      function Double (X : Integer) return Integer is (X * 2);

      function Transform is new Str_Int_Either.Map (F => Double);

      Result_L : Str_Int_Either.Either;
      Result_R : Str_Int_Either.Either;
   begin
      Put_Line ("Testing Map...");
      Result_L := Transform (E_Left);
      Assert
        (Str_Int_Either.Is_Left (Result_L)
         and then Str_Int_Either.Left_Value (Result_L) (1 .. 5) = "hello",
         "Map passes through Left value unchanged");

      Result_R := Transform (E_Right);
      Assert
        (Str_Int_Either.Is_Right (Result_R)
         and then Str_Int_Either.Right_Value (Result_R) = 10,
         "Map transforms Right value");
   end Test_Map;

   --  ==========================================================================
   --  Test: Swap (exchange Left and Right)
   --  ==========================================================================

   procedure Test_Swap is
      E_Left  : constant Str_Int_Either.Either :=
        Str_Int_Either.Left ("hello" & [6 .. 20 => ' ']);
      E_Right : constant Str_Int_Either.Either := Str_Int_Either.Right (42);

      function Do_Swap is new
        Str_Int_Either.Swap
          (Either_Swapped => Int_Str_Either.Either,
           Make_Left      => Int_Str_Either.Left,
           Make_Right     => Int_Str_Either.Right);

      Swapped_L : Int_Str_Either.Either;
      Swapped_R : Int_Str_Either.Either;
   begin
      Put_Line ("Testing Swap...");
      Swapped_L := Do_Swap (E_Left);
      Assert
        (Int_Str_Either.Is_Right (Swapped_L)
         and then Int_Str_Either.Right_Value (Swapped_L) (1 .. 5) = "hello",
         "Swap turns Left into Right");

      Swapped_R := Do_Swap (E_Right);
      Assert
        (Int_Str_Either.Is_Left (Swapped_R)
         and then Int_Str_Either.Left_Value (Swapped_R) = 42,
         "Swap turns Right into Left");
   end Test_Swap;

   --  ==========================================================================
   --  Test: And_Then (Right-biased monadic bind)
   --  ==========================================================================

   procedure Test_And_Then is
      E_Left  : constant Str_Int_Either.Either :=
        Str_Int_Either.Left ("error" & [6 .. 20 => ' ']);
      E_Right : constant Str_Int_Either.Either := Str_Int_Either.Right (10);

      --  Function that returns Right on success
      function Safe_Halve (X : Integer) return Str_Int_Either.Either is
      begin
         if X mod 2 = 0 then
            return Str_Int_Either.Right (X / 2);
         else
            return Str_Int_Either.Left ("not even" & [9 .. 20 => ' ']);
         end if;
      end Safe_Halve;

      function Chain is new Str_Int_Either.And_Then (F => Safe_Halve);

      Result_L : Str_Int_Either.Either;
      Result_R : Str_Int_Either.Either;
      E_Odd    : constant Str_Int_Either.Either := Str_Int_Either.Right (7);
      Result_F : Str_Int_Either.Either;
   begin
      Put_Line ("Testing And_Then...");

      --  And_Then on Left passes through unchanged
      Result_L := Chain (E_Left);
      Assert
        (Str_Int_Either.Is_Left (Result_L)
         and then Str_Int_Either.Left_Value (Result_L) (1 .. 5) = "error",
         "And_Then short-circuits on Left");

      --  And_Then on Right with success
      Result_R := Chain (E_Right);
      Assert
        (Str_Int_Either.Is_Right (Result_R)
         and then Str_Int_Either.Right_Value (Result_R) = 5,
         "And_Then chains Right -> Right");

      --  And_Then on Right with function returning Left
      Result_F := Chain (E_Odd);
      Assert
        (Str_Int_Either.Is_Left (Result_F)
         and then Str_Int_Either.Left_Value (Result_F) (1 .. 8) = "not even",
         "And_Then propagates Left from function");
   end Test_And_Then;

   --  ==========================================================================
   --  Test: Is_Left_And (predicate on Left value)
   --  ==========================================================================

   procedure Test_Is_Left_And is
      E_Left_Err : constant Str_Int_Either.Either :=
        Str_Int_Either.Left ("error" & [6 .. 20 => ' ']);
      E_Left_Ok  : constant Str_Int_Either.Either :=
        Str_Int_Either.Left ("ok" & [3 .. 20 => ' ']);
      E_Right    : constant Str_Int_Either.Either := Str_Int_Either.Right (42);

      function Starts_With_E (S : Fixed_String) return Boolean
      is (S (1) = 'e');

      function Is_Short (S : Fixed_String) return Boolean
      is (S (3) = ' ');

      function Check_Starts_E is new
        Str_Int_Either.Is_Left_And (Pred => Starts_With_E);
      function Check_Short is new
        Str_Int_Either.Is_Left_And (Pred => Is_Short);
   begin
      Put_Line ("Testing Is_Left_And...");

      --  Left with predicate satisfied
      Assert (Check_Starts_E (E_Left_Err),
              "Is_Left_And returns True when Left and predicate holds");
      Assert (Check_Short (E_Left_Ok),
              "Is_Left_And returns True when Left('ok') is short");

      --  Left with predicate not satisfied
      Assert (not Check_Starts_E (E_Left_Ok),
              "Is_Left_And returns False when Left but predicate fails");
      Assert (not Check_Short (E_Left_Err),
              "Is_Left_And returns False when Left('error') is not short");

      --  Right case: always False
      Assert (not Check_Starts_E (E_Right),
              "Is_Left_And returns False for Right");
   end Test_Is_Left_And;

   --  ==========================================================================
   --  Test: Is_Right_And (predicate on Right value)
   --  ==========================================================================

   procedure Test_Is_Right_And is
      E_Left    : constant Str_Int_Either.Either :=
        Str_Int_Either.Left ("error" & [6 .. 20 => ' ']);
      E_Right42 : constant Str_Int_Either.Either := Str_Int_Either.Right (42);
      E_Right7  : constant Str_Int_Either.Either := Str_Int_Either.Right (7);

      function Is_Even (X : Integer) return Boolean
      is (X mod 2 = 0);

      function Is_Large (X : Integer) return Boolean
      is (X > 10);

      function Check_Even is new
        Str_Int_Either.Is_Right_And (Pred => Is_Even);
      function Check_Large is new
        Str_Int_Either.Is_Right_And (Pred => Is_Large);
   begin
      Put_Line ("Testing Is_Right_And...");

      --  Right with predicate satisfied
      Assert (Check_Even (E_Right42),
              "Is_Right_And returns True when Right(42) is even");
      Assert (Check_Large (E_Right42),
              "Is_Right_And returns True when Right(42) is large");

      --  Right with predicate not satisfied
      Assert (not Check_Even (E_Right7),
              "Is_Right_And returns False when Right(7) is odd");
      Assert (not Check_Large (E_Right7),
              "Is_Right_And returns False when Right(7) is not large");

      --  Left case: always False
      Assert (not Check_Even (E_Left),
              "Is_Right_And returns False for Left");
   end Test_Is_Right_And;

   --  ==========================================================================
   --  Test: Is_Left_Or (Right or predicate holds on Left)
   --  ==========================================================================

   procedure Test_Is_Left_Or is
      E_Left_Err : constant Str_Int_Either.Either :=
        Str_Int_Either.Left ("error" & [6 .. 20 => ' ']);
      E_Left_Ok  : constant Str_Int_Either.Either :=
        Str_Int_Either.Left ("ok" & [3 .. 20 => ' ']);
      E_Right    : constant Str_Int_Either.Either := Str_Int_Either.Right (42);

      function Starts_With_E (S : Fixed_String) return Boolean
      is (S (1) = 'e');

      function Is_Short (S : Fixed_String) return Boolean
      is (S (3) = ' ');

      function Check_Starts_E is new
        Str_Int_Either.Is_Left_Or (Pred => Starts_With_E);
      function Check_Short is new
        Str_Int_Either.Is_Left_Or (Pred => Is_Short);
   begin
      Put_Line ("Testing Is_Left_Or...");

      --  Right case: always True (lenient - Right is acceptable)
      Assert (Check_Starts_E (E_Right),
              "Is_Left_Or returns True for Right");
      Assert (Check_Short (E_Right),
              "Is_Left_Or returns True for Right (different predicate)");

      --  Left with predicate satisfied: True
      Assert (Check_Starts_E (E_Left_Err),
              "Is_Left_Or returns True when Left and predicate holds");
      Assert (Check_Short (E_Left_Ok),
              "Is_Left_Or returns True when Left('ok') is short");

      --  Left with predicate not satisfied: False
      Assert (not Check_Starts_E (E_Left_Ok),
              "Is_Left_Or returns False when Left but predicate fails");
      Assert (not Check_Short (E_Left_Err),
              "Is_Left_Or returns False when Left('error') is not short");
   end Test_Is_Left_Or;

   --  ==========================================================================
   --  Test: Is_Right_Or (Left or predicate holds on Right)
   --  ==========================================================================

   procedure Test_Is_Right_Or is
      E_Left    : constant Str_Int_Either.Either :=
        Str_Int_Either.Left ("error" & [6 .. 20 => ' ']);
      E_Right42 : constant Str_Int_Either.Either := Str_Int_Either.Right (42);
      E_Right7  : constant Str_Int_Either.Either := Str_Int_Either.Right (7);

      function Is_Even (X : Integer) return Boolean
      is (X mod 2 = 0);

      function Is_Large (X : Integer) return Boolean
      is (X > 10);

      function Check_Even is new
        Str_Int_Either.Is_Right_Or (Pred => Is_Even);
      function Check_Large is new
        Str_Int_Either.Is_Right_Or (Pred => Is_Large);
   begin
      Put_Line ("Testing Is_Right_Or...");

      --  Left case: always True (lenient - Left is acceptable)
      Assert (Check_Even (E_Left),
              "Is_Right_Or returns True for Left");
      Assert (Check_Large (E_Left),
              "Is_Right_Or returns True for Left (different predicate)");

      --  Right with predicate satisfied: True
      Assert (Check_Even (E_Right42),
              "Is_Right_Or returns True when Right(42) is even");
      Assert (Check_Large (E_Right42),
              "Is_Right_Or returns True when Right(42) is large");

      --  Right with predicate not satisfied: False
      Assert (not Check_Even (E_Right7),
              "Is_Right_Or returns False when Right(7) is odd");
      Assert (not Check_Large (E_Right7),
              "Is_Right_Or returns False when Right(7) is not large");
   end Test_Is_Right_Or;

   --  ==========================================================================
   --  Test: Contains (check if Right equals value)
   --  ==========================================================================

   procedure Test_Contains is
      E_Left  : constant Str_Int_Either.Either :=
        Str_Int_Either.Left ("error" & [6 .. 20 => ' ']);
      E_Right : constant Str_Int_Either.Either := Str_Int_Either.Right (42);
   begin
      Put_Line ("Testing Contains...");

      Assert
        (Str_Int_Either.Contains (E_Right, 42),
         "Contains returns True when Right equals value");
      Assert
        (not Str_Int_Either.Contains (E_Right, 99),
         "Contains returns False when Right differs");
      Assert
        (not Str_Int_Either.Contains (E_Left, 42),
         "Contains returns False for Left");

      --  Test "=" operator alias
      Assert
        (Str_Int_Either."=" (E_Right, 42),
         """="" operator works for Right match");
      Assert
        (not Str_Int_Either."=" (E_Left, 42),
         """="" operator returns False for Left");
   end Test_Contains;

   --  ==========================================================================
   --  Test: Get_Or_Else (get Right or default)
   --  ==========================================================================

   procedure Test_Get_Or_Else is
      E_Left  : constant Str_Int_Either.Either :=
        Str_Int_Either.Left ("error" & [6 .. 20 => ' ']);
      E_Right : constant Str_Int_Either.Either := Str_Int_Either.Right (42);
   begin
      Put_Line ("Testing Get_Or_Else...");

      Assert
        (Str_Int_Either.Get_Or_Else (E_Right, 0) = 42,
         "Get_Or_Else returns Right value when present");
      Assert
        (Str_Int_Either.Get_Or_Else (E_Left, 99) = 99,
         "Get_Or_Else returns default for Left");
   end Test_Get_Or_Else;

   --  ==========================================================================
   --  Test: Merge (extract when both types same)
   --  ==========================================================================

   procedure Test_Merge is
      --  For Merge, we need L = R, so use Integer for both
      package Int_Int_Either is new
        Functional.Either (L => Integer, R => Integer);

      E_Left  : constant Int_Int_Either.Either := Int_Int_Either.Left (10);
      E_Right : constant Int_Int_Either.Either := Int_Int_Either.Right (42);

      function Id (X : Integer) return Integer is (X);

      function Do_Merge is new
        Int_Int_Either.Merge (T => Integer, From_Left => Id, From_Right => Id);
   begin
      Put_Line ("Testing Merge...");

      Assert (Do_Merge (E_Left) = 10, "Merge extracts Left value");
      Assert (Do_Merge (E_Right) = 42, "Merge extracts Right value");
   end Test_Merge;

   --  ==========================================================================
   --  Test: To_Option (convert to Option)
   --  ==========================================================================

   procedure Test_To_Option is
      package Int_Option is new Functional.Option (T => Integer);

      E_Left  : constant Str_Int_Either.Either :=
        Str_Int_Either.Left ("error" & [6 .. 20 => ' ']);
      E_Right : constant Str_Int_Either.Either := Str_Int_Either.Right (42);

      function Convert is new
        Str_Int_Either.To_Option
          (Option_Type => Int_Option.Option,
           Make_Some   => Int_Option.New_Some,
           Make_None   => Int_Option.None);

      Result_L : Int_Option.Option;
      Result_R : Int_Option.Option;
   begin
      Put_Line ("Testing To_Option...");

      Result_R := Convert (E_Right);
      Assert
        (Int_Option.Is_Some (Result_R)
         and then Int_Option.Value (Result_R) = 42,
         "To_Option converts Right to Some");

      Result_L := Convert (E_Left);
      Assert (Int_Option.Is_None (Result_L), "To_Option converts Left to None");
   end Test_To_Option;

   --  ==========================================================================
   --  Test: To_Result (convert to Result)
   --  ==========================================================================

   procedure Test_To_Result is
      package Int_Result is new
        Functional.Result (T => Integer, E => Fixed_String);

      E_Left  : constant Str_Int_Either.Either :=
        Str_Int_Either.Left ("error message" & [14 .. 20 => ' ']);
      E_Right : constant Str_Int_Either.Either := Str_Int_Either.Right (42);

      function Convert is new
        Str_Int_Either.To_Result
          (Result_Type => Int_Result.Result,
           Make_Ok     => Int_Result.Ok,
           Make_Error  => Int_Result.New_Error);

      Result_L : Int_Result.Result;
      Result_R : Int_Result.Result;
   begin
      Put_Line ("Testing To_Result...");

      Result_R := Convert (E_Right);
      Assert
        (Int_Result.Is_Ok (Result_R)
         and then Int_Result.Value (Result_R) = 42,
         "To_Result converts Right to Ok");

      Result_L := Convert (E_Left);
      Assert
        (Int_Result.Is_Error (Result_L)
         and then Int_Result.Error (Result_L) (1 .. 13) = "error message",
         "To_Result converts Left to Error");
   end Test_To_Result;

begin
   Put_Line ("======================================");
   Put_Line ("  Functional.Either Unit Tests");
   Put_Line ("  Target: 90%+ Coverage");
   Put_Line ("======================================");
   New_Line;

   Test_Constructors;
   Test_Extractors;
   Test_Parse_Or_Keep;
   Test_Map_Left;
   Test_Map_Right;
   Test_Bimap;
   Test_Map;
   Test_Swap;
   Test_And_Then;
   Test_Fold;
   Test_Is_Left_And;
   Test_Is_Right_And;
   Test_Is_Left_Or;
   Test_Is_Right_Or;
   Test_Contains;
   Test_Get_Or_Else;
   Test_Merge;
   Test_To_Option;
   Test_To_Result;

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
end Test_Either;
