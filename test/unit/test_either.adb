pragma Ada_2022;
--  ======================================================================
--  Test_Either
--  ======================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  Purpose:
--    Comprehensive unit tests for Functional.Either type.
--    Tests all 11 Either functions. Target: 90%+ code coverage.
--  ======================================================================

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with Functional.Either;
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
