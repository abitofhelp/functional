pragma Ada_2022;
--  ===========================================================================
--  SPARK_Instantiations (Body)
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  ===========================================================================

package body SPARK_Instantiations with
  SPARK_Mode => On
is

   --  ========================================================================
   --  Basic Test Functions
   --  ========================================================================

   function Test_Option_Some return Int_Option.Option is
   begin
      return Int_Option.New_Some (42);
   end Test_Option_Some;

   function Test_Option_None return Int_Option.Option is
   begin
      return Int_Option.None;
   end Test_Option_None;

   function Test_Result_Ok return Int_Result.Result is
   begin
      return Int_Result.Ok (42);
   end Test_Result_Ok;

   function Test_Result_Error return Int_Result.Result is
   begin
      return Int_Result.New_Error (Validation_Failed);
   end Test_Result_Error;

   function Test_Either_Left return String_Or_Int.Either is
   begin
      return String_Or_Int.Left (Blank_Message);
   end Test_Either_Left;

   function Test_Either_Right return String_Or_Int.Either is
   begin
      return String_Or_Int.Right (42);
   end Test_Either_Right;

   --  ========================================================================
   --  Comprehensive Test: Result Operations
   --  ========================================================================

   function Test_Result_Operations return Boolean is
      Ok_Val  : constant Int_Result.Result := Int_Result.Ok (10);
      Err_Val : constant Int_Result.Result := Int_Result.New_Error (Validation_Failed);

      --  Predicate tests
      P1 : constant Boolean := Result_Is_Ok_And_Positive (Ok_Val);
      P2 : constant Boolean := Result_Is_Ok_And_Even (Ok_Val);
      P3 : constant Boolean := Result_Is_Error_And_Validation (Err_Val);
      P4 : constant Boolean := Result_Is_Ok_Or_Positive (Ok_Val);
      P5 : constant Boolean := Result_Is_Error_Or_Validation (Err_Val);

      --  Unwrap tests
      U1 : constant Integer := Result_Unwrap_Or_Default (Ok_Val);
      U2 : constant Integer := Result_Unwrap_Or_Default (Err_Val);

      --  Map tests
      M1 : constant Int_Result.Result := Result_Map_Double (Ok_Val);
      M2 : constant Int_Result.Result := Result_Map_Increment (Ok_Val);
      M3 : constant Integer := Result_Map_Or_Double (Ok_Val, 0);
      M4 : constant Integer := Result_Map_Or_Else_Double (Ok_Val);
      M5 : constant Int_Result.Result := Result_Map_Error_To_Not_Found (Err_Val);
      M6 : constant Int_Result.Result := Result_Bimap (Ok_Val);

      --  Chain tests
      C1 : constant Int_Result.Result := Result_And_Then_Validate (Ok_Val);
      C2 : constant Int_Result.Result := Result_And_Then_Range (Ok_Val);
      C3 : constant Bool_Result.Result := Result_And_Then_To_Bool (Ok_Val);

      --  Fallback/Recovery tests
      F1 : constant Int_Result.Result := Result_Fallback_With (Err_Val);
      F2 : constant Integer := Result_Recover (Err_Val);
      F3 : constant Int_Result.Result := Result_Recover_With (Err_Val);

      --  Validation tests
      V1 : constant Int_Result.Result := Result_Ensure_Positive (Ok_Val);
      V2 : constant Int_Result.Result := Result_Ensure_In_Range (Ok_Val);
      V3 : constant Int_Result.Result := Result_With_Context (Err_Val, "test");

      --  Zip tests
      Z1 : constant Int_Result.Result := Result_Zip_Add (Ok_Val, Ok_Val);

      --  Flatten tests (flattens Nested_Result -> Nested_Result, same type)
      Nested : constant Nested_Result.Result := Nested_Result.Ok (Ok_Val);
      FL1    : constant Nested_Result.Result := Result_Flatten (Nested);

      --  Conversion tests
      CV1 : constant Int_Option.Option := Result_To_Option (Ok_Val);

      --  Suppress unused warnings by using all values
      pragma Unreferenced (P1, P2, P3, P4, P5);
      pragma Unreferenced (U1, U2);
      pragma Unreferenced (M1, M2, M3, M4, M5, M6);
      pragma Unreferenced (C1, C2, C3);
      pragma Unreferenced (F1, F2, F3);
      pragma Unreferenced (V1, V2, V3);
      pragma Unreferenced (Z1);
      pragma Unreferenced (FL1);
      pragma Unreferenced (CV1);
   begin
      return True;
   end Test_Result_Operations;

   --  ========================================================================
   --  Comprehensive Test: Option Operations
   --  ========================================================================

   function Test_Option_Operations return Boolean is
      Some_Val : constant Int_Option.Option := Int_Option.New_Some (10);
      None_Val : constant Int_Option.Option := Int_Option.None;

      --  Predicate tests
      P1 : constant Boolean := Option_Is_Some_And_Positive (Some_Val);
      P2 : constant Boolean := Option_Is_Some_And_Even (Some_Val);
      P3 : constant Boolean := Option_Is_None_Or_Positive (Some_Val);
      P4 : constant Boolean := Option_Is_None_Or_Even (None_Val);

      --  Unwrap tests
      U1 : constant Integer := Option_Unwrap_Or_Default (Some_Val);
      U2 : constant Integer := Option_Unwrap_Or_Default (None_Val);

      --  Map tests
      M1 : constant Int_Option.Option := Option_Map_Double (Some_Val);
      M2 : constant Int_Option.Option := Option_Map_Increment (Some_Val);
      M3 : constant Integer := Option_Map_Or_Double (Some_Val, 0);
      M4 : constant Integer := Option_Map_Or_Else_Double (Some_Val);

      --  Chain tests
      C1 : constant Int_Option.Option := Option_And_Then_Positive (Some_Val);
      C2 : constant Int_Option.Option := Option_And_Then_Even (Some_Val);

      --  Filter tests
      FI1 : constant Int_Option.Option := Option_Filter_Positive (Some_Val);
      FI2 : constant Int_Option.Option := Option_Filter_Even (Some_Val);
      FI3 : constant Int_Option.Option := Option_Filter_In_Range (Some_Val);

      --  Fallback tests
      F1 : constant Int_Option.Option := Option_Or_Else_With (None_Val);

      --  Zip tests
      Z1 : constant Int_Option.Option := Option_Zip_Add (Some_Val, Some_Val);

      --  Flatten tests
      Nested : constant Nested_Option.Option := Nested_Option.New_Some (Some_Val);
      FL1    : constant Int_Option.Option := Option_Flatten (Nested);

      --  Conversion tests
      CV1 : constant Int_Result.Result := Option_Ok_Or (Some_Val, Not_Found);
      CV2 : constant Int_Result.Result := Option_Ok_Or_Else (Some_Val);

      pragma Unreferenced (P1, P2, P3, P4);
      pragma Unreferenced (U1, U2);
      pragma Unreferenced (M1, M2, M3, M4);
      pragma Unreferenced (C1, C2);
      pragma Unreferenced (FI1, FI2, FI3);
      pragma Unreferenced (F1);
      pragma Unreferenced (Z1);
      pragma Unreferenced (FL1);
      pragma Unreferenced (CV1, CV2);
   begin
      return True;
   end Test_Option_Operations;

   --  ========================================================================
   --  Comprehensive Test: Either Operations
   --  ========================================================================

   function Test_Either_Operations return Boolean is
      Left_Val  : constant String_Or_Int.Either := String_Or_Int.Left (Blank_Message);
      Right_Val : constant String_Or_Int.Either := String_Or_Int.Right (10);

      --  Predicate tests
      P1 : constant Boolean := Either_Is_Left_And_Empty (Left_Val);
      P2 : constant Boolean := Either_Is_Right_And_Positive (Right_Val);
      P3 : constant Boolean := Either_Is_Left_Or_Empty (Left_Val);
      P4 : constant Boolean := Either_Is_Right_Or_Positive (Right_Val);

      --  Map tests
      M1 : constant String_Or_Int.Either := Either_Map_Left (Left_Val);
      M2 : constant String_Or_Int.Either := Either_Map_Right_Double (Right_Val);
      M3 : constant String_Or_Int.Either := Either_Map_Right_Increment (Right_Val);
      M4 : constant String_Or_Int.Either := Either_Map_Double (Right_Val);
      M5 : constant String_Or_Int.Either := Either_Bimap (Right_Val);

      --  Chain tests
      C1 : constant String_Or_Int.Either := Either_And_Then_Positive (Right_Val);

      --  Fold tests
      FO1 : constant Integer := Either_Fold_To_Int (Left_Val);
      FO2 : constant Integer := Either_Fold_To_Int (Right_Val);

      --  Merge tests (using Int_Either where L = R = Integer)
      Int_Left  : constant Int_Either.Either := Int_Either.Left (5);
      Int_Right : constant Int_Either.Either := Int_Either.Right (10);
      ME1 : constant Integer := Int_Either_Merge (Int_Left);
      ME2 : constant Integer := Int_Either_Merge (Int_Right);

      --  Conversion tests
      CV1 : constant Int_Option.Option := Either_To_Option (Right_Val);
      CV2 : constant Msg_Result.Result := Either_To_Result (Right_Val);

      --  Swap tests
      SW1 : constant Int_Or_String.Either := Either_Swap (Right_Val);

      pragma Unreferenced (P1, P2, P3, P4);
      pragma Unreferenced (M1, M2, M3, M4, M5);
      pragma Unreferenced (C1);
      pragma Unreferenced (FO1, FO2);
      pragma Unreferenced (ME1, ME2);
      pragma Unreferenced (CV1, CV2);
      pragma Unreferenced (SW1);
   begin
      return True;
   end Test_Either_Operations;

end SPARK_Instantiations;
