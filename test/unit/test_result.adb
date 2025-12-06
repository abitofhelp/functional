pragma Ada_2022;
--  ======================================================================
--  Test_Result
--  ======================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  Purpose:
--    Comprehensive unit tests for Functional.Result monad.
--    Tests all 20 Result operations. 43 tests. Target: 90%+ coverage.
--  ======================================================================

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with Functional.Result;
with Test_Framework;

procedure Test_Result is

   type Error_Kind is (Validation_Error, Parse_Error, IO_Error);

   --  Use bounded string to avoid unconstrained type in variant record
   subtype Error_Message is String (1 .. 100);

   type Error is record
      Kind    : Error_Kind;
      Message : Error_Message;
      Len     : Natural := 0;
   end record;

   package Int_Result is new Functional.Result (T => Integer, E => Error);

   --  Second Result type for And_Then_Into tests (type-changing chain)
   subtype Result_String is String (1 .. 20);
   package Str_Result is new Functional.Result (T => Result_String, E => Error);

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
      R_Ok  : constant Int_Result.Result := Int_Result.Ok (42);
      R_Err : constant Int_Result.Result :=
        Int_Result.Err ((Parse_Error, [others => ' '], 0));
   begin
      Put_Line ("Testing Constructors and Predicates...");
      Assert (Int_Result.Is_Ok (R_Ok), "Ok constructor creates Ok result");
      Assert (Int_Result.Is_Err (R_Err), "Err constructor creates Err result");
      Assert (not Int_Result.Is_Err (R_Ok), "Ok result is not Err");
      Assert (not Int_Result.Is_Ok (R_Err), "Err result is not Ok");
   end Test_Constructors;

   --  ==========================================================================
   --  Test: From_Error (boundary constructor)
   --  ==========================================================================

   procedure Test_From_Error is
      Test_Err : constant Error := (IO_Error, "boundary error" & [15 .. 100 => ' '], 14);
      R_Err    : constant Int_Result.Result := Int_Result.From_Error (Test_Err);
   begin
      Put_Line ("Testing From_Error...");
      Assert (Int_Result.Is_Err (R_Err), "From_Error creates Err result");
      Assert
        (Int_Result.Error (R_Err).Kind = IO_Error,
         "From_Error preserves error kind");
      Assert
        (Int_Result.Error (R_Err).Message (1 .. 14) = "boundary error",
         "From_Error preserves error message");
   end Test_From_Error;

   --  ==========================================================================
   --  Test: Extractors
   --  ==========================================================================

   procedure Test_Extractors is
      R_Ok    : constant Int_Result.Result := Int_Result.Ok (42);
      R_Err   : constant Int_Result.Result :=
        Int_Result.Err ((Validation_Error, [others => 'X'], 5));
      Val     : Integer;
      Err_Val : Error;
   begin
      Put_Line ("Testing Extractors...");
      Val := Int_Result.Value (R_Ok);
      Assert (Val = 42, "Value extractor returns correct value");

      Err_Val := Int_Result.Error (R_Err);
      Assert
        (Err_Val.Kind = Validation_Error,
         "Error extractor returns correct error");
   end Test_Extractors;

   --  ==========================================================================
   --  Test: Expect (extract with custom error message)
   --  ==========================================================================

   procedure Test_Expect is
      R_Ok : constant Int_Result.Result := Int_Result.Ok (42);
      Val  : Integer;
   begin
      Put_Line ("Testing Expect...");
      Val := Int_Result.Expect (R_Ok, "Expected Ok value");
      Assert (Val = 42, "Expect returns value for Ok result");
      --  Note: Cannot test Err case as it raises Program_Error
   end Test_Expect;

   --  ==========================================================================
   --  Test: Unwrap_Or and Unwrap_Or_With
   --  ==========================================================================

   procedure Test_Unwrap is
      R_Ok  : constant Int_Result.Result := Int_Result.Ok (42);
      R_Err : constant Int_Result.Result :=
        Int_Result.Err ((Parse_Error, [others => ' '], 0));

      function Get_Default return Integer
      is (999);
      function Unwrap_With is new Int_Result.Unwrap_Or_With (F => Get_Default);
   begin
      Put_Line ("Testing Unwrap_Or and Unwrap_Or_With...");
      Assert
        (Int_Result.Unwrap_Or (R_Ok, 0) = 42,
         "Unwrap_Or returns value for Ok");
      Assert
        (Int_Result.Unwrap_Or (R_Err, 99) = 99,
         "Unwrap_Or returns default for Err");

      Assert (Unwrap_With (R_Ok) = 42, "Unwrap_Or_With returns value for Ok");
      Assert
        (Unwrap_With (R_Err) = 999,
         "Unwrap_Or_With calls lazy default for Err");
   end Test_Unwrap;

   --  ==========================================================================
   --  Test: Map (transform Ok value)
   --  ==========================================================================

   procedure Test_Map is
      R_Ok  : constant Int_Result.Result := Int_Result.Ok (5);
      R_Err : constant Int_Result.Result :=
        Int_Result.Err ((Parse_Error, [others => ' '], 0));

      function Double (X : Integer) return Integer
      is (X * 2);

      function Transform is new Int_Result.Map (F => Double);

      Result : Int_Result.Result;
   begin
      Put_Line ("Testing Map...");
      Result := Transform (R_Ok);
      Assert
        (Int_Result.Is_Ok (Result) and then Int_Result.Value (Result) = 10,
         "Map transforms Ok value");

      Result := Transform (R_Err);
      Assert (Int_Result.Is_Err (Result), "Map leaves Err unchanged");
   end Test_Map;

   --  ==========================================================================
   --  Test: And_Then (monadic bind/chain)
   --  ==========================================================================

   procedure Test_And_Then is
      R_Ok  : constant Int_Result.Result := Int_Result.Ok (5);
      R_Err : constant Int_Result.Result :=
        Int_Result.Err ((Parse_Error, [others => ' '], 0));

      function Double (X : Integer) return Int_Result.Result
      is (Int_Result.Ok (X * 2));

      function Validate_Positive (X : Integer) return Int_Result.Result is
      begin
         if X > 0 then
            return Int_Result.Ok (X);
         else
            return Int_Result.Err ((Validation_Error, [others => ' '], 0));
         end if;
      end Validate_Positive;

      function Chain_Double is new Int_Result.And_Then (F => Double);
      function Chain_Validate is new
        Int_Result.And_Then (F => Validate_Positive);

      Result : Int_Result.Result;
   begin
      Put_Line ("Testing And_Then...");
      Result := Chain_Double (R_Ok);
      Assert
        (Int_Result.Is_Ok (Result) and then Int_Result.Value (Result) = 10,
         "And_Then chains Ok -> Ok");

      Result := Chain_Double (R_Err);
      Assert (Int_Result.Is_Err (Result), "And_Then short-circuits on Err");

      Result := Chain_Validate (Int_Result.Ok (-5));
      Assert
        (Int_Result.Is_Err (Result), "And_Then propagates Err from function");
   end Test_And_Then;

   --  ==========================================================================
   --  Test: And_Then_Into (type-changing monadic chain)
   --  ==========================================================================

   procedure Test_And_Then_Into is
      R_Ok  : constant Int_Result.Result := Int_Result.Ok (42);
      R_Err : constant Int_Result.Result :=
        Int_Result.Err ((Parse_Error, [others => ' '], 0));

      function Int_To_Str (X : Integer) return Str_Result.Result is
         Img : constant String := Integer'Image (X);
         Res : Result_String   := [others => ' '];
      begin
         Res (1 .. Img'Length) := Img;
         return Str_Result.Ok (Res);
      end Int_To_Str;

      function Int_To_Str_Fail (X : Integer) return Str_Result.Result is
         pragma Unreferenced (X);
      begin
         return Str_Result.Err ((Validation_Error, [others => ' '], 0));
      end Int_To_Str_Fail;

      function Chain_To_Str is new Int_Result.And_Then_Into
        (Result_U => Str_Result.Result,
         Err_U    => Str_Result.Err,
         F        => Int_To_Str);

      function Chain_To_Str_Fail is new Int_Result.And_Then_Into
        (Result_U => Str_Result.Result,
         Err_U    => Str_Result.Err,
         F        => Int_To_Str_Fail);

      Result : Str_Result.Result;
   begin
      Put_Line ("Testing And_Then_Into...");

      --  Test Ok path: transforms Int -> String
      Result := Chain_To_Str (R_Ok);
      Assert
        (Str_Result.Is_Ok (Result),
         "And_Then_Into transforms Ok to different type");
      Assert
        (Str_Result.Value (Result) (1 .. 3) = " 42",
         "And_Then_Into preserves transformed value");

      --  Test Err path: propagates original error
      Result := Chain_To_Str (R_Err);
      Assert
        (Str_Result.Is_Err (Result),
         "And_Then_Into propagates Err through type change");
      Assert
        (Str_Result.Error (Result).Kind = Parse_Error,
         "And_Then_Into preserves error kind through type change");

      --  Test function returning Err
      Result := Chain_To_Str_Fail (R_Ok);
      Assert
        (Str_Result.Is_Err (Result),
         "And_Then_Into propagates Err from function");
   end Test_And_Then_Into;

   --  ==========================================================================
   --  Test: Map_Err
   --  ==========================================================================

   procedure Test_Map_Err is
      R_Err : constant Int_Result.Result :=
        Int_Result.Err ((Parse_Error, [others => 'A'], 5));

      function Change_Kind (E : Error) return Error
      is ((IO_Error, E.Message, E.Len));

      function Transform_Err is new Int_Result.Map_Err (F => Change_Kind);

      Result : Int_Result.Result;
   begin
      Put_Line ("Testing Map_Err...");
      Result := Transform_Err (R_Err);
      Assert
        (Int_Result.Is_Err (Result)
         and then Int_Result.Error (Result).Kind = IO_Error,
         "Map_Err transforms error type");

      Result := Transform_Err (Int_Result.Ok (42));
      Assert (Int_Result.Is_Ok (Result), "Map_Err leaves Ok unchanged");
   end Test_Map_Err;

   --  ==========================================================================
   --  Test: Bimap (transform both Ok and Err)
   --  ==========================================================================

   procedure Test_Bimap is
      R_Ok  : constant Int_Result.Result := Int_Result.Ok (5);
      R_Err : constant Int_Result.Result :=
        Int_Result.Err ((Parse_Error, [others => 'A'], 5));

      function Double (X : Integer) return Integer
      is (X * 2);

      function Change_Kind (E : Error) return Error
      is ((IO_Error, E.Message, E.Len));

      function Transform is new
        Int_Result.Bimap (Map_Ok => Double, Map_Error => Change_Kind);

      Result : Int_Result.Result;
   begin
      Put_Line ("Testing Bimap...");
      Result := Transform (R_Ok);
      Assert
        (Int_Result.Is_Ok (Result) and then Int_Result.Value (Result) = 10,
         "Bimap transforms Ok value");

      Result := Transform (R_Err);
      Assert
        (Int_Result.Is_Err (Result)
         and then Int_Result.Error (Result).Kind = IO_Error,
         "Bimap transforms Err value");
   end Test_Bimap;

   --  ==========================================================================
   --  Test: Fallback and Fallback_With
   --  ==========================================================================

   procedure Test_Fallback is
      R_Ok1 : constant Int_Result.Result := Int_Result.Ok (10);
      R_Ok2 : constant Int_Result.Result := Int_Result.Ok (20);
      R_Err : constant Int_Result.Result :=
        Int_Result.Err ((Parse_Error, [others => ' '], 0));

      function Get_Backup return Int_Result.Result
      is (Int_Result.Ok (99));
      function Fallback_Lazy is new Int_Result.Fallback_With (F => Get_Backup);
   begin
      Put_Line ("Testing Fallback and Fallback_With...");
      Assert
        (Int_Result.Value (Int_Result.Fallback (R_Ok1, R_Ok2)) = 10,
         "Fallback returns first Ok");
      Assert
        (Int_Result.Value (Int_Result.Fallback (R_Err, R_Ok2)) = 20,
         "Fallback returns second if first is Err");

      Assert
        (Int_Result.Value (Fallback_Lazy (R_Ok1)) = 10,
         "Fallback_With returns Ok without calling function");
      Assert
        (Int_Result.Value (Fallback_Lazy (R_Err)) = 99,
         "Fallback_With calls lazy backup on Err");
   end Test_Fallback;

   --  ==========================================================================
   --  Test: Recover and Recover_With
   --  ==========================================================================

   procedure Test_Recover is
      R_Ok  : constant Int_Result.Result := Int_Result.Ok (42);
      R_Err : constant Int_Result.Result :=
        Int_Result.Err ((Parse_Error, [others => ' '], 0));

      function Handle_Error (E : Error) return Integer
      is (case E.Kind is
            when Parse_Error => -1,
            when Validation_Error => -2,
            when IO_Error => -3);

      function Handle_To_Result (E : Error) return Int_Result.Result
      is (Int_Result.Ok (Handle_Error (E)));

      function Recover_To_Int is new
        Int_Result.Recover (Handle => Handle_Error);
      function Recover_To_Res is new
        Int_Result.Recover_With (Handle => Handle_To_Result);
   begin
      Put_Line ("Testing Recover and Recover_With...");
      Assert
        (Recover_To_Int (R_Ok) = 42, "Recover returns Ok value unchanged");
      Assert (Recover_To_Int (R_Err) = -1, "Recover converts Err to value");

      Assert
        (Int_Result.Value (Recover_To_Res (R_Err)) = -1,
         "Recover_With converts Err to Result");
   end Test_Recover;

   --  ==========================================================================
   --  Test: Ensure (validation)
   --  ==========================================================================

   procedure Test_Ensure is
      function Is_Positive (X : Integer) return Boolean
      is (X > 0);

      function To_Err (X : Integer) return Error is
         pragma Unreferenced (X);
      begin
         return (Validation_Error, [others => ' '], 0);
      end To_Err;

      function Validate is new
        Int_Result.Ensure (Pred => Is_Positive, To_Error => To_Err);

      Result : Int_Result.Result;
   begin
      Put_Line ("Testing Ensure...");
      Result := Validate (Int_Result.Ok (10));
      Assert (Int_Result.Is_Ok (Result), "Ensure keeps Ok if predicate holds");

      Result := Validate (Int_Result.Ok (-5));
      Assert
        (Int_Result.Is_Err (Result),
         "Ensure converts to Err if predicate fails");

      Result := Validate (Int_Result.Err ((Parse_Error, [others => ' '], 0)));
      Assert (Int_Result.Is_Err (Result), "Ensure leaves Err unchanged");
   end Test_Ensure;

   --  ==========================================================================
   --  Test: With_Context (error breadcrumbs)
   --  ==========================================================================

   procedure Test_With_Context is
      R_Err : constant Int_Result.Result :=
        Int_Result.Err ((IO_Error, "file not found" & [15 .. 100 => ' '], 14));

      function Append (E : Error; Msg : String) return Error is
         New_Msg : constant String := E.Message (1 .. E.Len) & " :: " & Msg;
      begin
         return
           (E.Kind,
            New_Msg & [New_Msg'Length + 1 .. 100 => ' '],
            New_Msg'Length);
      end Append;

      function Add_Context is new Int_Result.With_Context (Append => Append);

      Result    : Int_Result.Result;
      Final_Err : Error;
   begin
      Put_Line ("Testing With_Context...");
      Result := Add_Context (R_Err, "reading config");
      Assert (Int_Result.Is_Err (Result), "With_Context preserves Err state");

      Final_Err := Int_Result.Error (Result);
      Assert
        (Final_Err.Message (1 .. Final_Err.Len)
         = "file not found :: reading config",
         "With_Context appends context to error message");

      Result := Add_Context (Int_Result.Ok (42), "context");
      Assert (Int_Result.Is_Ok (Result), "With_Context leaves Ok unchanged");
   end Test_With_Context;

   --  ==========================================================================
   --  Test: Tap (side effects without changing Result)
   --  ==========================================================================

   procedure Test_Tap is
      R_Ok  : constant Int_Result.Result := Int_Result.Ok (42);
      R_Err : constant Int_Result.Result :=
        Int_Result.Err ((Parse_Error, [others => ' '], 0));

      Ok_Called  : Boolean := False;
      Err_Called : Boolean := False;

      procedure On_Ok (V : Integer) is
         pragma Unreferenced (V);
      begin
         Ok_Called := True;
      end On_Ok;

      procedure On_Err (E : Error) is
         pragma Unreferenced (E);
      begin
         Err_Called := True;
      end On_Err;

      function Tap_Both is new
        Int_Result.Tap (On_Ok => On_Ok, On_Err => On_Err);

      Result : Int_Result.Result;
   begin
      Put_Line ("Testing Tap...");
      Ok_Called := False;
      Err_Called := False;
      Result := Tap_Both (R_Ok);
      Assert
        (Int_Result.Is_Ok (Result) and then Ok_Called and then not Err_Called,
         "Tap calls On_Ok for Ok result");

      Ok_Called := False;
      Err_Called := False;
      Result := Tap_Both (R_Err);
      Assert
        (Int_Result.Is_Err (Result) and then Err_Called and then not Ok_Called,
         "Tap calls On_Err for Err result");
   end Test_Tap;

begin
   Put_Line ("======================================");
   Put_Line ("  Functional.Result Unit Tests");
   Put_Line ("  Target: 90%+ Coverage");
   Put_Line ("======================================");
   New_Line;

   Test_Constructors;
   Test_From_Error;
   Test_Extractors;
   Test_Expect;
   Test_Unwrap;
   Test_Map;
   Test_And_Then;
   Test_And_Then_Into;
   Test_Map_Err;
   Test_Bimap;
   Test_Fallback;
   Test_Recover;
   Test_Ensure;
   Test_With_Context;
   Test_Tap;

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
end Test_Result;
