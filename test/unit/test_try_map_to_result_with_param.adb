pragma Ada_2022;
--  ======================================================================
--  Test_Try_Map_To_Result_With_Param
--  ======================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  Purpose:
--    Unit tests for Functional.Try.Map_To_Result_With_Param.
--    Tests parameterized declarative exception-to-Result mapping.
--  ======================================================================

with Ada.Text_IO;    use Ada.Text_IO;
with Ada.Command_Line;
with Ada.IO_Exceptions;
with Functional.Try.Map_To_Result_With_Param;
with Test_Framework;

procedure Test_Try_Map_To_Result_With_Param is

   --  Domain error enumeration (simulates real domain errors)
   type Error_Kind is
     (Validation_Error,
      Parse_Error,
      Not_Found_Error,
      IO_Error,
      Internal_Error);

   --  Simple Result type for testing
   type Test_Result_State is (Ok_State, Error_State);
   type Test_Result (State : Test_Result_State := Error_State) is record
      case State is
         when Ok_State =>
            Value : Integer;
         when Error_State =>
            Kind    : Error_Kind;
            Message : String (1 .. 100);
            Msg_Len : Natural;
      end case;
   end record;

   function Is_Ok (R : Test_Result) return Boolean is (R.State = Ok_State);
   function Is_Error (R : Test_Result) return Boolean is (R.State = Error_State);
   function Get_Value (R : Test_Result) return Integer is (R.Value);
   function Get_Kind (R : Test_Result) return Error_Kind is (R.Kind);
   function Get_Message (R : Test_Result) return String is
     (R.Message (1 .. R.Msg_Len));

   function Ok (V : Integer) return Test_Result is
     ((State => Ok_State, Value => V));

   function Make_Error (Kind : Error_Kind; Message : String) return Test_Result
   is
      Len : constant Natural := Natural'Min (Message'Length, 100);
      Msg : String (1 .. 100) := [others => ' '];
   begin
      Msg (1 .. Len) := Message (Message'First .. Message'First + Len - 1);
      return (State => Error_State, Kind => Kind, Message => Msg, Msg_Len => Len);
   end Make_Error;

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
   --  Test: Run with parameter - success path
   --  ==========================================================================

   procedure Test_Run_With_Param_Success is
      function Double_Value (N : Integer) return Test_Result is
      begin
         return Ok (N * 2);
      end Double_Value;

      package Try_Double is new Functional.Try.Map_To_Result_With_Param
        (Error_Kind_Type    => Error_Kind,
         Param_Type         => Integer,
         Result_Type        => Test_Result,
         Make_Error         => Make_Error,
         Default_Error_Kind => Internal_Error,
         Action             => Double_Value);

      Mappings : constant Try_Double.Mapping_Array (1 .. 0) := [others => <>];
      Result   : constant Test_Result := Try_Double.Run (21, Mappings);
   begin
      Put_Line ("Testing Run with parameter (success)...");
      Assert (Is_Ok (Result), "Run returns Ok for successful action");
      Assert (Get_Value (Result) = 42, "Run returns correct doubled value");
   end Test_Run_With_Param_Success;

   --  ==========================================================================
   --  Test: Run with String parameter - success path
   --  ==========================================================================

   procedure Test_Run_With_String_Param_Success is
      function Get_Length (S : String) return Test_Result is
      begin
         return Ok (S'Length);
      end Get_Length;

      package Try_Length is new Functional.Try.Map_To_Result_With_Param
        (Error_Kind_Type    => Error_Kind,
         Param_Type         => String,
         Result_Type        => Test_Result,
         Make_Error         => Make_Error,
         Default_Error_Kind => Internal_Error,
         Action             => Get_Length);

      Mappings : constant Try_Length.Mapping_Array (1 .. 0) := [others => <>];
      Result   : constant Test_Result := Try_Length.Run ("Hello", Mappings);
   begin
      Put_Line ("Testing Run with String parameter (success)...");
      Assert (Is_Ok (Result), "Run returns Ok for String action");
      Assert (Get_Value (Result) = 5, "Run returns correct string length");
   end Test_Run_With_String_Param_Success;

   --  ==========================================================================
   --  Test: Run with parameter - mapped exception
   --  ==========================================================================

   procedure Test_Run_With_Param_Mapped_Exception is
      function Validate_Positive (N : Integer) return Test_Result is
      begin
         if N < 0 then
            raise Ada.IO_Exceptions.Name_Error with "negative value";
         end if;
         return Ok (N);
      end Validate_Positive;

      package Try_Validate is new Functional.Try.Map_To_Result_With_Param
        (Error_Kind_Type    => Error_Kind,
         Param_Type         => Integer,
         Result_Type        => Test_Result,
         Make_Error         => Make_Error,
         Default_Error_Kind => Internal_Error,
         Action             => Validate_Positive);

      Mappings : constant Try_Validate.Mapping_Array :=
        [(Ada.IO_Exceptions.Name_Error'Identity, Validation_Error),
         (Ada.IO_Exceptions.Use_Error'Identity, IO_Error)];

      Result : constant Test_Result := Try_Validate.Run (-5, Mappings);
   begin
      Put_Line ("Testing Run with parameter (mapped exception)...");
      Assert (Is_Error (Result), "Run returns Error when action raises");
      Assert
        (Get_Kind (Result) = Validation_Error,
         "Run maps Name_Error to Validation_Error");
      Assert
        (Get_Message (Result) = "negative value",
         "Run preserves exception message");
   end Test_Run_With_Param_Mapped_Exception;

   --  ==========================================================================
   --  Test: Run with parameter - unmapped exception (fallback to default)
   --  ==========================================================================

   procedure Test_Run_With_Param_Unmapped_Exception is
      function Check_Range (N : Integer) return Test_Result is
      begin
         if N > 100 then
            raise Constraint_Error with "out of range";
         end if;
         return Ok (N);
      end Check_Range;

      package Try_Check is new Functional.Try.Map_To_Result_With_Param
        (Error_Kind_Type    => Error_Kind,
         Param_Type         => Integer,
         Result_Type        => Test_Result,
         Make_Error         => Make_Error,
         Default_Error_Kind => Internal_Error,
         Action             => Check_Range);

      --  Only map I/O exceptions, not Constraint_Error
      Mappings : constant Try_Check.Mapping_Array :=
        [(Ada.IO_Exceptions.Name_Error'Identity, Not_Found_Error)];

      Result : constant Test_Result := Try_Check.Run (150, Mappings);
   begin
      Put_Line ("Testing Run with parameter (unmapped exception)...");
      Assert (Is_Error (Result), "Run returns Error for unmapped exception");
      Assert
        (Get_Kind (Result) = Internal_Error,
         "Run uses Default_Error_Kind for unmapped exception");
   end Test_Run_With_Param_Unmapped_Exception;

   --  ==========================================================================
   --  Test: Run_Catch_All with parameter - success path
   --  ==========================================================================

   procedure Test_Run_Catch_All_With_Param_Success is
      function Square (N : Integer) return Test_Result is
      begin
         return Ok (N * N);
      end Square;

      package Try_Square is new Functional.Try.Map_To_Result_With_Param
        (Error_Kind_Type    => Error_Kind,
         Param_Type         => Integer,
         Result_Type        => Test_Result,
         Make_Error         => Make_Error,
         Default_Error_Kind => Internal_Error,
         Action             => Square);

      Result : constant Test_Result := Try_Square.Run_Catch_All (7);
   begin
      Put_Line ("Testing Run_Catch_All with parameter (success)...");
      Assert (Is_Ok (Result), "Run_Catch_All returns Ok");
      Assert (Get_Value (Result) = 49, "Run_Catch_All returns correct squared value");
   end Test_Run_Catch_All_With_Param_Success;

   --  ==========================================================================
   --  Test: Run_Catch_All with parameter - exception path
   --  ==========================================================================

   procedure Test_Run_Catch_All_With_Param_Exception is
      function Divide_By (N : Integer) return Test_Result is
      begin
         if N = 0 then
            raise Program_Error with "division by zero";
         end if;
         return Ok (100 / N);
      end Divide_By;

      package Try_Divide is new Functional.Try.Map_To_Result_With_Param
        (Error_Kind_Type    => Error_Kind,
         Param_Type         => Integer,
         Result_Type        => Test_Result,
         Make_Error         => Make_Error,
         Default_Error_Kind => Parse_Error,
         Action             => Divide_By);

      Result : constant Test_Result := Try_Divide.Run_Catch_All (0);
   begin
      Put_Line ("Testing Run_Catch_All with parameter (exception)...");
      Assert (Is_Error (Result), "Run_Catch_All returns Error when raises");
      Assert
        (Get_Kind (Result) = Parse_Error,
         "Run_Catch_All uses Default_Error_Kind");
      Assert
        (Get_Message (Result) = "division by zero",
         "Run_Catch_All preserves exception message");
   end Test_Run_Catch_All_With_Param_Exception;

   --  ==========================================================================
   --  Test: Parameter value affects result
   --  ==========================================================================

   procedure Test_Parameter_Affects_Result is
      function Add_Ten (N : Integer) return Test_Result is
      begin
         return Ok (N + 10);
      end Add_Ten;

      package Try_Add is new Functional.Try.Map_To_Result_With_Param
        (Error_Kind_Type    => Error_Kind,
         Param_Type         => Integer,
         Result_Type        => Test_Result,
         Make_Error         => Make_Error,
         Default_Error_Kind => Internal_Error,
         Action             => Add_Ten);

      Result_5  : constant Test_Result := Try_Add.Run_Catch_All (5);
      Result_20 : constant Test_Result := Try_Add.Run_Catch_All (20);
   begin
      Put_Line ("Testing parameter affects result...");
      Assert (Is_Ok (Result_5), "First call succeeds");
      Assert (Get_Value (Result_5) = 15, "First result is 5 + 10 = 15");
      Assert (Is_Ok (Result_20), "Second call succeeds");
      Assert (Get_Value (Result_20) = 30, "Second result is 20 + 10 = 30");
   end Test_Parameter_Affects_Result;

begin
   Put_Line ("==========================================");
   Put_Line ("  Functional.Try.Map_To_Result_With_Param Tests");
   Put_Line ("==========================================");
   New_Line;

   Test_Run_With_Param_Success;
   Test_Run_With_String_Param_Success;
   Test_Run_With_Param_Mapped_Exception;
   Test_Run_With_Param_Unmapped_Exception;
   Test_Run_Catch_All_With_Param_Success;
   Test_Run_Catch_All_With_Param_Exception;
   Test_Parameter_Affects_Result;

   New_Line;
   Put_Line ("==========================================");
   Put_Line
     ("  Results: " & Pass_Count'Image & " /" & Test_Count'Image & " passed");
   if Pass_Count = Test_Count then
      Put_Line ("  Status: ALL TESTS PASSED");
   else
      Put_Line ("  Status: FAILURES DETECTED");
   end if;
   Put_Line ("==========================================");

   --  Register results with test framework
   Test_Framework.Register_Results (Test_Count, Pass_Count);

   if Pass_Count /= Test_Count then
      Ada.Command_Line.Set_Exit_Status (1);
   end if;
end Test_Try_Map_To_Result_With_Param;
