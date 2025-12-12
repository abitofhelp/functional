pragma Ada_2022;
--  ======================================================================
--  Test_Try_Map_To_Result
--  ======================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  Purpose:
--    Comprehensive unit tests for Functional.Try.Map_To_Result.
--    Tests declarative exception-to-Result mapping with exception tables.
--    Target: 90%+ code coverage.
--  ======================================================================

with Ada.Text_IO;    use Ada.Text_IO;
with Ada.Command_Line;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.IO_Exceptions;
with Functional.Try.Map_To_Result;
with Test_Framework;

procedure Test_Try_Map_To_Result is

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
   --  Test: Run with mappings - success path
   --  ==========================================================================

   procedure Test_Run_Success is
      function Get_Value return Test_Result is
      begin
         return Ok (42);
      end Get_Value;

      package Try_Get is new Functional.Try.Map_To_Result
        (Error_Kind_Type    => Error_Kind,
         Result_Type        => Test_Result,
         Make_Error         => Make_Error,
         Default_Error_Kind => Internal_Error,
         Action             => Get_Value);

      Mappings : constant Try_Get.Mapping_Array (1 .. 0) := [others => <>];
      Result   : constant Test_Result := Try_Get.Run (Mappings);
   begin
      Put_Line ("Testing Run with mappings (success)...");
      Assert (Is_Ok (Result), "Run returns Ok for successful action");
      Assert (Get_Value (Result) = 42, "Run returns correct value");
   end Test_Run_Success;

   --  ==========================================================================
   --  Test: Run with mappings - mapped exception (anticipated)
   --  ==========================================================================

   procedure Test_Run_Mapped_Exception is
      function Raise_Name_Error return Test_Result is
      begin
         raise Ada.IO_Exceptions.Name_Error with "file not found";
         return Ok (0);  --  Never reached
      end Raise_Name_Error;

      package Try_Open is new Functional.Try.Map_To_Result
        (Error_Kind_Type    => Error_Kind,
         Result_Type        => Test_Result,
         Make_Error         => Make_Error,
         Default_Error_Kind => Internal_Error,
         Action             => Raise_Name_Error);

      --  Map Name_Error to Not_Found_Error
      Mappings : constant Try_Open.Mapping_Array :=
        [(Ada.IO_Exceptions.Name_Error'Identity, Not_Found_Error),
         (Ada.IO_Exceptions.Use_Error'Identity, IO_Error)];

      Result : constant Test_Result := Try_Open.Run (Mappings);
   begin
      Put_Line ("Testing Run with mapped exception...");
      Assert (Is_Error (Result), "Run returns Error when action raises");
      Assert
        (Get_Kind (Result) = Not_Found_Error,
         "Run maps Name_Error to Not_Found_Error");
      Assert
        (Get_Message (Result) = "file not found",
         "Run preserves exception message");
   end Test_Run_Mapped_Exception;

   --  ==========================================================================
   --  Test: Run with mappings - unmapped exception (fallback to default)
   --  ==========================================================================

   procedure Test_Run_Unmapped_Exception is
      function Raise_Constraint_Error return Test_Result is
      begin
         raise Constraint_Error with "unexpected error";
         return Ok (0);  --  Never reached
      end Raise_Constraint_Error;

      package Try_Action is new Functional.Try.Map_To_Result
        (Error_Kind_Type    => Error_Kind,
         Result_Type        => Test_Result,
         Make_Error         => Make_Error,
         Default_Error_Kind => Internal_Error,
         Action             => Raise_Constraint_Error);

      --  Only map I/O exceptions, not Constraint_Error
      Mappings : constant Try_Action.Mapping_Array :=
        [(Ada.IO_Exceptions.Name_Error'Identity, Not_Found_Error),
         (Ada.IO_Exceptions.Use_Error'Identity, IO_Error)];

      Result : constant Test_Result := Try_Action.Run (Mappings);
   begin
      Put_Line ("Testing Run with unmapped exception (fallback)...");
      Assert (Is_Error (Result), "Run returns Error for unmapped exception");
      Assert
        (Get_Kind (Result) = Internal_Error,
         "Run uses Default_Error_Kind for unmapped exception");
      Assert
        (Get_Message (Result) = "unexpected error",
         "Run preserves exception message for unmapped");
   end Test_Run_Unmapped_Exception;

   --  ==========================================================================
   --  Test: Run_Catch_All - success path
   --  ==========================================================================

   procedure Test_Run_Catch_All_Success is
      function Get_Value return Test_Result is
      begin
         return Ok (99);
      end Get_Value;

      package Try_Get is new Functional.Try.Map_To_Result
        (Error_Kind_Type    => Error_Kind,
         Result_Type        => Test_Result,
         Make_Error         => Make_Error,
         Default_Error_Kind => Internal_Error,
         Action             => Get_Value);

      Result : constant Test_Result := Try_Get.Run_Catch_All;
   begin
      Put_Line ("Testing Run_Catch_All (success)...");
      Assert (Is_Ok (Result), "Run_Catch_All returns Ok for successful action");
      Assert (Get_Value (Result) = 99, "Run_Catch_All returns correct value");
   end Test_Run_Catch_All_Success;

   --  ==========================================================================
   --  Test: Run_Catch_All - exception path
   --  ==========================================================================

   procedure Test_Run_Catch_All_Exception is
      function Raise_Error return Test_Result is
      begin
         raise Program_Error with "catch all test";
         return Ok (0);  --  Never reached
      end Raise_Error;

      package Try_Action is new Functional.Try.Map_To_Result
        (Error_Kind_Type    => Error_Kind,
         Result_Type        => Test_Result,
         Make_Error         => Make_Error,
         Default_Error_Kind => IO_Error,
         Action             => Raise_Error);

      Result : constant Test_Result := Try_Action.Run_Catch_All;
   begin
      Put_Line ("Testing Run_Catch_All (exception)...");
      Assert (Is_Error (Result), "Run_Catch_All returns Error when raises");
      Assert
        (Get_Kind (Result) = IO_Error,
         "Run_Catch_All uses Default_Error_Kind");
      Assert
        (Get_Message (Result) = "catch all test",
         "Run_Catch_All preserves exception message");
   end Test_Run_Catch_All_Exception;

   --  ==========================================================================
   --  Test: Multiple mappings - second mapping matches
   --  ==========================================================================

   procedure Test_Run_Second_Mapping_Matches is
      function Raise_Use_Error return Test_Result is
      begin
         raise Ada.IO_Exceptions.Use_Error with "permission denied";
         return Ok (0);  --  Never reached
      end Raise_Use_Error;

      package Try_Open is new Functional.Try.Map_To_Result
        (Error_Kind_Type    => Error_Kind,
         Result_Type        => Test_Result,
         Make_Error         => Make_Error,
         Default_Error_Kind => Internal_Error,
         Action             => Raise_Use_Error);

      Mappings : constant Try_Open.Mapping_Array :=
        [(Ada.IO_Exceptions.Name_Error'Identity, Not_Found_Error),
         (Ada.IO_Exceptions.Use_Error'Identity, IO_Error),
         (Ada.IO_Exceptions.Data_Error'Identity, Parse_Error)];

      Result : constant Test_Result := Try_Open.Run (Mappings);
   begin
      Put_Line ("Testing Run with second mapping match...");
      Assert (Is_Error (Result), "Run returns Error");
      Assert
        (Get_Kind (Result) = IO_Error,
         "Run correctly matches second mapping (Use_Error -> IO_Error)");
   end Test_Run_Second_Mapping_Matches;

   --  ==========================================================================
   --  Test: Empty mappings - all exceptions use default
   --  ==========================================================================

   procedure Test_Run_Empty_Mappings is
      function Raise_Error return Test_Result is
      begin
         raise Storage_Error with "out of memory";
         return Ok (0);  --  Never reached
      end Raise_Error;

      package Try_Action is new Functional.Try.Map_To_Result
        (Error_Kind_Type    => Error_Kind,
         Result_Type        => Test_Result,
         Make_Error         => Make_Error,
         Default_Error_Kind => Internal_Error,
         Action             => Raise_Error);

      Result : constant Test_Result :=
        Try_Action.Run (Try_Action.Empty_Mappings);
   begin
      Put_Line ("Testing Run with Empty_Mappings...");
      Assert (Is_Error (Result), "Run returns Error");
      Assert
        (Get_Kind (Result) = Internal_Error,
         "Run uses Default_Error_Kind with empty mappings");
   end Test_Run_Empty_Mappings;

begin
   Put_Line ("==========================================");
   Put_Line ("  Functional.Try.Map_To_Result Tests");
   Put_Line ("  Target: 90%+ Coverage");
   Put_Line ("==========================================");
   New_Line;

   Test_Run_Success;
   Test_Run_Mapped_Exception;
   Test_Run_Unmapped_Exception;
   Test_Run_Catch_All_Success;
   Test_Run_Catch_All_Exception;
   Test_Run_Second_Mapping_Matches;
   Test_Run_Empty_Mappings;

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
end Test_Try_Map_To_Result;
