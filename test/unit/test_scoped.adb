pragma Ada_2022;
--  ======================================================================
--  Test_Scoped
--  ======================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  Purpose:
--    Unit tests for Functional.Scoped RAII guards.
--    Tests Guard_For and Conditional_Guard_For for automatic cleanup.
--  ======================================================================

with Ada.Text_IO;    use Ada.Text_IO;
with Ada.Command_Line;
with Functional.Scoped;
with Test_Framework;

procedure Test_Scoped is

   --  Test statistics
   Test_Count   : Natural := 0;
   Passed_Count : Natural := 0;

   procedure Assert (Condition : Boolean; Test_Name : String) is
   begin
      Test_Count := Test_Count + 1;
      if Condition then
         Passed_Count := Passed_Count + 1;
         Put_Line ("[PASS] " & Test_Name);
      else
         Put_Line ("[FAIL] " & Test_Name);
      end if;
   end Assert;

   --  ========================================================================
   --  Test Resource - Simple counter to track releases
   --  ========================================================================

   type Test_Resource is record
      Value        : Integer := 0;
      Release_Count : Natural := 0;
   end record;

   procedure Release_Resource (R : in out Test_Resource) is
   begin
      R.Release_Count := R.Release_Count + 1;
   end Release_Resource;

   function Is_Active (R : Test_Resource) return Boolean is
     (R.Value > 0);

   --  Instantiate guards for our test resource
   package Resource_Guard is new Functional.Scoped.Guard_For
     (Resource => Test_Resource,
      Release  => Release_Resource);

   package Conditional_Resource_Guard is new
     Functional.Scoped.Conditional_Guard_For
       (Resource       => Test_Resource,
        Should_Release => Is_Active,
        Release        => Release_Resource);

   --  ========================================================================
   --  Raising Resource - For testing exception handling in Finalize
   --  ========================================================================

   type Raising_Resource is record
      Release_Attempted : Boolean := False;
   end record;

   Release_Exception : exception;

   procedure Raising_Release (R : in out Raising_Resource) is
   begin
      R.Release_Attempted := True;
      raise Release_Exception;
   end Raising_Release;

   function Always_True (R : Raising_Resource) return Boolean is
      pragma Unreferenced (R);
   begin
      return True;
   end Always_True;

   Condition_Exception : exception;

   function Raising_Condition (R : Raising_Resource) return Boolean is
      pragma Unreferenced (R);
   begin
      raise Condition_Exception;
      return True;  --  Never reached
   end Raising_Condition;

   package Raising_Guard is new Functional.Scoped.Guard_For
     (Resource => Raising_Resource,
      Release  => Raising_Release);

   package Cond_Raising_Release_Guard is new
     Functional.Scoped.Conditional_Guard_For
       (Resource       => Raising_Resource,
        Should_Release => Always_True,
        Release        => Raising_Release);

   package Cond_Raising_Check_Guard is new
     Functional.Scoped.Conditional_Guard_For
       (Resource       => Raising_Resource,
        Should_Release => Raising_Condition,
        Release        => Raising_Release);

begin
   Put_Line ("========================================");
   Put_Line ("Testing: Functional.Scoped");
   Put_Line ("========================================");
   New_Line;

   --  ========================================================================
   --  Test: Guard_For releases on normal scope exit
   --  ========================================================================

   Put_Line ("Test: Guard_For - Normal Scope Exit");
   declare
      Res : aliased Test_Resource := (Value => 42, Release_Count => 0);
   begin
      declare
         Guard : Resource_Guard.Guard (Res'Access);
         pragma Unreferenced (Guard);
      begin
         --  Do something with resource
         Res.Value := Res.Value + 1;
      end;  --  Guard.Finalize called here

      Assert
        (Res.Release_Count = 1,
         "Guard_For calls Release on normal scope exit");
   end;

   --  ========================================================================
   --  Test: Guard_For releases on exception
   --  ========================================================================

   Put_Line ("Test: Guard_For - Exception Path");
   declare
      Res : aliased Test_Resource := (Value => 42, Release_Count => 0);
      Custom_Exception : exception;
   begin
      begin
         declare
            Guard : Resource_Guard.Guard (Res'Access);
            pragma Unreferenced (Guard);
         begin
            --  Raise exception - Guard should still finalize
            raise Custom_Exception;
         end;
      exception
         when Custom_Exception =>
            null;  --  Expected, continue
      end;

      Assert
        (Res.Release_Count = 1,
         "Guard_For calls Release even when exception raised");
   end;

   --  ========================================================================
   --  Test: Multiple guards work independently
   --  ========================================================================

   Put_Line ("Test: Guard_For - Multiple Guards");
   declare
      Res1 : aliased Test_Resource := (Value => 1, Release_Count => 0);
      Res2 : aliased Test_Resource := (Value => 2, Release_Count => 0);
   begin
      declare
         Guard1 : Resource_Guard.Guard (Res1'Access);
         Guard2 : Resource_Guard.Guard (Res2'Access);
         pragma Unreferenced (Guard1, Guard2);
      begin
         null;  --  Just let guards finalize
      end;

      Assert
        (Res1.Release_Count = 1 and then Res2.Release_Count = 1,
         "Multiple guards release independently");
   end;

   --  ========================================================================
   --  Test: Conditional_Guard_For releases when condition true
   --  ========================================================================

   Put_Line ("Test: Conditional_Guard_For - Condition True");
   declare
      --  Value > 0 means Is_Active returns True
      Res : aliased Test_Resource := (Value => 42, Release_Count => 0);
   begin
      declare
         Guard : Conditional_Resource_Guard.Guard (Res'Access);
         pragma Unreferenced (Guard);
      begin
         null;  --  Resource is active (Value > 0)
      end;

      Assert
        (Res.Release_Count = 1,
         "Conditional_Guard_For calls Release when Should_Release true");
   end;

   --  ========================================================================
   --  Test: Conditional_Guard_For does NOT release when condition false
   --  ========================================================================

   Put_Line ("Test: Conditional_Guard_For - Condition False");
   declare
      --  Value = 0 means Is_Active returns False
      Res : aliased Test_Resource := (Value => 0, Release_Count => 0);
   begin
      declare
         Guard : Conditional_Resource_Guard.Guard (Res'Access);
         pragma Unreferenced (Guard);
      begin
         null;  --  Resource is inactive (Value = 0)
      end;

      Assert
        (Res.Release_Count = 0,
         "Conditional_Guard_For skips Release when Should_Release false");
   end;

   --  ========================================================================
   --  Test: Conditional_Guard_For checks condition at finalization time
   --  ========================================================================

   Put_Line ("Test: Conditional_Guard_For - Condition Checked At Finalize");
   declare
      --  Start inactive, become active during scope
      Res : aliased Test_Resource := (Value => 0, Release_Count => 0);
   begin
      declare
         Guard : Conditional_Resource_Guard.Guard (Res'Access);
         pragma Unreferenced (Guard);
      begin
         --  Make resource active during scope
         Res.Value := 100;
      end;

      Assert
        (Res.Release_Count = 1,
         "Conditional_Guard_For checks condition at finalization time");
   end;

   --  ========================================================================
   --  Test: Conditional_Guard_For with exception and condition true
   --  ========================================================================

   Put_Line ("Test: Conditional_Guard_For - Exception With True Condition");
   declare
      Res : aliased Test_Resource := (Value => 42, Release_Count => 0);
      Custom_Exception : exception;
   begin
      begin
         declare
            Guard : Conditional_Resource_Guard.Guard (Res'Access);
            pragma Unreferenced (Guard);
         begin
            raise Custom_Exception;
         end;
      exception
         when Custom_Exception =>
            null;
      end;

      Assert
        (Res.Release_Count = 1,
         "Conditional_Guard_For releases on exception when condition true");
   end;

   --  ========================================================================
   --  Test: Conditional_Guard_For with exception and condition false
   --  ========================================================================

   Put_Line ("Test: Conditional_Guard_For - Exception With False Condition");
   declare
      Res : aliased Test_Resource := (Value => 0, Release_Count => 0);
      Custom_Exception : exception;
   begin
      begin
         declare
            Guard : Conditional_Resource_Guard.Guard (Res'Access);
            pragma Unreferenced (Guard);
         begin
            raise Custom_Exception;
         end;
      exception
         when Custom_Exception =>
            null;
      end;

      Assert
        (Res.Release_Count = 0,
         "Conditional_Guard_For skips release on exception when false");
   end;

   --  ========================================================================
   --  Test: Guard_For handles raising Release without propagating
   --  ========================================================================

   Put_Line ("Test: Guard_For - Raising Release Is Caught");
   declare
      Res : aliased Raising_Resource := (Release_Attempted => False);
      Scope_Exited_Normally : Boolean := False;
   begin
      declare
         Guard : Raising_Guard.Guard (Res'Access);
         pragma Unreferenced (Guard);
      begin
         null;
      end;  --  Finalize called, Release raises, but is caught

      Scope_Exited_Normally := True;
      --  Primary check: scope exited normally (exception didn't propagate)
      Assert
        (Scope_Exited_Normally,
         "Guard_For catches exception from Release");
   end;

   --  ========================================================================
   --  Test: Conditional_Guard_For handles raising Release without propagating
   --  ========================================================================

   Put_Line ("Test: Conditional_Guard_For - Raising Release Is Caught");
   declare
      Res : aliased Raising_Resource := (Release_Attempted => False);
      Scope_Exited_Normally : Boolean := False;
   begin
      declare
         Guard : Cond_Raising_Release_Guard.Guard (Res'Access);
         pragma Unreferenced (Guard);
      begin
         null;
      end;  --  Finalize called, Release raises, but is caught

      Scope_Exited_Normally := True;
      --  Primary check: scope exited normally (exception didn't propagate)
      Assert
        (Scope_Exited_Normally,
         "Conditional_Guard_For catches exception from Release");
   end;

   --  ========================================================================
   --  Test: Conditional_Guard_For handles raising Should_Release
   --  ========================================================================

   Put_Line ("Test: Conditional_Guard_For - Raising Should_Release Is Caught");
   declare
      Res : aliased Raising_Resource := (Release_Attempted => False);
      Scope_Exited_Normally : Boolean := False;
   begin
      declare
         Guard : Cond_Raising_Check_Guard.Guard (Res'Access);
         pragma Unreferenced (Guard);
      begin
         null;
      end;  --  Should_Release raises, caught, defaults to no release

      Scope_Exited_Normally := True;
      --  Release should NOT have been attempted because Should_Release raised
      Assert
        (Scope_Exited_Normally and then not Res.Release_Attempted,
         "Conditional_Guard_For defaults to no release when check raises");
   end;

   --  Print summary
   New_Line;
   Put_Line ("========================================");
   Put_Line ("Test Summary: Functional.Scoped");
   Put_Line ("========================================");
   Put_Line ("Total tests:" & Test_Count'Image);
   Put_Line ("Passed:     " & Passed_Count'Image);
   Put_Line ("Failed:     " & Natural'Image (Test_Count - Passed_Count));
   New_Line;

   --  Register results with test framework
   Test_Framework.Register_Results (Test_Count, Passed_Count);

   --  Set exit status
   Ada.Command_Line.Set_Exit_Status
     (if Test_Count = Passed_Count
      then Ada.Command_Line.Success
      else Ada.Command_Line.Failure);

end Test_Scoped;
