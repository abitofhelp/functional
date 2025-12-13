pragma Ada_2022;
--  ===========================================================================
--  Functional.Scoped - RAII Resource Management for Boundary Operations
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Provides RAII (Resource Acquisition Is Initialization) pattern for
--    automatic resource cleanup at boundaries. Complements Functional.Try
--    by ensuring resources are released even when exceptions occur.
--
--  Use Cases:
--    - File handles that must be closed
--    - Database connections that must be released
--    - Locks that must be unlocked
--    - Any resource requiring deterministic cleanup
--
--  Design Pattern:
--    Declare a Guard object in a declare block. When the block exits
--    (normally or via exception), Finalize is automatically called,
--    which invokes the Release procedure on the resource.
--
--  Example:
--    declare
--       File : aliased Ada.Streams.Stream_IO.File_Type;
--       package File_Scoped is new Functional.Scoped
--         (Resource => Ada.Streams.Stream_IO.File_Type,
--          Release  => Ada.Streams.Stream_IO.Close);
--       Guard : File_Scoped.Guard (File'Access);
--    begin
--       Ada.Streams.Stream_IO.Open (File, In_File, "data.txt");
--       --  ... use file ...
--    end;  --  File automatically closed here, even on exception
--
--  Dependencies:
--    Ada.Finalization
--
--  ===========================================================================

with Ada.Finalization;

package Functional.Scoped with
  SPARK_Mode => Off  --  Uses Ada.Finalization (not SPARK compatible)
is

   --  ========================================================================
   --  Generic Scoped Resource Guard
   --  ========================================================================
   --  Instantiate with your resource type and release procedure.
   --  The Guard will automatically call Release when it goes out of scope.

   generic
      type Resource is limited private;
      with procedure Release (R : in out Resource);
   package Guard_For is

      --  Guard type - declare an instance to protect a resource
      type Guard (R : access Resource) is
        new Ada.Finalization.Limited_Controlled with null record;

      --  Releases the resource when Guard goes out of scope
      overriding procedure Finalize (G : in out Guard);

   end Guard_For;

   --  ========================================================================
   --  Conditional Guard - Only releases if condition is met
   --  ========================================================================
   --  Use when cleanup should only happen under certain conditions
   --  (e.g., only close file if it was successfully opened).

   generic
      type Resource is limited private;
      with function Should_Release (R : Resource) return Boolean;
      with procedure Release (R : in out Resource);
   package Conditional_Guard_For is

      type Guard (R : access Resource) is
        new Ada.Finalization.Limited_Controlled with null record;

      overriding procedure Finalize (G : in out Guard);

   end Conditional_Guard_For;

end Functional.Scoped;
