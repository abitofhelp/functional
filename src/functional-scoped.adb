pragma Ada_2022;
--  ===========================================================================
--  Functional.Scoped - Implementation
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Implements RAII guards for automatic resource cleanup.
--
--  Implementation Notes:
--    - Finalize is called automatically when Guard leaves scope
--    - Handles both normal exit and exception propagation
--    - Conditional_Guard_For checks Should_Release before cleanup
--
--  ===========================================================================

package body Functional.Scoped is

   --  ========================================================================
   --  Guard_For - Unconditional resource cleanup
   --  ========================================================================

   package body Guard_For is

      overriding procedure Finalize (G : in out Guard) is
      begin
         Release (G.R.all);
      end Finalize;

   end Guard_For;

   --  ========================================================================
   --  Conditional_Guard_For - Conditional resource cleanup
   --  ========================================================================

   package body Conditional_Guard_For is

      overriding procedure Finalize (G : in out Guard) is
      begin
         if Should_Release (G.R.all) then
            Release (G.R.all);
         end if;
      end Finalize;

   end Conditional_Guard_For;

end Functional.Scoped;
