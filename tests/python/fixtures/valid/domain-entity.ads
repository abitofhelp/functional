-- ===========================================================================
-- Domain.Entity - Valid domain entity using aspects
-- ===========================================================================
-- Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
-- SPDX-License-Identifier: BSD-3-Clause
-- ===========================================================================

package Domain.Entity
   with Pure  -- ✅ Using aspect instead of pragma
is
   type Entity_ID is range 1 .. 1_000_000;

   type Entity is record
      ID : Entity_ID;
   end record;

end Domain.Entity;
