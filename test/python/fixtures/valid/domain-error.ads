-- ===========================================================================
-- Domain.Error - Valid error type using Bounded_String
-- ===========================================================================
-- Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
-- SPDX-License-Identifier: BSD-3-Clause
-- ===========================================================================

with Ada.Strings.Bounded;

package Domain.Error is

   package Error_Strings is new Ada.Strings.Bounded.Generic_Bounded_Length (256);
   use Error_Strings;

   type Error_Code is (Success, Invalid_Input, Not_Found, Internal_Error);

   type Domain_Error is record
      Code    : Error_Code;
      Message : Bounded_String;  -- ✅ Using Bounded_String
   end record;

   function From_Error (Code : Error_Code; Message : String) return Domain_Error;

end Domain.Error;
