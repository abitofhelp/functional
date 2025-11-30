-- ===========================================================================
-- TEST FIXTURE: error_with_unbounded_string.ads
-- ===========================================================================
-- Purpose: Intentionally invalid fixture to test arch_guard detection
--          of Unbounded_String in error types (should use Bounded_String).
-- Expected Violation: UNBOUNDED_STRING_IN_ERROR
-- ===========================================================================

package Domain.Error is
   type Error_Code is (Success, Failure);

   type Domain_Error is record
      Code    : Error_Code;
      Message : String;  -- ❌ Should use Bounded_String
   end record;
end Domain.Error;
