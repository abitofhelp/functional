-- ===========================================================================
-- TEST FIXTURE: app_imports_test.ads
-- ===========================================================================
-- Purpose: Intentionally invalid fixture to test arch_guard detection
--          of test framework imports in production code.
-- Expected Violation: TEST_CODE_IN_PRODUCTION
-- ===========================================================================

with Test_Framework;  -- ❌ Production code must not import test frameworks

package Application.Service is
   procedure Execute;
end Application.Service;
