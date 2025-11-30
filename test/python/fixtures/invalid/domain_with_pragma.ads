-- ===========================================================================
-- TEST FIXTURE: domain_with_pragma.ads
-- ===========================================================================
-- Purpose: Intentionally invalid fixture to test arch_guard detection
--          of pragma Pure/Preelaborate (should use aspect syntax).
-- Expected Violation: PRAGMA_VS_ASPECT
-- ===========================================================================

package Domain.Types is
   pragma Pure;  -- ❌ Should use "with Pure" aspect

   type Count is range 0 .. 1_000;
end Domain.Types;
