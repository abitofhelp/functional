-- Invalid: Using pragma instead of aspect
package Domain.Types is
   pragma Pure;  -- ❌ Should use "with Pure" aspect

   type Count is range 0 .. 1_000;
end Domain.Types;
