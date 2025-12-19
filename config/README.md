# Functional Library - Embedded Restrictions

**Version:** 4.1.0<br>
**Date:** 2025-12-18<br>
**SPDX-License-Identifier:** BSD-3-Clause<br>
**License File:** See the LICENSE file in the project root<br>
**Copyright:** © 2025 Michael Gardner, A Bit of Help, Inc.<br>
**Status:** Released

## Overview

The Functional library is **100% embedded-safe** with ZERO heap allocations. All types (`Result<T,E>`, `Option<T>`, `Either<L,R>`) use stack-based discriminated records.

## Why No Restrictions in Source?

The library source code does NOT contain `pragma Restrictions` to allow testing with AUnit (which uses recursion). Instead, **your application** should apply the restrictions.

## Usage

### Option 1: Copy as gnat.adc
```bash
cp functional/config/embedded_restrictions.adc ./gnat.adc
alr build  # Restrictions applied automatically
```

### Option 2: Reference in GPR
```ada
project My_App is
   package Compiler is
      for Local_Configuration_Pragmas use
        "../functional/config/embedded_restrictions.adc";
   end Compiler;
end My_App;
```

### Option 3: Compiler Switch
```bash
alr exec -- gprbuild -gnatec=functional/config/embedded_restrictions.adc
```

## Restrictions Applied

The `embedded_restrictions.adc` file applies:

**Heap Safety:**
- `No_Allocators` - No explicit "new"
- `No_Implicit_Heap_Allocations` - No hidden heap use
- `No_Anonymous_Allocators` - No anonymous access with new
- `No_Coextensions` - No discriminant allocations
- `No_Local_Allocators` - No local pool allocations

**Additional Safety:**
- `No_Finalization` - No controlled types
- `No_Nested_Finalization` - No nested controlled types
- `No_Recursion` - Stack safety (no unbounded growth)

## Verification

The library is designed to comply with these restrictions. To verify:

```bash
alr exec -- gprbuild -P functional.gpr \
  -gnatec=config/embedded_restrictions.adc
```

If this builds successfully, the library complies.

## Why This Approach?

This is standard Ada library practice:
- **Library**: Compatible with restrictions (source unrestricted)
- **Tests**: Can use AUnit (needs recursion)
- **Applications**: Choose their own restriction policy
- **Verification**: Release process checks compliance

See `docs/EMBEDDED_RESTRICTIONS_SOLUTION.md` (in zoneinfo project) for detailed explanation.
