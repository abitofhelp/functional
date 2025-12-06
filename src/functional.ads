pragma Ada_2022;
--  ===========================================================================
--  Functional - Type-Safe Functional Programming Library
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Root namespace for the Functional library. Provides type-safe functional
--    programming abstractions for Ada 2022 including Result, Option, Either,
--    and Try exception bridges.
--
--  Child Packages:
--    Functional.Result  - Error handling (Ok/Err)
--    Functional.Option  - Optional values (Some/None)
--    Functional.Either  - Disjoint unions (Left/Right)
--    Functional.Try     - Exception-to-Result/Option bridges
--    Functional.Version - Library version information
--
--  ===========================================================================

package Functional with Pure is
   --  Pure namespace anchor - all functionality in child packages
end Functional;
