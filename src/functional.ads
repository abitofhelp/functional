pragma Ada_2022;
--  ============================================================================
--  Functional
--  ============================================================================
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Root package for functional programming types: Result<T,E>, Option<T>,
--    Either<L,R> for type-safe error handling in Ada 2022.
--
--  Usage:
--    with Functional.Result;
--    package My_Result is new Functional.Result (T => String, E => Error);
--
--  See Also:
--    Functional.Result        - Result<T,E> monad for error handling
--    Functional.Option        - Option<T> for optional values
--    Functional.Either        - Either<L,R> for disjoint unions
--    Functional.Try.To_Result - Exception boundary helpers
--  ============================================================================

package Functional
  with Pure
is
   --  Root package for functional programming types
end Functional;
