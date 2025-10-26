pragma Ada_2022;
--  ============================================================================
--  Functional.Either
--  ============================================================================
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Either<L, R> for neutral disjoint unions in Ada 2022.
--    Represents either a Left value or a Right value.
--
--  Usage:
--    package Str_Int_Either is new Functional.Either (L => String, R => Integer);
--    declare
--       E : Str_Int_Either.Either := Str_Int_Either.Left ("error");
--    begin
--       if Str_Int_Either.Is_Left (E) then
--          Put_Line ("Left: " & Str_Int_Either.Left_Value (E));
--       end if;
--    end;
--
--  Design Notes:
--    Prefer Functional.Result for error handling. Use Either when Left/Right
--    are not semantically Error/Ok, but rather two equally valid alternatives.
--
--  See Also:
--    Functional.Result - error-centric Ok/Err type
--    Functional.Option - optional values
--  ============================================================================

generic
   type L is private;
   type R is private;
package Functional.Either is

   type Either_Kind is (K_Left, K_Right);

   type Either (Kind : Either_Kind := K_Left) is record
      case Kind is
         when K_Left =>
            Left_Value : L;

         when K_Right =>
            Right_Value : R;
      end case;
   end record;

   --  ==========================================================================
   --  Constructors
   --  ==========================================================================

   function Left (V : L) return Either
   with Inline;
   function Right (V : R) return Either
   with Inline;

   --  ==========================================================================
   --  Predicates
   --  ==========================================================================

   function Is_Left (E : Either) return Boolean
   with Inline;
   function Is_Right (E : Either) return Boolean
   with Inline;

   --  ==========================================================================
   --  Extractors
   --  ==========================================================================

   function Left_Value (E : Either) return L
   with Pre => E.Kind = K_Left, Inline;

   function Right_Value (E : Either) return R
   with Pre => E.Kind = K_Right, Inline;

   --  ==========================================================================
   --  Transformations
   --  ==========================================================================

   --  Map_Left: transform Left value
   generic
      with function F (X : L) return L;
   function Map_Left (E : Either) return Either;

   --  Map_Right: transform Right value
   generic
      with function F (X : R) return R;
   function Map_Right (E : Either) return Either;

   --  Bimap: transform both Left and Right values simultaneously
   generic
      with function Map_L (X : L) return L;
      with function Map_R (X : R) return R;
   function Bimap (E : Either) return Either;

   --  Fold: reduce Either to single value by providing handlers for both cases
   generic
      type U is private;
      with function On_Left (X : L) return U;
      with function On_Right (X : R) return U;
   function Fold (E : Either) return U;

end Functional.Either;
