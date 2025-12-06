pragma Ada_2022;
--  ===========================================================================
--  Functional.Either - Disjoint Union Type
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Generic Either type representing one of two possible values (Left or
--    Right). Unlike Result, neither side is designated as "error" - both are
--    equally valid outcomes. Useful for parsing, validation, and branching.
--
--  Key Types:
--    Either       - Discriminated record with Is_Left : Boolean
--                   When True, holds Left_Value; when False, holds Right_Value
--
--  Operations (8):
--    Constructors:   Left, Right
--    Predicates:     Is_Left, Is_Right
--    Extractors:     Left_Value, Right_Value
--    Transforms:     Map_Left, Map_Right, Bimap
--    Reduction:      Fold
--
--  ===========================================================================

generic
   type L is private;
   type R is private;
package Functional.Either is

   type Either (Is_Left : Boolean := True) is record
      case Is_Left is
         when True =>
            Left_Value : L;

         when False =>
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
   with Pre => E.Is_Left, Inline;

   function Right_Value (E : Either) return R
   with Pre => not E.Is_Left, Inline;

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
