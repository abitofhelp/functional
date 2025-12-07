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
--  Operations (20):
--    Constructors:   Left, Right
--    Predicates:     Is_Left, Is_Right, Is_Left_And, Is_Left_Or, Is_Right_And, Is_Right_Or, Contains
--    Extractors:     Left_Value, Right_Value, Get_Or_Else
--    Transforms:     Map, Map_Left, Map_Right, Bimap, Swap, And_Then
--    Reduction:      Fold, Merge
--    Conversion:     To_Option, To_Result
--    Operators:      "=" (Contains)
--
--  ===========================================================================

generic
   type L is private;
   type R is private;
package Functional.Either
  with Preelaborate, SPARK_Mode => On
is

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

   --  Is_Left_And: test if Left and predicate holds on Left value
   generic
      with function Pred (X : L) return Boolean;
   function Is_Left_And (E : Either) return Boolean;

   --  Is_Right_And: test if Right and predicate holds on Right value
   generic
      with function Pred (X : R) return Boolean;
   function Is_Right_And (E : Either) return Boolean;

   --  Is_Left_Or: test if Right or (Left and predicate holds)
   generic
      with function Pred (X : L) return Boolean;
   function Is_Left_Or (E : Either) return Boolean;

   --  Is_Right_Or: test if Left or (Right and predicate holds)
   generic
      with function Pred (X : R) return Boolean;
   function Is_Right_Or (E : Either) return Boolean;

   --  Contains: check if Right value equals given value
   --  Note: Uses predefined equality for type R
   function Contains (E : Either; Value : R) return Boolean
   with
     Post => (if E.Is_Left then not Contains'Result);

   --  ==========================================================================
   --  Extractors
   --  ==========================================================================

   function Left_Value (E : Either) return L
   with Pre => E.Is_Left, Inline;

   function Right_Value (E : Either) return R
   with Pre => not E.Is_Left, Inline;

   --  Get_Or_Else: get Right value or default
   function Get_Or_Else (E : Either; Default : R) return R;

   --  ==========================================================================
   --  Transformations
   --  ==========================================================================

   --  Map_Left: transform Left value
   generic
      with function F (X : L) return L;
   function Map_Left (E : Either) return Either
   with
     Post => (Map_Left'Result.Is_Left = E.Is_Left);

   --  Map_Right: transform Right value
   generic
      with function F (X : R) return R;
   function Map_Right (E : Either) return Either
   with
     Post => (Map_Right'Result.Is_Left = E.Is_Left);

   --  Bimap: transform both Left and Right values simultaneously
   generic
      with function Map_L (X : L) return L;
      with function Map_R (X : R) return R;
   function Bimap (E : Either) return Either
   with
     Post => (Bimap'Result.Is_Left = E.Is_Left);

   --  Map: transform Right value (Right-biased convenience for common case)
   generic
      with function F (X : R) return R;
   function Map (E : Either) return Either
   with
     Post => (Map'Result.Is_Left = E.Is_Left);

   --  Swap: exchange Left and Right values
   --  Useful for switching convention or reversing bias
   generic
      type Either_Swapped is private;
      with function Make_Left (V : R) return Either_Swapped;
      with function Make_Right (V : L) return Either_Swapped;
   function Swap (E : Either) return Either_Swapped;

   --  And_Then: chain fallible operations (Right-biased monadic bind)
   --  On Right, applies function; on Left, propagates Left unchanged
   generic
      with function F (X : R) return Either;
   function And_Then (E : Either) return Either
   with
     Post => (if E.Is_Left then And_Then'Result.Is_Left);

   --  Fold: reduce Either to single value by providing handlers for both cases
   generic
      type U is private;
      with function On_Left (X : L) return U;
      with function On_Right (X : R) return U;
   function Fold (E : Either) return U;

   --  Merge: extract value when both types are the same
   --  Requires L = R (caller instantiates with identity conversions)
   generic
      type T is private;
      with function From_Left (X : L) return T;
      with function From_Right (X : R) return T;
   function Merge (E : Either) return T;

   --  ==========================================================================
   --  Conversion
   --  ==========================================================================

   --  To_Option: convert to Option - Right(v) -> Some(v), Left(_) -> None
   generic
      type Option_Type is private;
      with function Make_Some (V : R) return Option_Type;
      with function Make_None return Option_Type;
   function To_Option (E : Either) return Option_Type;

   --  To_Result: convert to Result - Right(v) -> Ok(v), Left(e) -> Error(e)
   generic
      type Result_Type is private;
      with function Make_Ok (V : R) return Result_Type;
      with function Make_Error (E_Val : L) return Result_Type;
   function To_Result (E : Either) return Result_Type;

   --  ==========================================================================
   --  Operator Aliases
   --  ==========================================================================

   --  "=" for Contains: Either = R -> Boolean
   --  Usage: if Parse_Result = 42 then ...
   function "=" (E : Either; Value : R) return Boolean renames Contains;

end Functional.Either;
