pragma Ada_2022;
--  ===========================================================================
--  Functional.Either (Body)
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Implementation of Either type operations. Map variants apply functions
--    to matching side only. Fold dispatches to appropriate handler function.
--
--  ===========================================================================

package body Functional.Either is

   --  Constructors
   function Left (V : L) return Either
   is ((Is_Left => True, Left_Value => V));

   function Right (V : R) return Either
   is ((Is_Left => False, Right_Value => V));

   --  Predicates
   function Is_Left (E : Either) return Boolean
   is (E.Is_Left);

   function Is_Right (E : Either) return Boolean
   is (not E.Is_Left);

   function Is_Left_And (E : Either) return Boolean is
     (E.Is_Left and then Pred (E.Left_Value));

   function Is_Right_And (E : Either) return Boolean is
     (not E.Is_Left and then Pred (E.Right_Value));

   function Is_Left_Or (E : Either) return Boolean is
     (not E.Is_Left or else Pred (E.Left_Value));

   function Is_Right_Or (E : Either) return Boolean is
     (E.Is_Left or else Pred (E.Right_Value));

   function Contains (E : Either; Value : R) return Boolean is
     (not E.Is_Left and then E.Right_Value = Value);

   --  Extractors
   function Left_Value (E : Either) return L
   is (E.Left_Value);

   function Right_Value (E : Either) return R
   is (E.Right_Value);

   function Get_Or_Else (E : Either; Default : R) return R is
     (if E.Is_Left then Default else E.Right_Value);

   --  Map_Left: transform Left value
   function Map_Left (E : Either) return Either is
   begin
      case E.Is_Left is
         when True =>
            return Left (F (E.Left_Value));

         when False =>
            return E;
      end case;
   end Map_Left;

   --  Map_Right: transform Right value
   function Map_Right (E : Either) return Either is
   begin
      case E.Is_Left is
         when True =>
            return E;

         when False =>
            return Right (F (E.Right_Value));
      end case;
   end Map_Right;

   --  Bimap: transform both Left and Right values simultaneously
   function Bimap (E : Either) return Either is
   begin
      case E.Is_Left is
         when True =>
            return Left (Map_L (E.Left_Value));

         when False =>
            return Right (Map_R (E.Right_Value));
      end case;
   end Bimap;

   --  Map: transform Right value (Right-biased convenience)
   function Map (E : Either) return Either is
   begin
      case E.Is_Left is
         when True =>
            return E;

         when False =>
            return Right (F (E.Right_Value));
      end case;
   end Map;

   --  Swap: exchange Left and Right values
   function Swap (E : Either) return Either_Swapped is
   begin
      case E.Is_Left is
         when True =>
            return Make_Right (E.Left_Value);

         when False =>
            return Make_Left (E.Right_Value);
      end case;
   end Swap;

   --  And_Then: chain fallible operations (Right-biased monadic bind)
   function And_Then (E : Either) return Either is
   begin
      case E.Is_Left is
         when True =>
            return E;

         when False =>
            return F (E.Right_Value);
      end case;
   end And_Then;

   --  Fold: reduce Either to single value
   function Fold (E : Either) return U is
   begin
      case E.Is_Left is
         when True =>
            return On_Left (E.Left_Value);

         when False =>
            return On_Right (E.Right_Value);
      end case;
   end Fold;

   --  Merge: extract value when both types are the same
   function Merge (E : Either) return T is
   begin
      case E.Is_Left is
         when True =>
            return From_Left (E.Left_Value);

         when False =>
            return From_Right (E.Right_Value);
      end case;
   end Merge;

   --  To_Option: convert to Option - Right(v) -> Some(v), Left(_) -> None
   function To_Option (E : Either) return Option_Type is
   begin
      if E.Is_Left then
         return Make_None;
      else
         return Make_Some (E.Right_Value);
      end if;
   end To_Option;

   --  To_Result: convert to Result - Right(v) -> Ok(v), Left(e) -> Error(e)
   function To_Result (E : Either) return Result_Type is
   begin
      if E.Is_Left then
         return Make_Error (E.Left_Value);
      else
         return Make_Ok (E.Right_Value);
      end if;
   end To_Result;

end Functional.Either;
