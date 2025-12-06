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

   --  Extractors
   function Left_Value (E : Either) return L
   is (E.Left_Value);

   function Right_Value (E : Either) return R
   is (E.Right_Value);

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

end Functional.Either;
