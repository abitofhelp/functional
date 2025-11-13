pragma Ada_2022;
--  ===========================================================================
--  Functional.Either
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Either implementation.
--
--  ===========================================================================

package body Functional.Either is

   --  Constructors
   function Left (V : L) return Either
   is ((Kind => K_Left, Left_Value => V));

   function Right (V : R) return Either
   is ((Kind => K_Right, Right_Value => V));

   --  Predicates
   function Is_Left (E : Either) return Boolean
   is (E.Kind = K_Left);

   function Is_Right (E : Either) return Boolean
   is (E.Kind = K_Right);

   --  Extractors
   function Left_Value (E : Either) return L
   is (E.Left_Value);

   function Right_Value (E : Either) return R
   is (E.Right_Value);

   --  Map_Left: transform Left value
   function Map_Left (E : Either) return Either is
   begin
      case E.Kind is
         when K_Left =>
            return Left (F (E.Left_Value));

         when K_Right =>
            return E;
      end case;
   end Map_Left;

   --  Map_Right: transform Right value
   function Map_Right (E : Either) return Either is
   begin
      case E.Kind is
         when K_Left =>
            return E;

         when K_Right =>
            return Right (F (E.Right_Value));
      end case;
   end Map_Right;

   --  Bimap: transform both Left and Right values simultaneously
   function Bimap (E : Either) return Either is
   begin
      case E.Kind is
         when K_Left =>
            return Left (Map_L (E.Left_Value));

         when K_Right =>
            return Right (Map_R (E.Right_Value));
      end case;
   end Bimap;

   --  Fold: reduce Either to single value
   function Fold (E : Either) return U is
   begin
      case E.Kind is
         when K_Left =>
            return On_Left (E.Left_Value);

         when K_Right =>
            return On_Right (E.Right_Value);
      end case;
   end Fold;

end Functional.Either;
