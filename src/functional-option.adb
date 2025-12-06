pragma Ada_2022;
--  ===========================================================================
--  Functional.Option (Body)
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Implementation of Option type operations. Transforms check Has_Value
--    first and pass through None unchanged, applying functions only to Some.
--
--  ===========================================================================

package body Functional.Option is

   --  Constructors
   function New_Some (V : T) return Option
   is ((Has_Value => True, Value => V));

   function None return Option
   is ((Has_Value => False));

   --  Predicates
   function Is_Some (O : Option) return Boolean
   is (O.Has_Value);

   function Is_None (O : Option) return Boolean
   is (not O.Has_Value);

   --  Extractors
   function Value (O : Option) return T
   is (O.Value);

   --  Unwrap with default
   function Unwrap_Or (O : Option; Default : T) return T is
   begin
      case O.Has_Value is
         when True =>
            return O.Value;

         when False =>
            return Default;
      end case;
   end Unwrap_Or;

   function Unwrap_Or_With (O : Option) return T is
   begin
      case O.Has_Value is
         when True =>
            return O.Value;

         when False =>
            return F;
      end case;
   end Unwrap_Or_With;

   --  Map: transform Some value
   function Map (O : Option) return Option is
   begin
      case O.Has_Value is
         when True =>
            return New_Some (F (O.Value));

         when False =>
            return O;
      end case;
   end Map;

   --  And_Then: chain optional operations (monadic bind)
   function And_Then (O : Option) return Option is
   begin
      case O.Has_Value is
         when True =>
            return F (O.Value);

         when False =>
            return O;
      end case;
   end And_Then;

   --  Filter: keep value only if predicate holds
   function Filter (O : Option) return Option is
   begin
      case O.Has_Value is
         when True =>
            if Pred (O.Value) then
               return O;
            else
               return None;
            end if;

         when False =>
            return O;
      end case;
   end Filter;

   --  Or_Else: eager fallback
   function Or_Else (A, B : Option) return Option is
   begin
      if A.Has_Value then
         return A;
      else
         return B;
      end if;
   end Or_Else;

   --  Or_Else_With: lazy fallback
   function Or_Else_With (O : Option) return Option is
   begin
      case O.Has_Value is
         when True =>
            return O;

         when False =>
            return F;
      end case;
   end Or_Else_With;

end Functional.Option;
