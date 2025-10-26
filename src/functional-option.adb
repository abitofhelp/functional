pragma Ada_2022;
--  ============================================================================
--  Functional.Option - Implementation
--  ============================================================================
--  Copyright (c) 2025 A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  ============================================================================

package body Functional.Option is

   --  Constructors
   function New_Some (V : T) return Option
   is ((Kind => K_Some, Value => V));

   function None return Option
   is ((Kind => K_None));

   --  Predicates
   function Is_Some (O : Option) return Boolean
   is (O.Kind = K_Some);

   function Is_None (O : Option) return Boolean
   is (O.Kind = K_None);

   --  Extractors
   function Value (O : Option) return T
   is (O.Value);

   --  Unwrap with default
   function Unwrap_Or (O : Option; Default : T) return T is
   begin
      case O.Kind is
         when K_Some =>
            return O.Value;

         when K_None =>
            return Default;
      end case;
   end Unwrap_Or;

   function Unwrap_Or_With (O : Option) return T is
   begin
      case O.Kind is
         when K_Some =>
            return O.Value;

         when K_None =>
            return F;
      end case;
   end Unwrap_Or_With;

   --  Map: transform Some value
   function Map (O : Option) return Option is
   begin
      case O.Kind is
         when K_Some =>
            return New_Some (F (O.Value));

         when K_None =>
            return O;
      end case;
   end Map;

   --  And_Then: chain optional operations (monadic bind)
   function And_Then (O : Option) return Option is
   begin
      case O.Kind is
         when K_Some =>
            return F (O.Value);

         when K_None =>
            return O;
      end case;
   end And_Then;

   --  Filter: keep value only if predicate holds
   function Filter (O : Option) return Option is
   begin
      case O.Kind is
         when K_Some =>
            if Pred (O.Value) then
               return O;
            else
               return None;
            end if;

         when K_None =>
            return O;
      end case;
   end Filter;

   --  Or_Else: eager fallback
   function Or_Else (A, B : Option) return Option is
   begin
      if A.Kind = K_Some then
         return A;
      else
         return B;
      end if;
   end Or_Else;

   --  Or_Else_With: lazy fallback
   function Or_Else_With (O : Option) return Option is
   begin
      case O.Kind is
         when K_Some =>
            return O;

         when K_None =>
            return F;
      end case;
   end Or_Else_With;

end Functional.Option;
