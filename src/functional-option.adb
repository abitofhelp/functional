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

   function Is_Some_And (O : Option) return Boolean is
     (O.Has_Value and then Pred (O.Value));

   function Is_None_Or (O : Option) return Boolean is
     (not O.Has_Value or else Pred (O.Value));

   function Contains (O : Option; Value : T) return Boolean is
     (O.Has_Value and then O.Value = Value);

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

   --  Map_Or: transform Some value or return default
   function Map_Or (O : Option; Default : T) return T is
   begin
      case O.Has_Value is
         when True =>
            return F (O.Value);

         when False =>
            return Default;
      end case;
   end Map_Or;

   --  Map_Or_Else: transform Some value or call default producer
   function Map_Or_Else (O : Option) return T is
   begin
      case O.Has_Value is
         when True =>
            return F (O.Value);

         when False =>
            return Default;
      end case;
   end Map_Or_Else;

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

   --  "and": returns B if both have values, else None
   function "and" (A, B : Option) return Option is
   begin
      if A.Has_Value and B.Has_Value then
         return B;
      else
         return None;
      end if;
   end "and";

   --  "xor": returns the value if exactly one has a value
   function "xor" (A, B : Option) return Option is
   begin
      if A.Has_Value and not B.Has_Value then
         return A;
      elsif B.Has_Value and not A.Has_Value then
         return B;
      else
         return None;
      end if;
   end "xor";

   --  Tap: run side effect without changing Option
   function Tap (O : Option) return Option is
   begin
      if O.Has_Value then
         On_Some (O.Value);
      end if;
      return O;
   end Tap;

   --  Zip_With: combine two Options with a function
   function Zip_With (A : Option; B : Option_U) return Option is
   begin
      if A.Has_Value and Has_Value_U (B) then
         return New_Some (Combine (A.Value, Value_U (B)));
      else
         return None;
      end if;
   end Zip_With;

   --  Flatten: Option[Option[T]] -> Option[T]
   function Flatten (Outer : Option) return Inner_Option is
   begin
      if not Outer.Has_Value then
         return None_Inner;
      end if;

      declare
         Inner : constant Inner_Option := Convert (Outer.Value);
      begin
         if Has_Value_Inner (Inner) then
            return Inner;
         else
            return None_Inner;
         end if;
      end;
   end Flatten;

   --  Ok_Or: convert to Result - Some(v) -> Ok(v), None -> Error(e)
   function Ok_Or (O : Option; Error : Error_Type) return Result_Type is
   begin
      if O.Has_Value then
         return Make_Ok (O.Value);
      else
         return Make_Error (Error);
      end if;
   end Ok_Or;

   --  Ok_Or_Else: convert to Result with lazy error production
   function Ok_Or_Else (O : Option) return Result_Type is
   begin
      if O.Has_Value then
         return Make_Ok (O.Value);
      else
         return Make_Error (Produce_Error);
      end if;
   end Ok_Or_Else;

end Functional.Option;
