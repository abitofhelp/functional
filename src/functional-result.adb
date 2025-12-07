pragma Ada_2022;
--  ===========================================================================
--  Functional.Result (Body)
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Implementation of Result type operations. Each transform checks Is_Ok
--    first to implement railway-oriented short-circuit semantics.
--
--  ===========================================================================

package body Functional.Result is

   --  Constructors
   function Ok (V : T) return Result
   is ((Is_Ok => True, Ok_Value => V));

   function New_Error (E_Val : E) return Result
   is ((Is_Ok => False, Error_Value => E_Val));

   --  Predicates
   function Is_Ok (R : Result) return Boolean
   is (R.Is_Ok);

   function Is_Error (R : Result) return Boolean
   is (not R.Is_Ok);

   function Is_Ok_And (R : Result) return Boolean is
     (R.Is_Ok and then Pred (R.Ok_Value));

   function Is_Error_And (R : Result) return Boolean is
     (not R.Is_Ok and then Pred (R.Error_Value));

   function Is_Ok_Or (R : Result) return Boolean is
     (not R.Is_Ok or else Pred (R.Ok_Value));

   function Is_Error_Or (R : Result) return Boolean is
     (R.Is_Ok or else Pred (R.Error_Value));

   function Contains (R : Result; Value : T) return Boolean is
     (R.Is_Ok and then R.Ok_Value = Value);

   --  Extractors
   function Value (R : Result) return T
   is (R.Ok_Value);

   function Error (R : Result) return E
   is (R.Error_Value);

   function Expect (R : Result; Msg : String) return T
   is (R.Ok_Value);

   function Expect_Error (R : Result; Msg : String) return E
   is (R.Error_Value);

   function Unwrap_Error (R : Result) return E
   is (R.Error_Value);

   --  Unwrap with default
   function Unwrap_Or (R : Result; Default : T) return T is
   begin
      case R.Is_Ok is
         when True =>
            return R.Ok_Value;

         when False =>
            return Default;
      end case;
   end Unwrap_Or;

   function Unwrap_Or_With (R : Result) return T is
   begin
      case R.Is_Ok is
         when True =>
            return R.Ok_Value;

         when False =>
            return F;
      end case;
   end Unwrap_Or_With;

   --  Map: transform Ok value
   function Map (R : Result) return Result is
   begin
      case R.Is_Ok is
         when True =>
            return Ok (F (R.Ok_Value));

         when False =>
            return R;
      end case;
   end Map;

   --  Map_Or: transform Ok value or return default
   function Map_Or (R : Result; Default : T) return T is
   begin
      case R.Is_Ok is
         when True =>
            return F (R.Ok_Value);

         when False =>
            return Default;
      end case;
   end Map_Or;

   --  Map_Or_Else: transform Ok value or call default producer
   function Map_Or_Else (R : Result) return T is
   begin
      case R.Is_Ok is
         when True =>
            return F (R.Ok_Value);

         when False =>
            return Default;
      end case;
   end Map_Or_Else;

   --  And_Then: chain fallible operations (monadic bind)
   function And_Then (R : Result) return Result is
   begin
      case R.Is_Ok is
         when True =>
            return F (R.Ok_Value);

         when False =>
            return R;
      end case;
   end And_Then;

   --  And_Then_Into: chain with type transformation
   function And_Then_Into (R : Result) return Result_U is
   begin
      case R.Is_Ok is
         when True =>
            return F (R.Ok_Value);

         when False =>
            return Error_U (R.Error_Value);
      end case;
   end And_Then_Into;

   --  Map_Error: transform error value
   function Map_Error (R : Result) return Result is
   begin
      case R.Is_Ok is
         when True =>
            return R;

         when False =>
            return New_Error (F (R.Error_Value));
      end case;
   end Map_Error;

   --  Bimap: transform both Ok and Err values
   function Bimap (R : Result) return Result is
   begin
      case R.Is_Ok is
         when True =>
            return Ok (Map_Ok (R.Ok_Value));

         when False =>
            return New_Error (Map_Error (R.Error_Value));
      end case;
   end Bimap;

   --  Fallback: eager alternative on error
   function Fallback (A, B : Result) return Result is
   begin
      case A.Is_Ok is
         when True =>
            return A;

         when False =>
            return B;
      end case;
   end Fallback;

   --  Fallback_With: lazy alternative on error
   function Fallback_With (R : Result) return Result is
   begin
      case R.Is_Ok is
         when True =>
            return R;

         when False =>
            return F;
      end case;
   end Fallback_With;

   --  Recover: turn error into value
   function Recover (R : Result) return T is
   begin
      case R.Is_Ok is
         when True =>
            return R.Ok_Value;

         when False =>
            return Handle (R.Error_Value);
      end case;
   end Recover;

   --  Recover_With: turn error into another Result
   function Recover_With (R : Result) return Result is
   begin
      case R.Is_Ok is
         when True =>
            return R;

         when False =>
            return Handle (R.Error_Value);
      end case;
   end Recover_With;

   --  Ensure: validate Ok value with predicate
   function Ensure (R : Result) return Result is
   begin
      case R.Is_Ok is
         when True =>
            if Pred (R.Ok_Value) then
               return R;
            else
               return New_Error (To_Error (R.Ok_Value));
            end if;

         when False =>
            return R;
      end case;
   end Ensure;

   --  With_Context: enrich error with context
   function With_Context (R : Result; Msg : String) return Result is
   begin
      case R.Is_Ok is
         when True =>
            return R;

         when False =>
            return New_Error (Append (R.Error_Value, Msg));
      end case;
   end With_Context;

   --  Tap: run side effects without changing Result
   function Tap (R : Result) return Result is
   begin
      case R.Is_Ok is
         when True =>
            On_Ok (R.Ok_Value);

         when False =>
            On_Error (R.Error_Value);
      end case;
      return R;
   end Tap;

   --  Tap_Ok: run side effect only on Ok value
   function Tap_Ok (R : Result) return Result is
   begin
      if R.Is_Ok then
         On_Ok (R.Ok_Value);
      end if;
      return R;
   end Tap_Ok;

   --  Tap_Error: run side effect only on Error value
   function Tap_Error (R : Result) return Result is
   begin
      if not R.Is_Ok then
         On_Error (R.Error_Value);
      end if;
      return R;
   end Tap_Error;

   --  Zip_With: combine two Results with a function
   function Zip_With (A : Result; B : Result_U) return Result is
   begin
      if not A.Is_Ok then
         return A;
      elsif not Is_Ok_U (B) then
         return New_Error (Error_U (B));
      else
         return Ok (Combine (A.Ok_Value, Value_U (B)));
      end if;
   end Zip_With;

   --  Flatten: Result[Result[T,E],E] -> Result[T,E]
   function Flatten (Outer : Result) return Result is
   begin
      if not Outer.Is_Ok then
         return Outer;
      end if;

      declare
         Inner : constant Inner_Result := Convert (Outer.Ok_Value);
      begin
         if Is_Ok_Inner (Inner) then
            return Ok (Value_Inner (Inner));
         else
            return New_Error (Error_Inner (Inner));
         end if;
      end;
   end Flatten;

   --  To_Option: convert to Option - Ok(v) -> Some(v), Error(_) -> None
   function To_Option (R : Result) return Option_Type is
   begin
      if R.Is_Ok then
         return Make_Some (R.Ok_Value);
      else
         return Make_None;
      end if;
   end To_Option;

end Functional.Result;
