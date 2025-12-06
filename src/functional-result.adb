pragma Ada_2022;
--  ===========================================================================
--  Functional.Result (Body)
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Implementation of Result type operations. Each transform checks Kind
--    first to implement railway-oriented short-circuit semantics.
--
--  ===========================================================================

package body Functional.Result is

   --  Constructors
   function Ok (V : T) return Result
   is ((Kind => K_Ok, Ok_Value => V));

   function Err (E_Val : E) return Result
   is ((Kind => K_Err, Err_Value => E_Val));

   function From_Error (E_Val : E) return Result
   is ((Kind => K_Err, Err_Value => E_Val));

   --  Predicates
   function Is_Ok (R : Result) return Boolean
   is (R.Kind = K_Ok);

   function Is_Err (R : Result) return Boolean
   is (R.Kind = K_Err);

   --  Extractors
   function Value (R : Result) return T
   is (R.Ok_Value);

   function Error (R : Result) return E
   is (R.Err_Value);

   function Expect (R : Result; Msg : String) return T
   is (R.Ok_Value);

   --  Unwrap with default
   function Unwrap_Or (R : Result; Default : T) return T is
   begin
      case R.Kind is
         when K_Ok =>
            return R.Ok_Value;

         when K_Err =>
            return Default;
      end case;
   end Unwrap_Or;

   function Unwrap_Or_With (R : Result) return T is
   begin
      case R.Kind is
         when K_Ok =>
            return R.Ok_Value;

         when K_Err =>
            return F;
      end case;
   end Unwrap_Or_With;

   --  Map: transform Ok value
   function Map (R : Result) return Result is
   begin
      case R.Kind is
         when K_Ok =>
            return Ok (F (R.Ok_Value));

         when K_Err =>
            return R;
      end case;
   end Map;

   --  And_Then: chain fallible operations (monadic bind)
   function And_Then (R : Result) return Result is
   begin
      case R.Kind is
         when K_Ok =>
            return F (R.Ok_Value);

         when K_Err =>
            return R;
      end case;
   end And_Then;

   --  And_Then_Into: chain with type transformation
   function And_Then_Into (R : Result) return Result_U is
   begin
      case R.Kind is
         when K_Ok =>
            return F (R.Ok_Value);

         when K_Err =>
            return Err_U (R.Err_Value);
      end case;
   end And_Then_Into;

   --  Map_Err: transform error value
   function Map_Err (R : Result) return Result is
   begin
      case R.Kind is
         when K_Ok =>
            return R;

         when K_Err =>
            return Err (F (R.Err_Value));
      end case;
   end Map_Err;

   --  Bimap: transform both Ok and Err values
   function Bimap (R : Result) return Result is
   begin
      case R.Kind is
         when K_Ok =>
            return Ok (Map_Ok (R.Ok_Value));

         when K_Err =>
            return Err (Map_Error (R.Err_Value));
      end case;
   end Bimap;

   --  Fallback: eager alternative on error
   function Fallback (A, B : Result) return Result is
   begin
      case A.Kind is
         when K_Ok =>
            return A;

         when K_Err =>
            return B;
      end case;
   end Fallback;

   --  Fallback_With: lazy alternative on error
   function Fallback_With (R : Result) return Result is
   begin
      case R.Kind is
         when K_Ok =>
            return R;

         when K_Err =>
            return F;
      end case;
   end Fallback_With;

   --  Recover: turn error into value
   function Recover (R : Result) return T is
   begin
      case R.Kind is
         when K_Ok =>
            return R.Ok_Value;

         when K_Err =>
            return Handle (R.Err_Value);
      end case;
   end Recover;

   --  Recover_With: turn error into another Result
   function Recover_With (R : Result) return Result is
   begin
      case R.Kind is
         when K_Ok =>
            return R;

         when K_Err =>
            return Handle (R.Err_Value);
      end case;
   end Recover_With;

   --  Ensure: validate Ok value with predicate
   function Ensure (R : Result) return Result is
   begin
      case R.Kind is
         when K_Ok =>
            if Pred (R.Ok_Value) then
               return R;
            else
               return Err (To_Error (R.Ok_Value));
            end if;

         when K_Err =>
            return R;
      end case;
   end Ensure;

   --  With_Context: enrich error with context
   function With_Context (R : Result; Msg : String) return Result is
   begin
      case R.Kind is
         when K_Ok =>
            return R;

         when K_Err =>
            return Err (Append (R.Err_Value, Msg));
      end case;
   end With_Context;

   --  Tap: run side effects without changing Result
   function Tap (R : Result) return Result is
   begin
      case R.Kind is
         when K_Ok =>
            On_Ok (R.Ok_Value);

         when K_Err =>
            On_Err (R.Err_Value);
      end case;
      return R;
   end Tap;

end Functional.Result;
