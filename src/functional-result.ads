pragma Ada_2022;
--  ===========================================================================
--  Functional.Result - Type-Safe Error Handling
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Generic Result type for explicit error handling. Represents either a
--    success value (Ok) or an error value (Err). Enables railway-oriented
--    programming with composable operations like Map, And_Then, and Recover.
--
--  Key Types:
--    Result       - Discriminated record with Is_Ok : Boolean
--                   When True, holds Ok_Value; when False, holds Error_Value
--
--  Operations (36):
--    Constructors: Ok, New_Error, From_Error
--    Predicates:   Is_Ok, Is_Error, Is_Ok_And, Is_Ok_Or, Is_Error_And, Is_Error_Or, Contains
--    Extractors:   Value, Error, Expect, Expect_Error, Unwrap_Error
--    Defaults:     Unwrap_Or, Unwrap_Or_With
--    Transforms:   Map, Map_Or, Map_Or_Else, And_Then, And_Then_Into,
--                  Map_Error, Bimap, Zip_With, Flatten
--    Recovery:     Fallback, Fallback_With, Recover, Recover_With
--    Validation:   Ensure, With_Context
--    Side Effects: Tap, Tap_Ok, Tap_Error
--    Conversion:   To_Option
--    Operators:    "or" (Unwrap_Or), "or" (Fallback), "=" (Contains)
--
--  ===========================================================================

generic
   type T is private;
   type E is private;
package Functional.Result
  with Preelaborate, SPARK_Mode => On
is

   type Result (Is_Ok : Boolean := True) is record
      case Is_Ok is
         when True =>
            Ok_Value : T;

         when False =>
            Error_Value : E;
      end case;
   end record;

   --  ==========================================================================
   --  Constructors
   --  ==========================================================================

   function Ok (V : T) return Result
   with Inline, Post => Is_Ok (Ok'Result);
   function New_Error (E_Val : E) return Result
   with Inline, Post => not Is_Ok (New_Error'Result);

   --  From_Error: construct Result from pre-existing error value
   --  Used at infrastructure boundaries when converting exceptions to Results
   --  Alias for New_Error for semantic clarity at exception boundaries
   function From_Error (E_Val : E) return Result renames New_Error;

   --  ==========================================================================
   --  Predicates
   --  ==========================================================================

   function Is_Ok (R : Result) return Boolean
   with Inline;
   function Is_Error (R : Result) return Boolean
   with Inline;

   --  Is_Ok_And: test if Ok and predicate holds
   generic
      with function Pred (X : T) return Boolean;
   function Is_Ok_And (R : Result) return Boolean;

   --  Is_Error_And: test if Error and predicate holds
   generic
      with function Pred (X : E) return Boolean;
   function Is_Error_And (R : Result) return Boolean;

   --  Is_Ok_Or: test if Error or (Ok and predicate holds)
   --  Useful for validation: "error is ok, or value must satisfy condition"
   generic
      with function Pred (X : T) return Boolean;
   function Is_Ok_Or (R : Result) return Boolean;

   --  Is_Error_Or: test if Ok or (Error and predicate holds)
   --  Useful for validation: "success is ok, or error must satisfy condition"
   generic
      with function Pred (X : E) return Boolean;
   function Is_Error_Or (R : Result) return Boolean;

   --  Contains: check if Ok value equals given value
   --  Note: Uses predefined equality for type T
   function Contains (R : Result; Value : T) return Boolean
   with
     Post => (if not R.Is_Ok then not Contains'Result);

   --  ==========================================================================
   --  Extractors
   --  ==========================================================================

   function Value (R : Result) return T
   with Pre => R.Is_Ok, Inline;

   function Error (R : Result) return E
   with Pre => not R.Is_Ok, Inline;

   --  Expect: extract value or raise with custom message
   --  Forces programmer to document why they believe Result is Ok
   function Expect (R : Result; Msg : String) return T
   with Pre => R.Is_Ok or else raise Program_Error with Msg;

   --  Expect_Error: extract error or raise with custom message
   --  Forces programmer to document why they believe Result is Error
   function Expect_Error (R : Result; Msg : String) return E
   with Pre => not R.Is_Ok or else raise Program_Error with Msg;

   --  Unwrap_Error: extract error, raise Program_Error if Ok
   --  For use when error is expected (e.g., in test assertions)
   function Unwrap_Error (R : Result) return E
   with Pre => not R.Is_Ok;

   --  ==========================================================================
   --  Unwrap with defaults
   --  ==========================================================================

   function Unwrap_Or (R : Result; Default : T) return T
   with
     Post =>
       (if R.Is_Ok then Unwrap_Or'Result = R.Ok_Value
        else Unwrap_Or'Result = Default);

   generic
      with function F return T;
   function Unwrap_Or_With (R : Result) return T;

   --  ==========================================================================
   --  Mapping and transformation
   --  ==========================================================================

   --  Map: transform Ok value (keeps same type)
   generic
      with function F (X : T) return T;
   function Map (R : Result) return Result
   with
     Post => (if R.Is_Ok then Map'Result.Is_Ok
              else not Map'Result.Is_Ok);

   --  Map_Or: transform Ok value or return default (eager)
   generic
      with function F (X : T) return T;
   function Map_Or (R : Result; Default : T) return T;

   --  Map_Or_Else: transform Ok value or call default producer (lazy)
   generic
      with function F (X : T) return T;
      with function Default return T;
   function Map_Or_Else (R : Result) return T;

   --  And_Then: chain fallible operations (monadic bind - the workhorse!)
   generic
      with function F (X : T) return Result;
   function And_Then (R : Result) return Result
   with
     Post => (if not R.Is_Ok then not And_Then'Result.Is_Ok);

   --  And_Then_Into: chain fallible operations with type transformation
   --  Transforms Result[T, E] -> Result[U, E] where U is a different success type
   --  Requires caller to provide target Result error constructor for error propagation
   generic
      type Result_U is private;
      with function Error_U (E_Val : E) return Result_U;
      with function F (X : T) return Result_U;
   function And_Then_Into (R : Result) return Result_U;

   --  Map_Error: transform error value
   generic
      with function F (X : E) return E;
   function Map_Error (R : Result) return Result
   with
     Post => (if R.Is_Ok then Map_Error'Result.Is_Ok
              else not Map_Error'Result.Is_Ok);

   --  Bimap: transform both Ok and Err values simultaneously
   generic
      with function Map_Ok (X : T) return T;
      with function Map_Error (E_Val : E) return E;
   function Bimap (R : Result) return Result
   with
     Post => (Bimap'Result.Is_Ok = R.Is_Ok);

   --  ==========================================================================
   --  Fallback and recovery
   --  ==========================================================================

   --  Fallback: try alternative on error (eager evaluation)
   function Fallback (A, B : Result) return Result
   with
     Post => (if A.Is_Ok then Fallback'Result.Is_Ok);

   --  Fallback_With: try alternative on error (lazy evaluation)
   generic
      with function F return Result;
   function Fallback_With (R : Result) return Result;

   --  Recover: turn error into value
   generic
      with function Handle (E_Val : E) return T;
   function Recover (R : Result) return T;

   --  Recover_With: turn error into another Result
   generic
      with function Handle (E_Val : E) return Result;
   function Recover_With (R : Result) return Result;

   --  ==========================================================================
   --  Validation and context
   --  ==========================================================================

   --  Ensure: validate Ok value with predicate
   generic
      with function Pred (X : T) return Boolean;
      with function To_Error (X : T) return E;
   function Ensure (R : Result) return Result;

   --  With_Context: enrich error with context (for debugging breadcrumbs)
   generic
      with function Append (X : E; Msg : String) return E;
   function With_Context (R : Result; Msg : String) return Result;

   --  ==========================================================================
   --  Side effects
   --  ==========================================================================

   --  Tap: run side effects without changing Result (for logging/debugging)
   generic
      with procedure On_Ok (V : T);
      with procedure On_Error (E_Val : E);
   function Tap (R : Result) return Result;

   --  Tap_Ok: run side effect only on Ok value
   generic
      with procedure On_Ok (V : T);
   function Tap_Ok (R : Result) return Result;

   --  Tap_Error: run side effect only on Error value
   generic
      with procedure On_Error (E_Val : E);
   function Tap_Error (R : Result) return Result;

   --  ==========================================================================
   --  Operator Aliases (Ada idioms for ergonomic syntax)
   --  ==========================================================================

   --  "or" for Unwrap_Or: Result or Default -> T
   --  Usage: Port : Integer := Port_Result or 8080;
   function "or" (R : Result; Default : T) return T renames Unwrap_Or;

   --  "or" for Fallback: Result or Result -> Result
   --  Usage: Config := Primary_Config or Backup_Config;
   function "or" (Left, Right : Result) return Result renames Fallback;

   --  "=" for Contains: Result = T -> Boolean
   --  Usage: if Port_Result = 8080 then ...
   function "=" (R : Result; Value : T) return Boolean renames Contains;

   --  ==========================================================================
   --  Combining and Flattening
   --  ==========================================================================

   --  Zip_With: combine two Results with a function
   --  Returns Ok(Combine(A.Value, B.Value)) if both are Ok, else first Error
   generic
      type U is private;
      type Result_U is private;
      with function Is_Ok_U (R : Result_U) return Boolean;
      with function Value_U (R : Result_U) return U;
      with function Error_U (R : Result_U) return E;
      with function Combine (A : T; B : U) return T;
   function Zip_With (A : Result; B : Result_U) return Result;

   --  Flatten: Result[Result[T,E],E] -> Result[T,E]
   --  Ok(Ok(v)) -> Ok(v), Ok(Err(e)) -> Err(e), Err(e) -> Err(e)
   --  Note: Convert allows T to be accessed as Inner_Result (identity when T = Inner_Result)
   generic
      type Inner_Result is private;
      with function Convert (V : T) return Inner_Result;
      with function Is_Ok_Inner (R : Inner_Result) return Boolean;
      with function Value_Inner (R : Inner_Result) return T;
      with function Error_Inner (R : Inner_Result) return E;
   function Flatten (Outer : Result) return Result;

   --  ==========================================================================
   --  Conversion to Option
   --  ==========================================================================

   --  To_Option: convert to Option - Ok(v) -> Some(v), Error(_) -> None
   generic
      type Option_Type is private;
      with function Make_Some (V : T) return Option_Type;
      with function Make_None return Option_Type;
   function To_Option (R : Result) return Option_Type;

end Functional.Result;
