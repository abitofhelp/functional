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
--  Operations (22):
--    Constructors: Ok, New_Error, From_Error
--    Predicates:   Is_Ok, Is_Error
--    Extractors:   Value, Error, Expect
--    Defaults:     Unwrap_Or, Unwrap_Or_With
--    Transforms:   Map, And_Then, And_Then_Into, Map_Error, Bimap
--    Recovery:     Fallback, Fallback_With, Recover, Recover_With
--    Validation:   Ensure, With_Context
--    Side Effects: Tap
--    Operators:    "or" (Unwrap_Or), "or" (Fallback)
--
--  ===========================================================================

generic
   type T is private;
   type E is private;
package Functional.Result is

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
   with Inline;
   function New_Error (E_Val : E) return Result
   with Inline;

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
   function Map (R : Result) return Result;

   --  And_Then: chain fallible operations (monadic bind - the workhorse!)
   generic
      with function F (X : T) return Result;
   function And_Then (R : Result) return Result;

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
   function Map_Error (R : Result) return Result;

   --  Bimap: transform both Ok and Err values simultaneously
   generic
      with function Map_Ok (X : T) return T;
      with function Map_Error (E_Val : E) return E;
   function Bimap (R : Result) return Result;

   --  ==========================================================================
   --  Fallback and recovery
   --  ==========================================================================

   --  Fallback: try alternative on error (eager evaluation)
   function Fallback (A, B : Result) return Result;

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

   --  ==========================================================================
   --  Operator Aliases (Ada idioms for ergonomic syntax)
   --  ==========================================================================

   --  "or" for Unwrap_Or: Result or Default -> T
   --  Usage: Port : Integer := Port_Result or 8080;
   function "or" (R : Result; Default : T) return T renames Unwrap_Or;

   --  "or" for Fallback: Result or Result -> Result
   --  Usage: Config := Primary_Config or Backup_Config;
   function "or" (Left, Right : Result) return Result renames Fallback;

end Functional.Result;
