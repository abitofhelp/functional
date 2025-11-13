pragma Ada_2022;
--  ===========================================================================
--  Functional.Result
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Result interface and type definitions.
--
--  Key Types:
--    T
--    E
--    Result_Kind
--    Result
--
--  Dependencies:
--    Inline
--    Inline
--    Inline
--
--  ===========================================================================

generic
   type T is private;
   type E is private;
package Functional.Result is

   type Result_Kind is (K_Ok, K_Err);

   type Result (Kind : Result_Kind := K_Ok) is record
      case Kind is
         when K_Ok =>
            Ok_Value : T;

         when K_Err =>
            Err_Value : E;
      end case;
   end record;

   --  ==========================================================================
   --  Constructors
   --  ==========================================================================

   function Ok (V : T) return Result
   with Inline;
   function Err (E_Val : E) return Result
   with Inline;

   --  ==========================================================================
   --  Predicates
   --  ==========================================================================

   function Is_Ok (R : Result) return Boolean
   with Inline;
   function Is_Err (R : Result) return Boolean
   with Inline;

   --  ==========================================================================
   --  Extractors
   --  ==========================================================================

   function Value (R : Result) return T
   with Pre => R.Kind = K_Ok, Inline;

   function Error (R : Result) return E
   with Pre => R.Kind = K_Err, Inline;

   --  Expect: extract value or raise with custom message
   --  Forces programmer to document why they believe Result is Ok
   function Expect (R : Result; Msg : String) return T
   with Pre => R.Kind = K_Ok or else raise Program_Error with Msg;

   --  ==========================================================================
   --  Unwrap with defaults
   --  ==========================================================================

   function Unwrap_Or (R : Result; Default : T) return T
   with
     Post =>
       (if R.Kind = K_Ok then Unwrap_Or'Result = R.Ok_Value
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

   --  Map_Err: transform error value
   generic
      with function F (X : E) return E;
   function Map_Err (R : Result) return Result;

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
      with procedure On_Err (E_Val : E);
   function Tap (R : Result) return Result;

end Functional.Result;
