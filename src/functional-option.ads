pragma Ada_2022;
--  ===========================================================================
--  Functional.Option - Optional Values
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Generic Option type for representing optional values. Encodes presence
--    (Some) or absence (None) in the type system, eliminating null references.
--    Supports Preelaborate for use in domain layers with strict elaboration.
--
--  Key Types:
--    Option       - Discriminated record with Has_Value : Boolean
--                   When True, holds Value; when False, empty
--
--  Operations (25):
--    Constructors: New_Some, None
--    Predicates:   Is_Some, Is_None, Is_Some_And, Contains
--    Extractors:   Value, Expect
--    Defaults:     Unwrap_Or, Unwrap_Or_With
--    Transforms:   Map, Map_Or, Map_Or_Else, And_Then, Filter, Zip_With, Flatten
--    Fallback:     Or_Else, Or_Else_With, Fallback (alias)
--    Side Effects: Tap
--    Conversion:   Ok_Or, Ok_Or_Else
--    Operators:    "or" (Unwrap_Or), "or" (Or_Else), "and", "xor", "=" (Contains)
--
--  ===========================================================================

generic
   type T is private;
package Functional.Option
  with Preelaborate, SPARK_Mode => On
is

   type Option (Has_Value : Boolean := False) is record
      case Has_Value is
         when True =>
            Value : T;

         when False =>
            null;
      end case;
   end record;

   --  ==========================================================================
   --  Constructors
   --  ==========================================================================

   function New_Some (V : T) return Option
   with Inline;
   function None return Option
   with Inline;

   --  ==========================================================================
   --  Predicates
   --  ==========================================================================

   function Is_Some (O : Option) return Boolean
   with Inline;
   function Is_None (O : Option) return Boolean
   with Inline;

   --  Is_Some_And: test if Some and predicate holds
   generic
      with function Pred (X : T) return Boolean;
   function Is_Some_And (O : Option) return Boolean;

   --  Contains: check if Some value equals given value
   --  Note: Uses predefined equality for type T
   function Contains (O : Option; Value : T) return Boolean
   with
     Post => (if not O.Has_Value then not Contains'Result);

   --  ==========================================================================
   --  Extractors
   --  ==========================================================================

   function Value (O : Option) return T
   with Pre => O.Has_Value, Inline;

   --  Expect: extract value or raise with custom message
   --  Forces programmer to document why they believe Option is Some
   function Expect (O : Option; Msg : String) return T
   with Pre => O.Has_Value or else raise Program_Error with Msg;

   --  ==========================================================================
   --  Unwrap with defaults
   --  ==========================================================================

   function Unwrap_Or (O : Option; Default : T) return T
   with
     Post =>
       (if O.Has_Value then Unwrap_Or'Result = O.Value
        else Unwrap_Or'Result = Default);

   generic
      with function F return T;
   function Unwrap_Or_With (O : Option) return T;

   --  ==========================================================================
   --  Mapping and chaining
   --  ==========================================================================

   --  Map: transform Some value
   generic
      with function F (X : T) return T;
   function Map (O : Option) return Option
   with
     Post => (if O.Has_Value then Map'Result.Has_Value
              else not Map'Result.Has_Value);

   --  Map_Or: transform Some value or return default (eager)
   generic
      with function F (X : T) return T;
   function Map_Or (O : Option; Default : T) return T;

   --  Map_Or_Else: transform Some value or call default producer (lazy)
   generic
      with function F (X : T) return T;
      with function Default return T;
   function Map_Or_Else (O : Option) return T;

   --  And_Then: chain optional operations (monadic bind)
   generic
      with function F (X : T) return Option;
   function And_Then (O : Option) return Option
   with
     Post => (if not O.Has_Value then not And_Then'Result.Has_Value);

   --  Filter: keep value only if predicate holds
   generic
      with function Pred (X : T) return Boolean;
   function Filter (O : Option) return Option
   with
     Post => (if not O.Has_Value then not Filter'Result.Has_Value);

   --  ==========================================================================
   --  Fallback
   --  ==========================================================================

   --  Or_Else: fallback to alternative (eager evaluation)
   function Or_Else (A, B : Option) return Option
   with
     Post => (if A.Has_Value then Or_Else'Result.Has_Value);

   --  Or_Else_With: fallback to alternative (lazy evaluation)
   generic
      with function F return Option;
   function Or_Else_With (O : Option) return Option;

   --  Aliases for discoverability (Result-style naming)
   function Fallback (A, B : Option) return Option renames Or_Else;

   --  ==========================================================================
   --  Side Effects
   --  ==========================================================================

   --  Tap: run side effect without changing Option (for logging/debugging)
   generic
      with procedure On_Some (V : T);
   function Tap (O : Option) return Option;

   --  ==========================================================================
   --  Operator Aliases (Ada idioms for ergonomic syntax)
   --  ==========================================================================

   --  "or" for Unwrap_Or: Option or Default -> T
   --  Usage: Port : Integer := Port_Option or 8080;
   function "or" (O : Option; Default : T) return T renames Unwrap_Or;

   --  "or" for Or_Else: Option or Option -> Option
   --  Usage: Config := Primary_Config or Backup_Config;
   function "or" (Left, Right : Option) return Option renames Or_Else;

   --  "and": returns B if both have values, else None
   --  Usage: if (Config_A and Config_B).Has_Value then Process (Config_B);
   function "and" (A, B : Option) return Option;

   --  "xor": returns the value if exactly one has a value
   --  Usage: Choice := Option_A xor Option_B;
   function "xor" (A, B : Option) return Option;

   --  "=" for Contains: Option = T -> Boolean
   --  Usage: if Port_Option = 8080 then ...
   function "=" (O : Option; Value : T) return Boolean renames Contains;

   --  ==========================================================================
   --  Combining and Flattening
   --  ==========================================================================

   --  Zip_With: combine two Options with a function
   --  Returns Some(Combine(A.Value, B.Value)) if both have values, else None
   generic
      type U is private;
      type Option_U is private;
      with function Has_Value_U (O : Option_U) return Boolean;
      with function Value_U (O : Option_U) return U;
      with function Combine (A : T; B : U) return T;
   function Zip_With (A : Option; B : Option_U) return Option;

   --  Flatten: Option[Option[T]] -> Option[T]
   --  Returns inner option if outer has value, else None
   --  Note: Convert allows T to be accessed as Inner_Option (identity when T = Inner_Option)
   generic
      type Inner_Option is private;
      with function Convert (V : T) return Inner_Option;
      with function Has_Value_Inner (O : Inner_Option) return Boolean;
      with function None_Inner return Inner_Option;
   function Flatten (Outer : Option) return Inner_Option;

   --  ==========================================================================
   --  Conversion to Result
   --  ==========================================================================

   --  Ok_Or: convert to Result - Some(v) -> Ok(v), None -> Error(e)
   generic
      type Error_Type is private;
      type Result_Type is private;
      with function Make_Ok (V : T) return Result_Type;
      with function Make_Error (E : Error_Type) return Result_Type;
   function Ok_Or (O : Option; Error : Error_Type) return Result_Type;

   --  Ok_Or_Else: convert to Result with lazy error production
   --  Some(v) -> Ok(v), None -> Error(Produce_Error())
   generic
      type Error_Type is private;
      type Result_Type is private;
      with function Make_Ok (V : T) return Result_Type;
      with function Make_Error (E : Error_Type) return Result_Type;
      with function Produce_Error return Error_Type;
   function Ok_Or_Else (O : Option) return Result_Type;

end Functional.Option;
