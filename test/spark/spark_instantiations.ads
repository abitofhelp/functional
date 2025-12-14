pragma Ada_2022;
--  ===========================================================================
--  SPARK_Instantiations - Comprehensive SPARK Verification
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Comprehensive instantiations of ALL functional generics for SPARK proof.
--    SPARK only analyzes instantiated generics, not generic templates.
--    This file ensures every provable operation is formally verified.
--
--  Coverage:
--    - Result: 30+ operations
--    - Option: 25+ operations
--    - Either: 20 operations
--    - Version: 3 functions (separate unit)
--
--  ===========================================================================

with Functional.Option;
with Functional.Result;
with Functional.Either;

package SPARK_Instantiations with
  SPARK_Mode => On
is

   --  ========================================================================
   --  Helper Types
   --  ========================================================================

   --  Simple error type for testing
   type Test_Error is (Validation_Failed, Parse_Failed, Not_Found);

   --  Constrained string type for Either (unconstrained types not allowed)
   subtype Error_Message is String (1 .. 64);

   --  ========================================================================
   --  Package Instantiations
   --  ========================================================================

   package Int_Option is new Functional.Option (T => Integer);
   package Int_Result is new Functional.Result (T => Integer, E => Test_Error);
   package String_Or_Int is new Functional.Either (L => Error_Message, R => Integer);

   --  Secondary instantiations for cross-type operations
   package Bool_Option is new Functional.Option (T => Boolean);
   package Bool_Result is new Functional.Result (T => Boolean, E => Test_Error);

   --  ========================================================================
   --  Helper Functions for Generic Parameters
   --  ========================================================================

   --  Integer predicates
   function Is_Positive (X : Integer) return Boolean is (X > 0);
   function Is_Even (X : Integer) return Boolean is (X mod 2 = 0);
   function Is_In_Range (X : Integer) return Boolean is (X in 1 .. 100);

   --  Integer transformations (overflow-safe: no arithmetic that can overflow)
   function Double (X : Integer) return Integer is
     (if X > Integer'First then abs X else Integer'Last);  -- Safe absolute
   function Increment (X : Integer) return Integer is
     (if X < Integer'Last then X + 1 else X);  -- Safe increment
   function Negate (X : Integer) return Integer is
     (if X > Integer'First then -X else Integer'Last);  -- Safe negation
   function Square (X : Integer) return Integer is
     (if X in -100 .. 100 then X * X else X);  -- Bounded square

   --  Default producers
   function Default_Int return Integer is (0);
   function Default_True return Boolean is (True);

   --  Error predicates
   function Is_Validation (E : Test_Error) return Boolean is (E = Validation_Failed);
   function Is_Not_Found (E : Test_Error) return Boolean is (E = Not_Found);

   --  Error transformations
   function To_Not_Found (E : Test_Error) return Test_Error is (Not_Found);

   --  Value to error conversion
   function Int_To_Error (X : Integer) return Test_Error is
     (if X < 0 then Validation_Failed else Parse_Failed);

   --  Error context (simplified - returns same error, SPARK doesn't do strings well)
   function Append_Context (E : Test_Error; Msg : String) return Test_Error is (E);

   --  String predicates/transforms for Either
   function Is_Empty_Msg (S : Error_Message) return Boolean is (S (1) = ' ');
   function Msg_Identity (S : Error_Message) return Error_Message is (S);

   --  ========================================================================
   --  RESULT: Predicate Instantiations
   --  ========================================================================

   function Result_Is_Ok_And_Positive is new Int_Result.Is_Ok_And (Pred => Is_Positive);
   function Result_Is_Ok_And_Even is new Int_Result.Is_Ok_And (Pred => Is_Even);
   function Result_Is_Error_And_Validation is new Int_Result.Is_Error_And (Pred => Is_Validation);
   function Result_Is_Error_And_Not_Found is new Int_Result.Is_Error_And (Pred => Is_Not_Found);
   function Result_Is_Ok_Or_Positive is new Int_Result.Is_Ok_Or (Pred => Is_Positive);
   function Result_Is_Error_Or_Validation is new Int_Result.Is_Error_Or (Pred => Is_Validation);

   --  ========================================================================
   --  RESULT: Unwrap Instantiations
   --  ========================================================================

   function Result_Unwrap_Or_Default is new Int_Result.Unwrap_Or_With (F => Default_Int);

   --  ========================================================================
   --  RESULT: Map Instantiations
   --  ========================================================================

   function Result_Map_Double is new Int_Result.Map (F => Double);
   function Result_Map_Increment is new Int_Result.Map (F => Increment);
   function Result_Map_Negate is new Int_Result.Map (F => Negate);

   function Result_Map_Or_Double is new Int_Result.Map_Or (F => Double);
   function Result_Map_Or_Increment is new Int_Result.Map_Or (F => Increment);

   function Result_Map_Or_Else_Double is new Int_Result.Map_Or_Else
     (F => Double, Default => Default_Int);

   function Result_Map_Error_To_Not_Found is new Int_Result.Map_Error (F => To_Not_Found);

   function Result_Bimap is new Int_Result.Bimap
     (Map_Ok => Double, Map_Error => To_Not_Found);

   --  ========================================================================
   --  RESULT: Chain Instantiations (And_Then)
   --  ========================================================================

   --  Helper for And_Then: returns Ok if positive, else Error
   function Validate_Positive (X : Integer) return Int_Result.Result is
     (if X > 0 then Int_Result.Ok (X) else Int_Result.New_Error (Validation_Failed));

   function Validate_In_Range (X : Integer) return Int_Result.Result is
     (if X in 1 .. 100 then Int_Result.Ok (X) else Int_Result.New_Error (Validation_Failed));

   function Result_And_Then_Validate is new Int_Result.And_Then (F => Validate_Positive);
   function Result_And_Then_Range is new Int_Result.And_Then (F => Validate_In_Range);

   --  ========================================================================
   --  RESULT: And_Then_Into (type transformation)
   --  ========================================================================

   function Int_To_Bool (X : Integer) return Bool_Result.Result is
     (Bool_Result.Ok (X > 0));

   function Result_And_Then_To_Bool is new Int_Result.And_Then_Into
     (Result_U => Bool_Result.Result,
      Error_U  => Bool_Result.New_Error,
      F        => Int_To_Bool);

   --  ========================================================================
   --  RESULT: Fallback/Recovery Instantiations
   --  ========================================================================

   function Fallback_Producer return Int_Result.Result is (Int_Result.Ok (0));

   function Result_Fallback_With is new Int_Result.Fallback_With (F => Fallback_Producer);

   function Error_To_Default (E : Test_Error) return Integer is (0);

   function Result_Recover is new Int_Result.Recover (Handle => Error_To_Default);

   function Error_To_Result (E : Test_Error) return Int_Result.Result is
     (Int_Result.Ok (0));

   function Result_Recover_With is new Int_Result.Recover_With (Handle => Error_To_Result);

   --  ========================================================================
   --  RESULT: Validation Instantiations
   --  ========================================================================

   function Result_Ensure_Positive is new Int_Result.Ensure
     (Pred => Is_Positive, To_Error => Int_To_Error);

   function Result_Ensure_In_Range is new Int_Result.Ensure
     (Pred => Is_In_Range, To_Error => Int_To_Error);

   function Result_With_Context is new Int_Result.With_Context (Append => Append_Context);

   --  ========================================================================
   --  RESULT: Zip_With Instantiation
   --  ========================================================================

   --  Overflow-safe addition: use saturating semantics
   function Add_Ints (A, B : Integer) return Integer is
     (if B >= 0 then
        (if A <= Integer'Last - B then A + B else Integer'Last)
      else
        (if A >= Integer'First - B then A + B else Integer'First));

   function Result_Zip_Add is new Int_Result.Zip_With
     (U        => Integer,
      Result_U => Int_Result.Result,
      Is_Ok_U  => Int_Result.Is_Ok,
      Value_U  => Int_Result.Value,
      Error_U  => Int_Result.Error,
      Combine  => Add_Ints);

   --  ========================================================================
   --  RESULT: Flatten Instantiation
   --  ========================================================================
   --  Note: Flatten is for same-type unwrapping (Result[Result[T,E],E] where
   --  outer T = inner Result). The Value_Inner must return the outer's T type.
   --  For cross-type flattening, use And_Then instead.

   --  Nested_Result: Result[Int_Result.Result, Test_Error]
   package Nested_Result is new Functional.Result
     (T => Int_Result.Result, E => Test_Error);

   --  For Flatten, Convert/Value_Inner work on T = Int_Result.Result
   function Identity_Result (V : Int_Result.Result) return Int_Result.Result is (V);

   --  Note: This flattens Nested_Result -> Nested_Result (same type)
   --  Useful for composition patterns, not cross-type unwrapping
   function Result_Flatten is new Nested_Result.Flatten
     (Inner_Result  => Int_Result.Result,
      Convert       => Identity_Result,
      Is_Ok_Inner   => Int_Result.Is_Ok,
      Value_Inner   => Identity_Result,  -- Returns T (Int_Result.Result)
      Error_Inner   => Int_Result.Error);

   --  ========================================================================
   --  RESULT: To_Option Instantiation
   --  ========================================================================

   function Result_To_Option is new Int_Result.To_Option
     (Option_Type => Int_Option.Option,
      Make_Some   => Int_Option.New_Some,
      Make_None   => Int_Option.None);

   --  ========================================================================
   --  OPTION: Predicate Instantiations
   --  ========================================================================

   function Option_Is_Some_And_Positive is new Int_Option.Is_Some_And (Pred => Is_Positive);
   function Option_Is_Some_And_Even is new Int_Option.Is_Some_And (Pred => Is_Even);
   function Option_Is_None_Or_Positive is new Int_Option.Is_None_Or (Pred => Is_Positive);
   function Option_Is_None_Or_Even is new Int_Option.Is_None_Or (Pred => Is_Even);

   --  ========================================================================
   --  OPTION: Unwrap Instantiations
   --  ========================================================================

   function Option_Unwrap_Or_Default is new Int_Option.Unwrap_Or_With (F => Default_Int);

   --  ========================================================================
   --  OPTION: Map Instantiations
   --  ========================================================================

   function Option_Map_Double is new Int_Option.Map (F => Double);
   function Option_Map_Increment is new Int_Option.Map (F => Increment);
   function Option_Map_Negate is new Int_Option.Map (F => Negate);

   function Option_Map_Or_Double is new Int_Option.Map_Or (F => Double);
   function Option_Map_Or_Increment is new Int_Option.Map_Or (F => Increment);

   function Option_Map_Or_Else_Double is new Int_Option.Map_Or_Else
     (F => Double, Default => Default_Int);

   --  ========================================================================
   --  OPTION: Chain Instantiations (And_Then)
   --  ========================================================================

   function Optional_If_Positive (X : Integer) return Int_Option.Option is
     (if X > 0 then Int_Option.New_Some (X) else Int_Option.None);

   function Optional_If_Even (X : Integer) return Int_Option.Option is
     (if X mod 2 = 0 then Int_Option.New_Some (X) else Int_Option.None);

   function Option_And_Then_Positive is new Int_Option.And_Then (F => Optional_If_Positive);
   function Option_And_Then_Even is new Int_Option.And_Then (F => Optional_If_Even);

   --  ========================================================================
   --  OPTION: Filter Instantiations
   --  ========================================================================

   function Option_Filter_Positive is new Int_Option.Filter (Pred => Is_Positive);
   function Option_Filter_Even is new Int_Option.Filter (Pred => Is_Even);
   function Option_Filter_In_Range is new Int_Option.Filter (Pred => Is_In_Range);

   --  ========================================================================
   --  OPTION: Fallback Instantiations
   --  ========================================================================

   function Fallback_Option return Int_Option.Option is (Int_Option.New_Some (0));

   function Option_Or_Else_With is new Int_Option.Or_Else_With (F => Fallback_Option);

   --  ========================================================================
   --  OPTION: Zip_With Instantiation
   --  ========================================================================

   function Option_Zip_Add is new Int_Option.Zip_With
     (U           => Integer,
      Option_U    => Int_Option.Option,
      Has_Value_U => Int_Option.Is_Some,
      Value_U     => Int_Option.Value,
      Combine     => Add_Ints);

   --  ========================================================================
   --  OPTION: Flatten Instantiation
   --  ========================================================================

   package Nested_Option is new Functional.Option (T => Int_Option.Option);

   function Identity_Option (V : Int_Option.Option) return Int_Option.Option is (V);

   function Option_Flatten is new Nested_Option.Flatten
     (Inner_Option    => Int_Option.Option,
      Convert         => Identity_Option,
      Has_Value_Inner => Int_Option.Is_Some,
      None_Inner      => Int_Option.None);

   --  ========================================================================
   --  OPTION: Ok_Or Instantiations (to Result)
   --  ========================================================================

   function Option_Ok_Or is new Int_Option.Ok_Or
     (Error_Type  => Test_Error,
      Result_Type => Int_Result.Result,
      Make_Ok     => Int_Result.Ok,
      Make_Error  => Int_Result.New_Error);

   function Produce_Not_Found return Test_Error is (Not_Found);

   function Option_Ok_Or_Else is new Int_Option.Ok_Or_Else
     (Error_Type    => Test_Error,
      Result_Type   => Int_Result.Result,
      Make_Ok       => Int_Result.Ok,
      Make_Error    => Int_Result.New_Error,
      Produce_Error => Produce_Not_Found);

   --  ========================================================================
   --  EITHER: Predicate Instantiations
   --  ========================================================================

   function Either_Is_Left_And_Empty is new String_Or_Int.Is_Left_And (Pred => Is_Empty_Msg);
   function Either_Is_Right_And_Positive is new String_Or_Int.Is_Right_And (Pred => Is_Positive);
   function Either_Is_Left_Or_Empty is new String_Or_Int.Is_Left_Or (Pred => Is_Empty_Msg);
   function Either_Is_Right_Or_Positive is new String_Or_Int.Is_Right_Or (Pred => Is_Positive);

   --  ========================================================================
   --  EITHER: Map Instantiations
   --  ========================================================================

   function Either_Map_Left is new String_Or_Int.Map_Left (F => Msg_Identity);
   function Either_Map_Right_Double is new String_Or_Int.Map_Right (F => Double);
   function Either_Map_Right_Increment is new String_Or_Int.Map_Right (F => Increment);
   function Either_Map_Double is new String_Or_Int.Map (F => Double);

   function Either_Bimap is new String_Or_Int.Bimap
     (Map_L => Msg_Identity, Map_R => Double);

   --  ========================================================================
   --  EITHER: And_Then Instantiation
   --  ========================================================================

   Blank_Message : constant Error_Message := (others => ' ');

   function Either_If_Positive (X : Integer) return String_Or_Int.Either is
     (if X > 0 then String_Or_Int.Right (X) else String_Or_Int.Left (Blank_Message));

   function Either_And_Then_Positive is new String_Or_Int.And_Then (F => Either_If_Positive);

   --  ========================================================================
   --  EITHER: Fold Instantiation
   --  ========================================================================

   function Msg_Length (S : Error_Message) return Integer is (64);
   function Int_Identity (X : Integer) return Integer is (X);

   function Either_Fold_To_Int is new String_Or_Int.Fold
     (U        => Integer,
      On_Left  => Msg_Length,
      On_Right => Int_Identity);

   --  ========================================================================
   --  EITHER: Merge Instantiation (when L = R)
   --  ========================================================================

   package Int_Either is new Functional.Either (L => Integer, R => Integer);

   function Int_From_Left (X : Integer) return Integer is (X);
   function Int_From_Right (X : Integer) return Integer is (X);

   function Int_Either_Merge is new Int_Either.Merge
     (T          => Integer,
      From_Left  => Int_From_Left,
      From_Right => Int_From_Right);

   --  ========================================================================
   --  EITHER: To_Option Instantiation
   --  ========================================================================

   function Either_To_Option is new String_Or_Int.To_Option
     (Option_Type => Int_Option.Option,
      Make_Some   => Int_Option.New_Some,
      Make_None   => Int_Option.None);

   --  ========================================================================
   --  EITHER: To_Result Instantiation
   --  ========================================================================

   --  Need a Result with Error_Message as error type
   package Msg_Result is new Functional.Result (T => Integer, E => Error_Message);

   function Either_To_Result is new String_Or_Int.To_Result
     (Result_Type => Msg_Result.Result,
      Make_Ok     => Msg_Result.Ok,
      Make_Error  => Msg_Result.New_Error);

   --  ========================================================================
   --  EITHER: Swap Instantiation
   --  ========================================================================

   package Int_Or_String is new Functional.Either (L => Integer, R => Error_Message);

   function Either_Swap is new String_Or_Int.Swap
     (Either_Swapped => Int_Or_String.Either,
      Make_Left      => Int_Or_String.Left,
      Make_Right     => Int_Or_String.Right);

   --  ========================================================================
   --  Test Functions (exercise contracts)
   --  ========================================================================

   --  Option tests
   function Test_Option_Some return Int_Option.Option
     with Post => Int_Option.Is_Some (Test_Option_Some'Result);

   function Test_Option_None return Int_Option.Option
     with Post => Int_Option.Is_None (Test_Option_None'Result);

   --  Result tests
   function Test_Result_Ok return Int_Result.Result
     with Post => Int_Result.Is_Ok (Test_Result_Ok'Result);

   function Test_Result_Error return Int_Result.Result
     with Post => not Int_Result.Is_Ok (Test_Result_Error'Result);

   --  Either tests
   function Test_Either_Left return String_Or_Int.Either
     with Post => String_Or_Int.Is_Left (Test_Either_Left'Result);

   function Test_Either_Right return String_Or_Int.Either
     with Post => not String_Or_Int.Is_Left (Test_Either_Right'Result);

   --  ========================================================================
   --  Comprehensive Test Functions (exercise all instantiated operations)
   --  ========================================================================

   --  Result comprehensive test
   function Test_Result_Operations return Boolean
     with Post => Test_Result_Operations'Result;

   --  Option comprehensive test
   function Test_Option_Operations return Boolean
     with Post => Test_Option_Operations'Result;

   --  Either comprehensive test
   function Test_Either_Operations return Boolean
     with Post => Test_Either_Operations'Result;

end SPARK_Instantiations;
