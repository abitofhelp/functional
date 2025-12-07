pragma Ada_2022;
--  ===========================================================================
--  SPARK_Instantiations
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Test instantiations of functional generics for SPARK verification.
--    SPARK only analyzes instantiated generics, not generic templates.
--
--  ===========================================================================

with Functional.Option;
with Functional.Result;
with Functional.Either;

package SPARK_Instantiations with
  SPARK_Mode => On
is

   --  ========================================================================
   --  Option Instantiation
   --  ========================================================================

   package Int_Option is new Functional.Option (T => Integer);

   --  ========================================================================
   --  Result Instantiation
   --  ========================================================================

   --  Simple error type for testing
   type Test_Error is (Validation_Failed, Parse_Failed, Not_Found);

   package Int_Result is new Functional.Result (T => Integer, E => Test_Error);

   --  ========================================================================
   --  Either Instantiation
   --  ========================================================================

   --  Constrained string type for Either (unconstrained types not allowed)
   subtype Error_Message is String (1 .. 64);

   package String_Or_Int is new Functional.Either
     (L => Error_Message, R => Integer);

   --  ========================================================================
   --  Test Functions (to exercise contracts)
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

end SPARK_Instantiations;
