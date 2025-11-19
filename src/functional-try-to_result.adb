pragma Ada_2022;
--  ===========================================================================
--  Functional.Try.To_Result - Implementation
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  ===========================================================================

package body Functional.Try.To_Result is

   function Run return T_Result.Result is
      --  Instantiate the core generic function
      function Try_Impl is new
        Try_To_Functional_Result
          (T             => T,
           E             => E,
           Result_Pkg    => T_Result,
           Map_Exception => Map_Exception,
           Action        => Action);
   begin
      return Try_Impl;
   end Run;

end Functional.Try.To_Result;
