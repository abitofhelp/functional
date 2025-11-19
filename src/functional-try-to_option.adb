pragma Ada_2022;
--  ===========================================================================
--  Functional.Try.To_Option - Implementation
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  ===========================================================================

package body Functional.Try.To_Option is

   function Run return T_Option.Option is
      --  Instantiate the core generic function
      function Try_Impl is new
        Try_To_Functional_Option
          (T          => T,
           Option_Pkg => T_Option,
           Action     => Action);
   begin
      return Try_Impl;
   end Run;

end Functional.Try.To_Option;
