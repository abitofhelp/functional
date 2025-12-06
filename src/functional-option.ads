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
--    Option_Kind  - Discriminant: K_Some or K_None
--    Option       - Discriminated record holding Value when K_Some
--
--  Operations (11):
--    Constructors: New_Some, None
--    Predicates:   Is_Some, Is_None
--    Extractors:   Value
--    Defaults:     Unwrap_Or, Unwrap_Or_With
--    Transforms:   Map, And_Then, Filter
--    Fallback:     Or_Else, Or_Else_With, Fallback (alias)
--
--  ===========================================================================

generic
   type T is private;
package Functional.Option with
  Preelaborate
is

   type Option_Kind is (K_Some, K_None);

   type Option (Kind : Option_Kind := K_None) is record
      case Kind is
         when K_Some =>
            Value : T;

         when K_None =>
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

   --  ==========================================================================
   --  Extractors
   --  ==========================================================================

   function Value (O : Option) return T
   with Pre => O.Kind = K_Some, Inline;

   --  ==========================================================================
   --  Unwrap with defaults
   --  ==========================================================================

   function Unwrap_Or (O : Option; Default : T) return T
   with
     Post =>
       (if O.Kind = K_Some then Unwrap_Or'Result = O.Value
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
   function Map (O : Option) return Option;

   --  And_Then: chain optional operations (monadic bind)
   generic
      with function F (X : T) return Option;
   function And_Then (O : Option) return Option;

   --  Filter: keep value only if predicate holds
   generic
      with function Pred (X : T) return Boolean;
   function Filter (O : Option) return Option;

   --  ==========================================================================
   --  Fallback
   --  ==========================================================================

   --  Or_Else: fallback to alternative (eager evaluation)
   function Or_Else (A, B : Option) return Option;

   --  Or_Else_With: fallback to alternative (lazy evaluation)
   generic
      with function F return Option;
   function Or_Else_With (O : Option) return Option;

   --  Aliases for discoverability (Result-style naming)
   function Fallback (A, B : Option) return Option renames Or_Else;

end Functional.Option;
