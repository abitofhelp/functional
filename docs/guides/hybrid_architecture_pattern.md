# Hybrid Architecture Pattern - Using Functional Library

**Version:** 2.0.0
**Date:** November 13, 2025
**SPDX-License-Identifier:** BSD-3-Clause
**License File:** See the LICENSE file in the project root.
**Copyright:** © 2025 Michael Gardner, A Bit of Help, Inc.
**Status:** Released

This example demonstrates how to integrate the `functional` library into a clean/hexagonal architecture using the Dependency Inversion Principle.

## Problem

You want to use `Functional.Result` in your application, but:
- ❌ Domain layer should have **zero external dependencies**
- ❌ Coupling Domain directly to `functional` library makes it hard to swap implementations
- ❌ Domain code should be **maximally reusable** across projects

## Solution: Interface in Domain, Implementation in Infrastructure

### Step 1: Domain Defines What It Needs (Interface-Only)

```ada
--  domain/src/model/hybrid-domain-model-result.ads
pragma Ada_2022;
package Hybrid.Domain.Model.Result with Pure is

   --  Domain defines ONLY the operations it needs
   generic
      type T is private;
      type E is private;
   package Instance is

      type Result is private;

      --  Constructors
      function Ok (V : T) return Result;
      function Err (E_Val : E) return Result;

      --  Predicates
      function Is_Ok (R : Result) return Boolean;
      function Is_Err (R : Result) return Boolean;

      --  Extractors
      function Value (R : Result) return T with Pre => Is_Ok (R);
      function Error (R : Result) return E with Pre => Is_Err (R);

      --  Core operations
      function Unwrap_Or (R : Result; Default : T) return T;

      generic
         with function F (X : T) return Result;
      function And_Then (R : Result) return Result;

      --  Add more as needed...

   private
      --  Implementation details hidden - could be anything!
      type Result_Kind is (K_Ok, K_Err);
      type Result (Kind : Result_Kind := K_Ok) is record
         case Kind is
            when K_Ok  => Ok_Value  : T;
            when K_Err => Err_Value : E;
         end case;
      end record;

   end Instance;

end Hybrid.Domain.Model.Result;
```

### Step 2: Infrastructure Provides How (Using Functional Library)

```ada
--  infrastructure/src/adapter/functional/
--  hybrid-domain-model-result.adb
pragma Ada_2022;
with Functional.Result;

package body Hybrid.Domain.Model.Result is

   package body Instance is

      --  Use Functional.Result as the backing implementation
      package FR is new Functional.Result (T => T, E => E);

      --  Delegate to Functional.Result
      function Ok (V : T) return Result is
      begin
         return Result (FR.Ok (V));
      end Ok;

      function Err (E_Val : E) return Result is
      begin
         return Result (FR.Err (E_Val));
      end Err;

      function Is_Ok (R : Result) return Boolean is
      begin
         return FR.Is_Ok (FR.Result (R));
      end Is_Ok;

      function Is_Err (R : Result) return Boolean is
      begin
         return FR.Is_Err (FR.Result (R));
      end Is_Err;

      function Value (R : Result) return T is
      begin
         return FR.Value (FR.Result (R));
      end Value;

      function Error (R : Result) return E is
      begin
         return FR.Error (FR.Result (R));
      end Error;

      function Unwrap_Or (R : Result; Default : T) return T is
      begin
         return FR.Unwrap_Or (FR.Result (R), Default);
      end Unwrap_Or;

      function And_Then (R : Result) return Result is
         --  Wrapper to adapt F's signature
         function F_Wrapper (X : T) return FR.Result is
         begin
            return FR.Result (F (X));
         end F_Wrapper;

         function Chain is new FR.And_Then (F => F_Wrapper);
      begin
         return Result (Chain (FR.Result (R)));
      end And_Then;

   end Instance;

end Hybrid.Domain.Model.Result;
```

### Step 3: Domain Code Uses Its Own Interface

```ada
--  domain/src/value/hybrid-domain-value-person_name.ads
with Hybrid.Domain.Model.Result;  --  Domain's interface, not Functional!

package Hybrid.Domain.Value.Person_Name is

   type Person_Name_Error_Kind is (Empty_Name, Name_Too_Long);
   type Person_Name_Error is record
      Kind : Person_Name_Error_Kind;
   end record;

   type Person_Name is private;

   --  Use Domain's Result interface
   package Person_Name_Result is new Hybrid.Domain.Model.Result.Instance
     (T => Person_Name, E => Person_Name_Error);

   subtype Result is Person_Name_Result.Result;

   function Create (Name : String) return Result;

private
   type Person_Name is record
      Value : String (1 .. 255);
      Len   : Natural;
   end record;
end Hybrid.Domain.Value.Person_Name;
```

```ada
--  domain/src/value/hybrid-domain-value-person_name.adb
package body Hybrid.Domain.Value.Person_Name is

   function Create (Name : String) return Result is
   begin
      if Name'Length = 0 then
         return Person_Name_Result.Err ((Kind => Empty_Name));
      end if;

      if Name'Length > 255 then
         return Person_Name_Result.Err ((Kind => Name_Too_Long));
      end if;

      return Person_Name_Result.Ok ((Value => Name, Len => Name'Length));
   end Create;

end Hybrid.Domain.Value.Person_Name;
```

### Step 4: GPRbuild Configuration

```gpr
--  domain.gpr
with "config/functional_config.gpr";  --  References functional library

library project Domain is
   for Library_Name use "domain";
   for Source_Dirs use ("src/model/", "src/value/", "src/service/");
   for Object_Dir use "obj";

   package Compiler renames Functional_Config.Compiler;
end Domain;
```

```toml
# domain/alire.toml
name = "domain"

[[depends-on]]
functional = "^0.1.0"  # Infrastructure adapts this
```

## Benefits

### ✅ Zero Coupling
- Domain defines its own `Result` interface
- Domain code has **zero knowledge** of `functional` library
- Can swap to different implementation anytime

### ✅ Maximum Reusability
- Domain layer portable across projects
- Same Domain code works with:
  - `functional` library (this implementation)
  - Hand-rolled Result (embedded systems)
  - SPARK-proven Result (high-assurance)

### ✅ Change Isolation
- Want to upgrade `functional` library with breaking changes?
  - Change **1 file** in Infrastructure (the adapter)
  - Domain code **untouched**

### ✅ Testability
- Mock Result implementation for testing
- No need to depend on real functional library in tests

## When to Use This Pattern

✅ **Use interface + adapter when:**
- Building reusable Domain layer
- Planning to support multiple contexts (e.g., native + embedded)
- External library might change or be swapped
- Teaching clean architecture principles

❌ **Direct dependency is fine when:**
- Small utility project
- Not building reusable components
- `functional` is "the" standard (like Ada.Containers)

## Alternative: Thin Wrapper

For simpler cases, use a thin type wrapper instead of full delegation:

```ada
--  domain/src/model/hybrid-domain-model-result.ads
with Functional.Result;

package Hybrid.Domain.Model.Result is
   generic
      type T is private;
      type E is private;
   package Instance is
      --  Wrap Functional.Result in a new type
      package FR is new Functional.Result (T => T, E => E);

      type Result is new FR.Result;  --  Derived type inherits all operations

      --  Or explicitly re-export just what you need:
      function Ok (V : T) return Result renames FR.Ok;
      function Err (E : E) return Result renames FR.Err;
      --  ... etc
   end Instance;
end Hybrid.Domain.Model.Result;
```

This still provides an abstraction layer but with less code.

## Summary

| Aspect | Direct Use | Thin Wrapper | Full Adapter |
|--------|-----------|--------------|--------------|
| **Coupling** | High | Medium | Low |
| **Flexibility** | Low | Medium | High |
| **Code** | Minimal | Light | More |
| **Best for** | Utilities | Apps | Reusable domains |

Choose the level of indirection that matches your architectural needs!
