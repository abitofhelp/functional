# Functional Library Cheatsheet

**Project:** Functional - Type-Safe Error Handling Library for Ada 2022
**Version:** 3.0.0  
**Date:** December 06, 2025  
**Author:** Michael Gardner, A Bit of Help, Inc.
**Status:** Released  

---

## Result[T,E] — 36 Operations

```ada
package R is new Functional.Result (T => Integer, E => Error);
```

| Operation | Signature | Description |
|-----------|-----------|-------------|
| `Ok(v)` | `T → Result` | Success |
| `New_Error(e)` | `E → Result` | Failure |
| `Is_Ok(r)` | `Result → Boolean` | Test success |
| `Is_Error(r)` | `Result → Boolean` | Test failure |
| `Is_Ok_And(r)` | `(T→Bool) → Result → Boolean` | Ok and predicate (strict) |
| `Is_Error_And(r)` | `(E→Bool) → Result → Boolean` | Error and predicate (strict) |
| `Is_Ok_Or(r)` | `(T→Bool) → Result → Boolean` | Error or predicate holds (lenient) |
| `Is_Error_Or(r)` | `(E→Bool) → Result → Boolean` | Ok or predicate holds (lenient) |
| `Contains(r,v)` | `Result × T → Boolean` | Ok equals value |
| `r = v` | `Result × T → Boolean` | Operator alias |
| `Value(r)` | `Result → T` | Extract (Pre: Is_Ok) |
| `Error(r)` | `Result → E` | Extract (Pre: Is_Error) |
| `Expect(r,msg)` | `Result × String → T` | Extract or raise |
| `Expect_Error(r,msg)` | `Result × String → E` | Extract error or raise |
| `Unwrap_Error(r)` | `Result → E` | Extract error (Pre: Is_Error) |
| `Unwrap_Or(r,d)` | `Result × T → T` | Value or default |
| `r or d` | `Result × T → T` | Operator alias |
| `a or b` | `Result × Result → Result` | Fallback operator |

**Transforms** (generic instantiation required):

| Operation | Type | Description |
|-----------|------|-------------|
| `Map` | `(T→T) → Result→Result` | Transform Ok |
| `Map_Or` | `(T→T) → Result×T→T` | Transform Ok or default |
| `Map_Or_Else` | `(T→T,→T) → Result→T` | Transform Ok or lazy default |
| `And_Then` | `(T→Result) → Result→Result` | Chain (monadic bind) |
| `And_Then_Into` | `(T→Result_U) → Result→Result_U` | Chain + type change |
| `Map_Error` | `(E→E) → Result→Result` | Transform Error |
| `Bimap` | `(T→T,E→E) → Result→Result` | Transform both |
| `Fallback_With` | `(→Result) → Result→Result` | Lazy alternative |
| `Recover` | `(E→T) → Result→T` | Error to value |
| `Recover_With` | `(E→Result) → Result→Result` | Error to Result |
| `Ensure` | `(T→Bool,T→E) → Result→Result` | Validate predicate |
| `With_Context` | `(E×String→E) → Result×String→Result` | Add error context |
| `Tap` | `(T→,E→) → Result→Result` | Side effects (both) |
| `Tap_Ok` | `(T→) → Result→Result` | Side effect on Ok |
| `Tap_Error` | `(E→) → Result→Result` | Side effect on Error |
| `Zip_With` | `(T×U→T) → Result×Result_U→Result` | Combine two |
| `Flatten` | `Result[Result[T,E],E] → Result[T,E]` | Unwrap nested |
| `To_Option` | `Result → Option` | Ok→Some, Error→None |

---

## Option[T] — 26 Operations

```ada
package O is new Functional.Option (T => Integer);
```

| Operation | Signature | Description |
|-----------|-----------|-------------|
| `New_Some(v)` | `T → Option` | Present |
| `None` | `→ Option` | Absent |
| `Is_Some(o)` | `Option → Boolean` | Test present |
| `Is_None(o)` | `Option → Boolean` | Test absent |
| `Is_Some_And(o)` | `(T→Bool) → Option → Boolean` | Some and predicate (strict) |
| `Is_None_Or(o)` | `(T→Bool) → Option → Boolean` | None or predicate holds (lenient) |
| `Contains(o,v)` | `Option × T → Boolean` | Some equals value |
| `o = v` | `Option × T → Boolean` | Operator alias |
| `Value(o)` | `Option → T` | Extract (Pre: Has_Value) |
| `Expect(o,msg)` | `Option × String → T` | Extract or raise |
| `Unwrap_Or(o,d)` | `Option × T → T` | Value or default |
| `o or d` | `Option × T → T` | Operator alias |
| `a or b` | `Option × Option → Option` | Or_Else operator |
| `a and b` | `Option × Option → Option` | Both → second |
| `a xor b` | `Option × Option → Option` | Exactly one |

**Transforms** (generic instantiation required):

| Operation | Type | Description |
|-----------|------|-------------|
| `Map` | `(T→T) → Option→Option` | Transform Some |
| `Map_Or` | `(T→T) → Option×T→T` | Transform Some or default |
| `Map_Or_Else` | `(T→T,→T) → Option→T` | Transform Some or lazy default |
| `And_Then` | `(T→Option) → Option→Option` | Chain (monadic bind) |
| `Filter` | `(T→Bool) → Option→Option` | Keep if predicate |
| `Or_Else_With` | `(→Option) → Option→Option` | Lazy alternative |
| `Unwrap_Or_With` | `(→T) → Option→T` | Lazy default |
| `Tap` | `(T→) → Option→Option` | Side effect on Some |
| `Zip_With` | `(T×U→T) → Option×Option_U→Option` | Combine two |
| `Flatten` | `Option[Option[T]] → Option[T]` | Unwrap nested |
| `Ok_Or` | `Option × E → Result` | Some→Ok, None→Error |
| `Ok_Or_Else` | `(→E) → Option→Result` | Lazy error |

---

## Either[L,R] — 20 Operations

```ada
package E is new Functional.Either (L => String, R => Integer);
```

| Operation | Signature | Description |
|-----------|-----------|-------------|
| `Left(v)` | `L → Either` | Left value |
| `Right(v)` | `R → Either` | Right value |
| `Is_Left(e)` | `Either → Boolean` | Test left |
| `Is_Right(e)` | `Either → Boolean` | Test right |
| `Is_Left_And(e)` | `(L→Bool) → Either → Boolean` | Left and predicate (strict) |
| `Is_Right_And(e)` | `(R→Bool) → Either → Boolean` | Right and predicate (strict) |
| `Is_Left_Or(e)` | `(L→Bool) → Either → Boolean` | Right or predicate holds (lenient) |
| `Is_Right_Or(e)` | `(R→Bool) → Either → Boolean` | Left or predicate holds (lenient) |
| `Contains(e,v)` | `Either × R → Boolean` | Right equals value |
| `e = v` | `Either × R → Boolean` | Operator alias |
| `Left_Value(e)` | `Either → L` | Extract (Pre: Is_Left) |
| `Right_Value(e)` | `Either → R` | Extract (Pre: Is_Right) |
| `Get_Or_Else(e,d)` | `Either × R → R` | Right or default |

**Transforms** (generic instantiation required):

| Operation | Type | Description |
|-----------|------|-------------|
| `Map` | `(R→R) → Either→Either` | Right-biased transform |
| `Map_Left` | `(L→L) → Either→Either` | Transform Left |
| `Map_Right` | `(R→R) → Either→Either` | Transform Right |
| `Bimap` | `(L→L,R→R) → Either→Either` | Transform both |
| `And_Then` | `(R→Either) → Either→Either` | Right-biased chain |
| `Swap` | `Either[L,R] → Either[R,L]` | Exchange sides |
| `Fold` | `(L→U,R→U) → Either→U` | Reduce to single |
| `Merge` | `(L→T,R→T) → Either→T` | Extract when L=R |
| `To_Option` | `Either → Option` | Right→Some, Left→None |
| `To_Result` | `Either → Result` | Right→Ok, Left→Error |

---

## Try — 5 Functions (Exception Boundary)

```ada
with Functional.Try;
```

| Function | Use Case |
|----------|----------|
| `Try_To_Result` | General Result bridge |
| `Try_To_Functional_Result` | Convenience for Functional.Result |
| `Try_To_Functional_Option` | Convenience for Functional.Option |
| `Try_To_Result_With_Param` | Parameterized Result bridge |
| `Try_To_Option_With_Param` | Parameterized Option bridge |

```ada
function Safe_Parse is new Functional.Try.Try_To_Functional_Result
  (T => Integer, E => Error, Result_Pkg => Int_Result,
   Map_Exception => To_Error, Action => Parse);
```

---

## Quick Patterns

```ada
-- Railway chaining
Result := Chain (Validate (Transform (Parse (Input))));

-- Operator fallback
Config := Load_Primary or Load_Backup;
Port := Port_Result or 8080;

-- Contains check
if Port_Result = 8080 then Handle_Default;

-- Option to Result
R := To_Result (Find_User (ID), User_Not_Found);

-- Result to Option
O := To_Opt (Parse_Int (S));  -- Drops error info
```

---

## Discriminant Access (v3.0.0)

```ada
if R.Is_Ok then ...        -- Result
if O.Has_Value then ...    -- Option
if E.Is_Left then ...      -- Either
```

---

**SPARK**: Option, Result, Either are `SPARK_Mode => On`. Try is `SPARK_Mode => Off`.

**Total Operations**: 87 (Result: 36, Option: 26, Either: 20, Try: 5)
