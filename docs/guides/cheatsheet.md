# Functional Library Cheatsheet

**v3.0.0** | Result[T,E] • Option[T] • Either[L,R] • Try

---

## Result[T,E] — 25 Operations

```ada
package R is new Functional.Result (T => Integer, E => Error);
```

| Operation | Signature | Description |
|-----------|-----------|-------------|
| `Ok(v)` | `T → Result` | Success |
| `New_Error(e)` | `E → Result` | Failure |
| `Is_Ok(r)` | `Result → Boolean` | Test success |
| `Is_Error(r)` | `Result → Boolean` | Test failure |
| `Value(r)` | `Result → T` | Extract (Pre: Is_Ok) |
| `Error(r)` | `Result → E` | Extract (Pre: Is_Error) |
| `Expect(r,msg)` | `Result × String → T` | Extract or raise |
| `Unwrap_Or(r,d)` | `Result × T → T` | Value or default |
| `r or d` | `Result × T → T` | Operator alias |
| `a or b` | `Result × Result → Result` | Fallback operator |

**Transforms** (generic instantiation required):

| Operation | Type | Description |
|-----------|------|-------------|
| `Map` | `(T→T) → Result→Result` | Transform Ok |
| `And_Then` | `(T→Result) → Result→Result` | Chain (monadic bind) |
| `And_Then_Into` | `(T→Result_U) → Result→Result_U` | Chain + type change |
| `Map_Error` | `(E→E) → Result→Result` | Transform Error |
| `Bimap` | `(T→T,E→E) → Result→Result` | Transform both |
| `Fallback_With` | `(→Result) → Result→Result` | Lazy alternative |
| `Recover` | `(E→T) → Result→T` | Error to value |
| `Recover_With` | `(E→Result) → Result→Result` | Error to Result |
| `Ensure` | `(T→Bool,T→E) → Result→Result` | Validate predicate |
| `With_Context` | `(E×String→E) → Result×String→Result` | Add error context |
| `Tap` | `(T→,E→) → Result→Result` | Side effects |
| `Zip_With` | `(T×U→T) → Result×Result_U→Result` | Combine two |
| `Flatten` | `Result[Result[T,E],E] → Result[T,E]` | Unwrap nested |
| `To_Option` | `Result → Option` | Ok→Some, Error→None |

---

## Option[T] — 19 Operations

```ada
package O is new Functional.Option (T => Integer);
```

| Operation | Signature | Description |
|-----------|-----------|-------------|
| `New_Some(v)` | `T → Option` | Present |
| `None` | `→ Option` | Absent |
| `Is_Some(o)` | `Option → Boolean` | Test present |
| `Is_None(o)` | `Option → Boolean` | Test absent |
| `Value(o)` | `Option → T` | Extract (Pre: Has_Value) |
| `Unwrap_Or(o,d)` | `Option × T → T` | Value or default |
| `o or d` | `Option × T → T` | Operator alias |
| `a or b` | `Option × Option → Option` | Or_Else operator |
| `a and b` | `Option × Option → Option` | Both → second |
| `a xor b` | `Option × Option → Option` | Exactly one |

**Transforms** (generic instantiation required):

| Operation | Type | Description |
|-----------|------|-------------|
| `Map` | `(T→T) → Option→Option` | Transform Some |
| `And_Then` | `(T→Option) → Option→Option` | Chain (monadic bind) |
| `Filter` | `(T→Bool) → Option→Option` | Keep if predicate |
| `Or_Else_With` | `(→Option) → Option→Option` | Lazy alternative |
| `Unwrap_Or_With` | `(→T) → Option→T` | Lazy default |
| `Zip_With` | `(T×U→T) → Option×Option_U→Option` | Combine two |
| `Flatten` | `Option[Option[T]] → Option[T]` | Unwrap nested |
| `Ok_Or` | `Option × E → Result` | Some→Ok, None→Error |
| `Ok_Or_Else` | `(→E) → Option→Result` | Lazy error |

---

## Either[L,R] — 11 Operations

```ada
package E is new Functional.Either (L => String, R => Integer);
```

| Operation | Signature | Description |
|-----------|-----------|-------------|
| `Left(v)` | `L → Either` | Left value |
| `Right(v)` | `R → Either` | Right value |
| `Is_Left(e)` | `Either → Boolean` | Test left |
| `Is_Right(e)` | `Either → Boolean` | Test right |
| `Left_Value(e)` | `Either → L` | Extract (Pre: Is_Left) |
| `Right_Value(e)` | `Either → R` | Extract (Pre: Is_Right) |

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
