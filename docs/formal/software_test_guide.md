# Software Test Guide (STG)

**Project:** Functional - Type-Safe Error Handling Library for Ada 2022
**Version:** 4.0.0  
**Date:** December 12, 2025  
**Author:** Michael Gardner, A Bit of Help, Inc.
**Status:** Released  

---

## 1. Introduction

### 1.1 Purpose

This Software Test Guide (STG) describes the testing strategy, organization, and execution procedures for the Functional library. It provides guidance for running existing tests and writing new tests.

### 1.2 Scope

This document covers:
- Test organization and directory structure
- Test execution commands
- Writing new unit tests
- Coverage analysis procedures
- Test naming conventions

## 2. Test Strategy

### 2.1 Testing Levels

| Level | Description | Location |
|-------|-------------|----------|
| **Unit Tests** | Test individual package operations in isolation | `test/unit/` |
| **Coverage Analysis** | Statement + decision coverage via GNATcoverage | `coverage/` |

### 2.2 Testing Approach

- **Exhaustive Operation Testing**: Every public operation tested
- **Both Paths**: Ok/Error, Some/None, Left/Right branches exercised
- **Contract Verification**: Pre/Post conditions validated
- **Edge Cases**: Empty values, error conditions, boundary inputs
- **Lenient/Strict Predicates**: Both predicate types tested for correct semantics

## 3. Test Organization

### 3.1 Directory Structure

```
test/
├── alire.toml              # Test crate manifest (v3.0.0)
├── bin/                    # Compiled test executables
│   └── unit_runner
├── common/                 # Shared test infrastructure
│   ├── test_framework.ads  # Test framework spec
│   └── test_framework.adb  # Test framework body
├── config/                 # Alire-generated config
│   └── functional_tests_config.ads
└── unit/                   # Unit test sources
    ├── unit_tests.gpr      # Unit test GPR project
    ├── unit_runner.adb     # Main test runner
    ├── test_result.adb     # Result package tests (84)
    ├── test_option.adb     # Option package tests (65)
    ├── test_either.adb     # Either package tests (58)
    ├── test_try.adb        # Try package tests (14)
    └── test_try_option.adb # Try Option bridge tests (6)
```

### 3.2 Test Naming Convention

| Pattern | Description |
|---------|-------------|
| `test_<package>.adb` | Tests for `Functional.<Package>` |
| `Test_<Operation>` | Procedure testing specific operation |
| `unit_runner.adb` | Main entry point that runs all tests |

## 4. Running Tests

### 4.1 Quick Start

```bash
# Build and run all tests
make test-all

# Run only unit tests
make test-unit

# Run with coverage analysis
make test-coverage
```

### 4.2 Manual Execution

```bash
# Build test crate
cd test && alr build

# Run unit tests directly
./test/bin/unit_runner
```

### 4.3 Expected Output

```
======================================================================
Functional Library - Unit Test Suite
======================================================================

======================================
  Test_Result - Result Operations
======================================
[PASS] Test_Ok_Constructor
[PASS] Test_New_Error_Constructor
[PASS] Test_Is_Ok_Predicate
...

======================================
  Test_Option - Option Operations
======================================
[PASS] Test_New_Some_Constructor
[PASS] Test_None_Constructor
...

======================================
  Test_Either - Either Operations
======================================
[PASS] Test_Left_Constructor
[PASS] Test_Right_Constructor
...

========================================
        GRAND TOTAL - ALL UNIT TESTS
========================================
Total tests:   227
Passed:        227
Failed:        0

########################################
###
###    UNIT TESTS: SUCCESS
###    All  227 tests passed!
###
########################################
```

## 5. Writing Tests

### 5.1 Test File Structure

```ada
pragma Ada_2022;
--  ======================================================================
--  Test_Package_Name
--  ======================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  Purpose:
--    Tests for Functional.Package operations.
--    Target: 90%+ coverage
--  ======================================================================

with Ada.Text_IO;    use Ada.Text_IO;
with Ada.Command_Line;
with Functional.Package;
with Test_Framework;

procedure Test_Package_Name is

   --  Package instantiation
   package My_Package is new Functional.Package (...);

   --  Test counters
   Test_Count : Natural := 0;
   Pass_Count : Natural := 0;
   Fail_Count : Natural := 0;

   --  Local assert helper
   procedure Assert (Condition : Boolean; Message : String) is
   begin
      Test_Count := Test_Count + 1;
      if Condition then
         Pass_Count := Pass_Count + 1;
         Put_Line ("[PASS] " & Message);
      else
         Fail_Count := Fail_Count + 1;
         Put_Line ("[FAIL] " & Message);
      end if;
   end Assert;

   --  ==========================================================================
   --  Test: Operation_Name
   --  ==========================================================================

   procedure Test_Operation_Name is
      --  Setup
      Input : My_Package.Type := ...;
      Expected : ... := ...;
      Actual : ...;
   begin
      Put_Line ("Testing Operation_Name...");

      --  Execute
      Actual := My_Package.Operation (Input);

      --  Verify
      Assert (Actual = Expected, "Operation_Name produces expected result");
   end Test_Operation_Name;

begin
   Put_Line ("======================================================================");
   Put_Line ("Package_Name Tests");
   Put_Line ("======================================================================");
   New_Line;

   --  Run all tests
   Test_Operation_Name;
   --  ... more tests ...

   --  Report results
   New_Line;
   Test_Framework.Register_Results (Pass_Count, Fail_Count);
end Test_Package_Name;
```

### 5.2 Test Categories

Each test file should cover:

| Category | Example Tests |
|----------|---------------|
| **Constructors** | `Test_Ok`, `Test_New_Error`, `Test_New_Some`, `Test_None` |
| **Predicates** | `Test_Is_Ok`, `Test_Is_Error`, `Test_Is_Some`, `Test_Is_None` |
| **Lenient Predicates** | `Test_Is_Ok_Or`, `Test_Is_None_Or`, `Test_Is_Left_Or` |
| **Strict Predicates** | `Test_Is_Ok_And`, `Test_Is_Some_And`, `Test_Is_Right_And` |
| **Extractors** | `Test_Value_Ok`, `Test_Value_Error`, `Test_Error` |
| **Transforms** | `Test_Map_Ok`, `Test_Map_Error`, `Test_And_Then_Ok`, `Test_And_Then_Error` |
| **Recovery** | `Test_Fallback`, `Test_Recover`, `Test_Unwrap_Or` |
| **Operators** | `Test_Or_Operator`, `Test_Equals_Operator`, `Test_And_Operator` |

### 5.3 Testing Both Paths

Always test both discriminant branches:

```ada
--  Test Map with Ok input
procedure Test_Map_Ok is
   R : Int_Result.Result := Int_Result.Ok (5);
   function Double is new Int_Result.Map (F => Times_Two);
begin
   Assert (Int_Result.Is_Ok (Double (R)), "Map Ok returns Ok");
   Assert (Int_Result.Value (Double (R)) = 10, "Map transforms value");
end Test_Map_Ok;

--  Test Map with Error input
procedure Test_Map_Error is
   R : Int_Result.Result := Int_Result.New_Error (Error_Value);
   function Double is new Int_Result.Map (F => Times_Two);
begin
   Assert (Int_Result.Is_Error (Double (R)), "Map Error returns Error unchanged");
end Test_Map_Error;
```

### 5.4 Testing Lenient vs Strict Predicates

```ada
--  Strict: Is_Some_And - must be Some AND predicate holds
procedure Test_Is_Some_And is
   Some_Pos : Int_Option.Option := Int_Option.New_Some (5);
   Some_Neg : Int_Option.Option := Int_Option.New_Some (-5);
   None_Val : Int_Option.Option := Int_Option.None;

   function Check is new Int_Option.Is_Some_And (P => Is_Positive);
begin
   Assert (Check (Some_Pos), "Some(5) and positive = True");
   Assert (not Check (Some_Neg), "Some(-5) and positive = False");
   Assert (not Check (None_Val), "None and any = False");
end Test_Is_Some_And;

--  Lenient: Is_None_Or - None OR predicate holds
procedure Test_Is_None_Or is
   Some_Pos : Int_Option.Option := Int_Option.New_Some (5);
   Some_Neg : Int_Option.Option := Int_Option.New_Some (-5);
   None_Val : Int_Option.Option := Int_Option.None;

   function Check is new Int_Option.Is_None_Or (P => Is_Positive);
begin
   Assert (Check (Some_Pos), "Some(5) or positive = True");
   Assert (not Check (Some_Neg), "Some(-5) or positive = False");
   Assert (Check (None_Val), "None or any = True");
end Test_Is_None_Or;
```

## 6. Test Coverage

### 6.1 Coverage Goals

| Metric | Target | Current |
|--------|--------|---------|
| Statement Coverage | 90%+ | 95%+ |
| Decision Coverage | 90%+ | 95%+ |

### 6.2 Running Coverage Analysis

```bash
# Generate coverage report
make test-coverage

# View HTML report
open coverage/report/index.html
```

### 6.3 Coverage by Package

| Package | Coverage | Status |
|---------|----------|--------|
| Functional.Result | 95%+ | Exceeds target |
| Functional.Option | 95%+ | Exceeds target |
| Functional.Either | 95%+ | Exceeds target |
| Functional.Try | 95%+ | Exceeds target |
| Functional.Try.To_Result | 95%+ | Exceeds target |
| Functional.Try.To_Option | 95%+ | Exceeds target |

### 6.4 Improving Coverage

To identify uncovered lines:

```bash
# View detailed coverage report
open coverage/report/functional-result.adb.html
```

Look for red-highlighted lines (not covered) and add tests that exercise those code paths.

## 7. Test Framework

### 7.1 Test_Framework Package

The shared test infrastructure in `test/common/`:

```ada
package Test_Framework is
   --  Global counters
   Total_Pass : Natural := 0;
   Total_Fail : Natural := 0;

   --  Register test results from a test file
   procedure Register_Results (Pass, Fail : Natural);

   --  Print final summary (called by unit_runner)
   procedure Print_Summary;

   --  Return exit code based on results
   function Exit_Code return Ada.Command_Line.Exit_Status;
end Test_Framework;
```

### 7.2 Integration with unit_runner

```ada
--  unit_runner.adb
procedure Unit_Runner is
begin
   --  Run each test suite (they call Register_Results internally)
   Test_Result;
   Test_Option;
   Test_Either;
   Test_Try;
   Test_Try_Option;

   --  Print summary and exit
   Test_Framework.Print_Summary;
   Ada.Command_Line.Set_Exit_Status (Test_Framework.Exit_Code);
end Unit_Runner;
```

## 8. Test Data

### 8.1 Standard Test Types

Tests typically instantiate packages with simple types:

```ada
--  Integer results with string errors
type Error is record
   Kind    : Error_Kind;
   Message : String (1 .. 100);
   Length  : Natural;
end record;

package Int_Result is new Functional.Result (T => Integer, E => Error);
package Int_Option is new Functional.Option (T => Integer);
package Str_Int_Either is new Functional.Either (L => String, R => Integer);
```

### 8.2 Test Helper Functions

Common test utilities:

```ada
--  Create error with message
function Make_Error (Kind : Error_Kind; Msg : String) return Error is
begin
   return (Kind, Msg & (Msg'Length + 1 .. 100 => ' '), Msg'Length);
end Make_Error;

--  Simple transformation functions
function Double (X : Integer) return Integer is (X * 2);
function Add_One (X : Integer) return Integer is (X + 1);
function Is_Positive (X : Integer) return Boolean is (X > 0);
```

## 9. Continuous Integration

### 9.1 CI Commands

```bash
# Full test suite (for CI)
make test-all

# Expected exit code: 0 (all tests pass)
```

### 9.2 Success Criteria

- All unit tests pass
- Coverage >= 90% (statement + decision)
- No compiler warnings in test code
- Clean build with `alr build`

### 9.3 Platform Testing

Tests run on:
- **POSIX**: macOS, Linux
- **Windows**: Windows Server 2022 (via GitHub Actions)

## 10. Test Maintenance

### 10.1 Adding New Tests

1. Add test procedure to appropriate `test_<package>.adb`
2. Call new test procedure from the test file's main block
3. Run `make test-unit` to verify
4. Run `make test-coverage` to check coverage impact

### 10.2 Updating Tests

When API changes:
1. Update affected test procedures
2. Add new tests for new operations
3. Remove tests for removed operations
4. Update test counts in documentation

### 10.3 Test Documentation

Each test file header should include:
- Target coverage percentage
- Number of tests in file
- Brief description of what's tested

## 11. Test Statistics

### 11.1 Current Test Counts

| Test File | Tests | Target Package |
|-----------|-------|----------------|
| test_result.adb | 84 | Functional.Result (36 operations) |
| test_option.adb | 65 | Functional.Option (26 operations) |
| test_either.adb | 58 | Functional.Either (20 operations) |
| test_try.adb | 14 | Functional.Try (5 functions) |
| test_try_option.adb | 6 | Try Option bridges |
| **Total** | **227** | **87 operations** |

### 11.2 Test Commands Reference

| Command | Description |
|---------|-------------|
| `make test-all` | Run all test suites |
| `make test-unit` | Run unit tests only |
| `make test-coverage` | Run with GNATcoverage |
| `make clean-test` | Clean test artifacts |
| `./test/bin/unit_runner` | Run tests directly |

### 11.3 v3.0.0 Test Additions

The following operations were added in v3.0.0 and have full test coverage:

**Result tests added:**
- `Contains` / `"="` operator
- `Is_Ok_And`, `Is_Error_And` (strict predicates)
- `Is_Ok_Or`, `Is_Error_Or` (lenient predicates)
- `Expect_Error`, `Unwrap_Error`
- `Map_Or`, `Map_Or_Else`
- `Tap_Ok`, `Tap_Error`
- `Zip_With`, `Flatten`, `To_Option`

**Option tests added:**
- `Contains` / `"="` operator
- `Is_Some_And` (strict predicate)
- `Is_None_Or` (lenient predicate)
- `Expect`
- `Map_Or`, `Map_Or_Else`
- `Tap`
- `"and"`, `"xor"` operators
- `Zip_With`, `Flatten`
- `Ok_Or`, `Ok_Or_Else`

**Either tests added:**
- `Contains` / `"="` operator
- `Is_Left_And`, `Is_Right_And` (strict predicates)
- `Is_Left_Or`, `Is_Right_Or` (lenient predicates)
- `Get_Or_Else`
- `Map`, `Swap`, `And_Then`
- `Merge`
- `To_Option`, `To_Result`
