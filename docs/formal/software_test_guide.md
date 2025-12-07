# Software Test Guide (STG)

**Project:** Functional - Type-Safe Error Handling Library for Ada 2022
**Version:** 3.0.0
**Date:** December 06, 2025
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
- **Both Paths**: Ok/Err, Some/None, Left/Right branches exercised
- **Contract Verification**: Pre/Post conditions validated
- **Edge Cases**: Empty values, error conditions, boundary inputs

## 3. Test Organization

### 3.1 Directory Structure

```
test/
├── alire.toml              # Test crate manifest
├── functional_tests.gpr    # Test GPR project
├── bin/                    # Compiled test executables
│   └── unit_runner
├── common/                 # Shared test infrastructure
│   ├── test_framework.ads  # Test framework spec
│   └── test_framework.adb  # Test framework body
├── config/                 # Alire-generated config
│   └── functional_tests_config.ads
└── unit/                   # Unit test sources
    ├── unit_runner.adb     # Main test runner
    ├── test_result.adb     # Result package tests
    ├── test_option.adb     # Option package tests
    ├── test_either.adb     # Either package tests
    ├── test_try.adb        # Try package tests
    └── test_try_option.adb # Try Option bridge tests
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

Testing Result constructors...
[PASS] Ok creates success result
[PASS] Err creates error result
...

Testing Option constructors...
[PASS] New_Some creates Some option
[PASS] None creates empty option
...

======================================================================
Test Summary
======================================================================
Total:  93
Passed: 93
Failed: 0

All tests passed!
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
| **Constructors** | `Test_Ok`, `Test_Err`, `Test_New_Some`, `Test_None` |
| **Predicates** | `Test_Is_Ok`, `Test_Is_Err`, `Test_Is_Some`, `Test_Is_None` |
| **Extractors** | `Test_Value_Ok`, `Test_Value_Err`, `Test_Error` |
| **Transforms** | `Test_Map_Ok`, `Test_Map_Err`, `Test_And_Then_Ok`, `Test_And_Then_Err` |
| **Recovery** | `Test_Fallback`, `Test_Recover`, `Test_Unwrap_Or` |

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

--  Test Map with Err input
procedure Test_Map_Err is
   R : Int_Result.Result := Int_Result.Err (Error_Value);
   function Double is new Int_Result.Map (F => Times_Two);
begin
   Assert (Int_Result.Is_Err (Double (R)), "Map Err returns Err unchanged");
end Test_Map_Err;
```

## 6. Test Coverage

### 6.1 Coverage Goals

| Metric | Target | Current |
|--------|--------|---------|
| Statement Coverage | 90%+ | 95% |
| Decision Coverage | 90%+ | 95% |

### 6.2 Running Coverage Analysis

```bash
# Generate coverage report
make test-coverage

# View HTML report
open coverage/report/index.html
```

### 6.3 Coverage by Package

| Package | Coverage | Lines |
|---------|----------|-------|
| Functional.Result | 91% | 48/53 |
| Functional.Option | 100% | 28/28 |
| Functional.Either | 100% | 18/18 |
| Functional.Try | 100% | 18/18 |
| Functional.Try.To_Result | 100% | 8/8 |
| Functional.Try.To_Option | 100% | 6/6 |
| **Total** | **95%** | **152/160** |

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

- All 93 unit tests pass
- Coverage >= 90% (statement + decision)
- No compiler warnings in test code
- Clean build with `alr build`

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
| test_result.adb | 35 | Functional.Result |
| test_option.adb | 22 | Functional.Option |
| test_either.adb | 16 | Functional.Either |
| test_try.adb | 14 | Functional.Try |
| test_try_option.adb | 6 | Try Option bridges |
| **Total** | **93** | |

### 11.2 Test Commands Reference

| Command | Description |
|---------|-------------|
| `make test-all` | Run all test suites |
| `make test-unit` | Run unit tests only |
| `make test-coverage` | Run with GNATcoverage |
| `make clean-test` | Clean test artifacts |
| `./test/bin/unit_runner` | Run tests directly |
