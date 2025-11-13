# Software Test Guide

**Project**: Functional - Type-Safe Error Handling Library for Ada 2022
**Version**: 2.0.0
**Date**: 2025-11-13
**Author**: Michael Gardner, A Bit of Help, Inc.
**Status**: Released

---

## 1. Introduction

### 1.1 Purpose

This Software Test Guide describes the testing approach, test organization, and procedures for the TZif library.

### 1.2 Scope

This document covers:
- Test strategy and levels
- Test organization
- Running tests
- Writing new tests
- Test coverage analysis

---

## 2. Test Strategy

### 2.1 Testing Levels

**Unit Tests**:
- Test individual packages in isolation
- Mock dependencies via test spies
- Focus on domain and value objects
- Count: 0 tests

**Integration Tests**:
- Test full stack with real data
- Test use cases end-to-end
- Test error conditions and edge cases
- Count: 0 tests

**Examples as Tests**:
- Working examples that demonstrate usage
- Validate real-world scenarios
- Count: 0 examples

### 2.2 Testing Approach

- **Test-Driven**: Tests written alongside or before code
- **Railway-Oriented**: Test both success and error paths
- **Comprehensive**: Cover normal, edge, and error cases
- **Automated**: All tests runnable via `make test-all`

---

## 3. Test Organization

### 3.1 Directory Structure

```
test/
├── unit/              # Unit tests
│   ├── test_zone_id.adb
│   ├── test_iana_releases.adb
│   └── unit_runner.adb
├── integration/       # Integration tests
│   ├── test_find_by_id.adb
│   ├── test_discover_sources.adb
│   └── integration_runner.adb
├── support/           # Test utilities
│   └── test_spies/    # Test spies for ports
└── common/            # Shared test framework
    └── test_framework.ads
```

### 3.2 Test Naming Convention

- **Pattern**: `test_<component>.adb`
- **Example**: `test_zone_id.adb` tests `Domain.Value_Object.Zone_Id`
- **Runner**: Each level has a runner executable

---

## 4. Running Tests

### 4.1 Quick Start

```bash
# Run all tests (unit + integration)
make test-all

# Run only unit tests
make test-unit

# Run only integration tests
make test-integration

# Run with coverage
make test-coverage
```

### 4.2 Individual Test Execution

```bash
# Run specific unit test
./test/bin/test_zone_id

# Run specific integration test
./test/bin/test_find_by_id
```

### 4.3 Test Output

**Success**:
```
Test: Zone ID Creation
  [PASS] Make_Zone_Id creates valid zone
  [PASS] Zone ID length is correct
====================================================
  Results: 2 / 2 passed
  Status: ALL TESTS PASSED
====================================================
```

**Failure**:
```
Test: Zone ID Creation
  [PASS] Make_Zone_Id creates valid zone
  [FAIL] Zone ID length is incorrect
====================================================
  Results: 1 / 2 passed
  Status: TESTS FAILED
====================================================
```

---

## 5. Writing Tests

### 5.1 Unit Test Structure

```ada
pragma Ada_2022;
with Test_Framework;
with Domain.Value_Object.Zone_Id;

procedure Test_Zone_Id is
   use Domain.Value_Object.Zone_Id;
   use Test_Framework;

   Test_Count : Natural := 0;
   Pass_Count : Natural := 0;

   procedure Assert(Condition : Boolean; Test_Name : String) is
   begin
      Test_Count := Test_Count + 1;
      if Condition then
         Pass_Count := Pass_Count + 1;
         Put_Line("  [PASS] " & Test_Name);
      else
         Put_Line("  [FAIL] " & Test_Name);
      end if;
   end Assert;

begin
   Put_Line("Test: Zone ID Creation");

   -- Test case
   declare
      Zone : constant Zone_Id_Type := Make_Zone_Id("America/New_York");
   begin
      Assert(To_String(Zone) = "America/New_York",
             "Make_Zone_Id creates valid zone");
   end;

   -- Final summary
   Report_Results(Test_Count, Pass_Count);
end Test_Zone_Id;
```

### 5.2 Integration Test Structure

```ada
pragma Ada_2022;
with Test_Framework;
with Application.Usecase.Find_By_Id;
with Infrastructure.Adapter.File_System.Repository;

procedure Test_Find_By_Id is
   use Test_Framework;

begin
   Put_Line("Test: Find By ID - Basic Lookup");

   -- Setup real repository
   declare
      Repo : Infrastructure.Adapter.File_System.Repository.Repository_Type;
      UC   : Application.Usecase.Find_By_Id.Use_Case_Type(Repo'Access);
      Result : constant Find_By_Id_Result := UC.Execute("America/New_York");
   begin
      Assert(Result.Is_Ok, "Should find valid zone");
      Assert(Result.Value.Get_Id = "America/New_York", "Zone ID matches");
   end;

   Report_Results(Test_Count, Pass_Count);
end Test_Find_By_Id;
```

### 5.3 Using Test Spies

```ada
-- Test use case without real infrastructure
with Test_Spies.Find_By_Id_Spy;

procedure Test_Use_Case is
   Spy : Test_Spies.Find_By_Id_Spy.Spy_Type;
   UC  : Application.Usecase.SomeCase.Use_Case_Type(Spy'Access);
begin
   -- Execute use case
   UC.Execute(...);

   -- Verify spy was called correctly
   Assert(Spy.Was_Called, "Repository was called");
   Assert(Spy.Call_Count = 1, "Called exactly once");
end Test_Use_Case;
```

---

## 6. Test Coverage

### 6.1 Coverage Goals

- **Target**: > 90% line coverage
- **Critical Code**: 100% coverage for error handling
- **Domain Layer**: Near 100% coverage

### 6.2 Running Coverage Analysis

```bash
# Generate coverage report
make test-coverage

# View HTML report
open coverage/report/index.html
```

### 6.3 Coverage Reports

Coverage reports show:
- Lines executed vs total
- Branches taken
- Functions covered
- Per-file statistics

---

## 7. Test Data

### 7.1 Test Timezone Data

**Location**: `/usr/share/zoneinfo` (system default)

**Test Zones Used**:
- `America/New_York`: Standard US timezone
- `Europe/London`: European timezone with DST
- `UTC`: Special timezone with no transitions
- `America/Los_Angeles`: West coast timezone

### 7.2 Test Cache Data

Test caches are generated during tests and cleaned up after.

---

## 8. Continuous Integration

### 8.1 CI Pipeline

```yaml
steps:
  - name: Build
    run: alr build

  - name: Unit Tests
    run: make test-unit

  - name: Integration Tests
    run: make test-integration

  - name: Coverage
    run: make test-coverage

  - name: Examples
    run: make test-examples
```

### 8.2 Success Criteria

All must pass:
- ✅ Zero build warnings
- ✅ All unit tests pass
- ✅ All integration tests pass
- ✅ All examples execute successfully
- ✅ Coverage > 90%

---

## 9. Test Maintenance

### 9.1 Adding New Tests

1. Create test file: `test/unit/test_<component>.adb`
2. Write test procedure
3. Add to runner if needed
4. Update Makefile if required

### 9.2 Updating Tests

- Update tests when requirements change
- Keep tests in sync with code
- Refactor tests alongside code

### 9.3 Test Documentation

- Document test purpose in header
- Comment complex test scenarios
- Explain expected vs actual behavior

---

## 10. Known Issues

None at this time. All 0 tests pass.

---

## 11. Appendices

### 11.1 Test Statistics

- Total Tests: 0
  - Unit: 0
  - Integration: 0
- Examples: 0
- Test Framework: Custom Ada framework
- Coverage Tool: gnatcov

### 11.2 Test Commands Reference

```bash
make test-all          # Run all tests
make test-unit         # Unit tests only
make test-integration  # Integration tests only
make test-coverage     # With coverage
make clean-coverage    # Remove coverage data
```

---

**Document Control**:
- Version: 1.0.0
- Last Updated: 2025-11-13
- Status: Released
- Copyright © 2025 Michael Gardner, A Bit of Help, Inc.
- License: BSD-3-Clause
