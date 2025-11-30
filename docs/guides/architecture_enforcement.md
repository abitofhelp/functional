# Architecture Enforcement in Ada/GPRbuild

**Version:** 1.0.0<br>
**Date:** November 29, 2025<br>
**SPDX-License-Identifier:** BSD-3-Clause<br>
**License File:** See the LICENSE file in the project root<br>
**Copyright:** © 2025 Michael Gardner, A Bit of Help, Inc.<br>
**Status:** Released


This document explains how to enforce hexagonal/clean architecture rules in Ada projects using GPRbuild and the architecture guard script.

## Two Architectural Patterns

Our hybrid DDD/Clean/Hexagonal architecture supports two project types with different layer structures:

| Aspect | Application (5-layer) | Library (4-layer) |
|--------|----------------------|-------------------|
| **Outer layers** | Bootstrap + Presentation | API (3-package pattern) |
| **Composition root** | Bootstrap | API.Desktop |
| **Public interface** | Presentation (CLI/UI) | API facade |
| **SPARK boundary** | Optional | API.Operations |
| **Consumer** | End users | Other Ada projects |

Both patterns share the same inner core: **Domain → Application → Infrastructure**.

---

## Application Architecture (5-Layer)

For executable projects (CLI tools, servers, embedded apps):

```
┌──────────────────────────────────────┐
│         Bootstrap (outermost)        │
│   Bootstrap → All ✓                  │
│   All → Bootstrap ✗                  │
│                                      │
│  ┌────────────────────────────────┐  │
│  │  Presentation | Infrastructure │  │
│  │       ↓              ↓      ↓  │  │
│  │       │              └──────┘  │  │
│  │       │  Presentation ↔ Infra  │  │
│  │       │       ✗ (forbidden)    │  │
│  │  ┌────┼──────────────────┐     │  │
│  │  │    ↓         ↓        │     │  │
│  │  │   Application         │     │  │
│  │  │        ↓              │     │  │
│  │  │   ┌──────────┐        │     │  │
│  │  │   │  Domain  │        │     │  │
│  │  │   └──────────┘        │     │  │
│  │  └───────────────────────┘     │  │
│  └────────────────────────────────┘  │
└──────────────────────────────────────┘
```

**Application Layer Rules:**
- **Domain**: ZERO dependencies (innermost core)
- **Application**: Depends ONLY on Domain
- **Infrastructure**: Depends on Application + Domain
- **Presentation**: Depends ONLY on Application (NOT Domain, NOT Infrastructure)
- **Bootstrap**: Can depend on all layers; NO layer can depend on Bootstrap
- **Lateral Rule**: Presentation ↔ Infrastructure cannot depend on each other

---

## Library Architecture (4-Layer with 3-Package API)

For reusable libraries (like tzif) consumed by other Ada projects:

```
┌─────────────────────────────────────────────────────────┐
│                    API Layer (outermost)                 │
│  ┌─────────────────────────────────────────────────┐    │
│  │  API Facade ──► API.Desktop ──► API.Operations  │    │
│  │  (public)       (composition)   (SPARK-safe)    │    │
│  │                      │                │         │    │
│  │                      ▼                │         │    │
│  │              Infrastructure           │         │    │
│  │                      │                │         │    │
│  │                      ▼                ▼         │    │
│  │  ┌───────────────────────────────────────────┐ │    │
│  │  │              Application                   │ │    │
│  │  │                    │                       │ │    │
│  │  │              ┌──────────┐                  │ │    │
│  │  │              │  Domain  │                  │ │    │
│  │  │              └──────────┘                  │ │    │
│  │  └───────────────────────────────────────────┘ │    │
│  └─────────────────────────────────────────────────┘    │
└─────────────────────────────────────────────────────────┘
```

**Library Layer Rules:**
- **Domain**: ZERO dependencies (innermost core)
- **Application**: Depends ONLY on Domain
- **Infrastructure**: Depends on Application + Domain
- **API.Operations**: Depends ONLY on Application + Domain (SPARK-safe boundary)
- **API.Desktop**: Composition root - can depend on ALL layers including Infrastructure
- **API Facade**: Re-exports types, delegates to API.Desktop

### The 3-Package API Pattern

Libraries use a **3-package API pattern** instead of Bootstrap/Presentation:

| Package | Role | Equivalent in Apps | Dependencies | SPARK |
|---------|------|-------------------|--------------|-------|
| **API.Operations** | SPARK-safe operations | *(no equivalent)* | Application + Domain only | On |
| **API.Desktop** | Composition root | Bootstrap | All layers | Off |
| **API Facade** | Public interface | Presentation | Delegates to Desktop | Off |

**Why three packages?**

1. **API.Operations** - Enables formal verification via SPARK. Generic package parameterized by output ports. No Infrastructure dependencies.

2. **API.Desktop** - Platform-specific wiring. Instantiates API.Operations with concrete Infrastructure adapters (e.g., Console_Writer). Other variants possible: API.Embedded, API.Web, API.Test.

3. **API Facade** - Clean public interface for consumers. Re-exports Domain/Application types. Delegates operations to API.Desktop.

### Mapping Between Patterns

The library pattern is NOT a 1:1 mapping to the application pattern:

```
APPLICATION                         LIBRARY
═══════════                         ═══════
Bootstrap (composition root)   ──►  API.Desktop (composition root)
Presentation (public UI/CLI)   ──►  API Facade (public interface)
        *(no equivalent)*      ◄──  API.Operations (SPARK boundary)
Infrastructure                 ══►  Infrastructure
Application                    ══►  Application
Domain                         ══►  Domain
```

**Key difference**: Libraries add `API.Operations` as a SPARK verification boundary between the public API and application logic. Applications typically don't need this because they control their own entry points.

## Critical: Preventing Transitive Domain Exposure

**The Challenge**: Even though Presentation depends only on Application, it could still access Domain types if Application exposes them in its public interface.

**Example of Violation**:
```ada
-- application-usecase-find_by_id.ads
with Domain.Entity.Zone;  -- Application depends on Domain (OK)

package Application.Usecase.Find_By_Id is
   -- BAD: Exposing Domain type to Application clients!
   function Execute (Id : String) return Domain.Entity.Zone;
end Application.Usecase.Find_By_Id;

-- presentation-main.adb
with Application.Usecase.Find_By_Id;  -- Presentation depends on Application (OK)

-- But now Presentation has transitive access to Domain.Entity.Zone!
-- This violates the isolation principle.
```

## Enforcement Strategies

### 1. Architecture Guard Script (Compile-Time Dependencies)

**What it does**: Validates that layers don't directly `with` forbidden packages.

**Usage**:
```bash
python3 scripts/arch_guard.py
```

**Run in CI/CD**:
```bash
# In your CI pipeline
alr build
python3 scripts/arch_guard.py || exit 1
```

**Limitations**:
- ✅ Detects direct dependency violations (e.g., Presentation `with Domain.*`)
- ❌ Cannot detect transitive exposure through Application interfaces

### 2. Stand-Alone Library with Library_Interface (Recommended - IMPLEMENTED in tzif)

**What it does**: Makes Application a stand-alone library with explicit public interface, preventing transitive access to Domain packages.

**Implementation**:

```ada
-- application.gpr
with "../domain/domain.gpr";  -- Application depends on Domain

library project Application is
   for Library_Name use "application";
   for Library_Kind use "static";

   -- Enable stand-alone library mode
   for Library_Standalone use "standard";

   -- Explicit public interface - Domain.* packages NOT listed!
   for Library_Interface use
     ("Application",
      "Application.Usecase",
      "Application.Usecase.Find_By_Id",
      -- ... other Application packages
      -- NOTE: Domain.* packages deliberately EXCLUDED
     );

   -- Clients can ONLY access packages in Library_Interface
   -- Domain packages are NOT accessible transitively
end Application;

-- presentation.gpr
with "../application/application.gpr";  -- Depends on Application

library project Presentation is
   for Library_Name use "presentation";
   for Library_Kind use "static";
   for Source_Dirs use (".");
   -- Can access Application.* packages
   -- CANNOT access Domain.* packages (not in Library_Interface)
   -- GPRbuild enforces this at compile time!
end Presentation;
```

**Result**:
- Presentation can access `Application.*` packages
- Presentation CANNOT access `Domain.*` packages (compilation error if attempted)
- Even if Application returns Domain types, Presentation cannot use them because the Domain package specs are not in the library interface

**Benefits**:
- ✅ **Enforced at compile time by GPRbuild** (not just convention)
- ✅ **IMPLEMENTED in tzif library** - Application layer already configured
- ✅ No folder restructuring needed
- ✅ Clear separation without DTOs (if Domain types are designed for external use)

**Drawbacks**:
- ⚠️ If Application returns Domain types, they become part of the public API
- ⚠️ May require DTOs for strict isolation (see Strategy #4)

### 3. Limited/Private With (Ada 2022)

**What it does**: Prevents automatic transitive visibility of types.

**Implementation**:

```ada
-- application-usecase-find_by_id.ads
limited with Domain.Entity.Zone;  -- Limited visibility

package Application.Usecase.Find_By_Id is
   -- Can only use Domain.Entity.Zone in incomplete form
   -- Cannot expose it directly to clients
   type Zone_DTO is record
      Id     : String (1 .. 100);
      Offset : Integer;
   end record;

   function Execute (Id : String) return Zone_DTO;
private
   -- Full visibility in private part and body only
end Application.Usecase.Find_By_Id;
```

**Benefits**:
- ✅ Prevents accidental transitive exposure
- ✅ Enforces DTO/interface segregation pattern
- ✅ Compile-time checking

**Drawbacks**:
- ⚠️ Requires discipline (must use DTOs consistently)
- ⚠️ More complex package structure

### 4. Interface Segregation Pattern (Design)

**What it does**: Application provides view models/DTOs instead of Domain entities.

**Implementation**:

```ada
-- domain/entity/domain-entity-zone.ads
package Domain.Entity.Zone is
   type Zone_Type is private;
   -- Rich domain behavior
private
   type Zone_Type is record
      -- Internal domain details
   end record;
end Domain.Entity.Zone;

-- application/model/application-model-zone_view.ads
package Application.Model.Zone_View is
   -- Presentation-specific view model (NO Domain dependencies)
   type Zone_View is record
      Id          : String (1 .. 100);
      Display_Name : String (1 .. 200);
      Offset_Hours : Integer;
   end record;
end Application.Model.Zone_View;

-- application/usecase/application-usecase-find_by_id.ads
with Application.Model.Zone_View;  -- NOT Domain.Entity.Zone!

package Application.Usecase.Find_By_Id is
   function Execute (Id : String)
      return Application.Model.Zone_View.Zone_View;
end Application.Usecase.Find_By_Id;

-- application/usecase/application-usecase-find_by_id.adb (body)
with Domain.Entity.Zone;  -- Domain visible in BODY only

package body Application.Usecase.Find_By_Id is
   function Execute (Id : String)
      return Application.Model.Zone_View.Zone_View is
      Domain_Zone : Domain.Entity.Zone.Zone_Type;
   begin
      -- 1. Get Domain entity
      Domain_Zone := Repository.Find (Id);

      -- 2. Map to view model
      return Application.Model.Zone_View.Zone_View'(
         Id => Id,
         Display_Name => Extract_Name (Domain_Zone),
         Offset_Hours => Get_Offset (Domain_Zone)
      );
   end Execute;
end Application.Usecase.Find_By_Id;
```

**Benefits**:
- ✅ Complete isolation (Presentation never sees Domain types)
- ✅ Presentation gets exactly what it needs (no coupling)
- ✅ Can evolve Domain independently

**Drawbacks**:
- ❌ Most verbose (requires mapping layer)
- ❌ Performance overhead (object copying)

## Recommendations by Project Type

### Library Projects (like tzif)

**Current Approach**:
- Domain types ARE exposed through Application layer
- Result monad and value objects are designed as public contracts
- ✅ Acceptable because types are immutable and part of the API

**Enforcement**:
1. Use architecture guard script in CI/CD
2. Document in README that Domain types are part of the public API
3. Ensure Domain types are designed for external consumption (immutable, safe)

**If strict isolation is needed**:
- Use Strategy #2 (Separate GPR projects)
- Create Application DTOs that don't expose Domain types
- Document the mapping layer

### Executable Projects (with Presentation layer)

**Recommended Approach**:
1. **Must use**: Separate GPR projects (Strategy #2)
2. **Should use**: Interface Segregation (Strategy #4)
3. **Must use**: Architecture guard script in CI/CD

**Project Structure**:
```
src/
  domain/domain.gpr           (0 dependencies)
  application/application.gpr (depends: domain.gpr)
  infrastructure/infrastructure.gpr (depends: application.gpr, domain.gpr)
  presentation/presentation.gpr (depends: application.gpr ONLY)
  bootstrap/main.gpr (depends: all)
```

## CI/CD Integration

Add to your CI pipeline:

```yaml
# .github/workflows/build.yml
name: Build and Validate

on: [push, pull_request]

jobs:
  validate-architecture:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - name: Install Alire
        run: |
          wget https://github.com/alire-project/alire/releases/download/v2.0.0/alr-2.0.0-bin-x86_64-linux.zip
          unzip alr-2.0.0-bin-x86_64-linux.zip
          echo "$PWD/bin" >> $GITHUB_PATH

      - name: Build
        run: alr build

      - name: Validate Architecture
        run: python3 scripts/arch_guard.py

      - name: Run Tests
        run: cd test && alr build && ./bin/test_runner
```

## Summary

| Strategy | Compile-Time | Transitive Prevention | Effort | Strictness | Status in tzif |
|----------|-------------|---------------------|--------|------------|----------------|
| Arch Guard Script | ✅ Direct deps | ❌ | Low | Medium | ✅ Implemented |
| Stand-Alone Library | ✅ Complete | ✅ | Low | High | ✅ Implemented |
| Limited/Private With | ✅ Partial | ⚠️ | Medium | Medium-High | Not needed |
| Interface Segregation | ✅ Complete | ✅ | High | Highest | Not needed |

**Best Practice for Executable Projects**: Use **Stand-Alone Library** + **Architecture Guard Script**.

**tzif Implementation**:
- ✅ Application layer configured as stand-alone library with explicit `Library_Interface`
- ✅ Domain packages NOT included in `Library_Interface` (transitive access prevented)
- ✅ Architecture guard script validates direct dependencies
- ✅ GPRbuild enforces interface restrictions at compile time

**Result**: Presentation layer (when added) CANNOT access Domain packages, even transitively through Application.
