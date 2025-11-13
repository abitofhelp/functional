# Software Design Specification (SDS)

**Project**: Functional - Type-Safe Error Handling Library for Ada 2022
**Version**: 2.0.0
**Date**: 2025-11-13
**Author**: Michael Gardner, A Bit of Help, Inc.
**Status**: Released

---

## 1. Introduction

### 1.1 Purpose

This Software Design Specification (SDS) describes the architectural design and detailed design of the TZif library.

### 1.2 Scope

This document covers:
- Architectural patterns and decisions
- Layer organization and dependencies
- Key components and their responsibilities
- Data flow and error handling
- Design patterns employed

---

## 2. Architectural Design

### 2.1 Architecture Style

TZif uses **Hexagonal Architecture** (Ports and Adapters), also known as Clean Architecture.

**Benefits**:
- Clear separation of concerns
- Testable business logic
- Swappable infrastructure
- Compiler-enforced boundaries

### 2.2 Layer Organization

```
┌─────────────────────────────────────────┐
│         Application Layer               │
│  (Use Cases, Ports/Interfaces)          │
│                                          │
│  ┌────────────────────────────────────┐ │
│  │      Domain Layer                  │ │
│  │  (Business Logic, Entities, VOs)   │ │
│  └────────────────────────────────────┘ │
└─────────────────────────────────────────┘
           ↑                    ↑
           │                    │
┌──────────┴──────────┐  ┌─────┴──────────┐
│  Infrastructure     │  │  Infrastructure │
│  (File System)      │  │  (Parsing)      │
└─────────────────────┘  └─────────────────┘
```

### 2.3 Layer Responsibilities

#### Domain Layer
- **Purpose**: Pure business logic, no dependencies
- **Components**:
  - Value Objects: Zone_Id, Epoch_Seconds, UTC_Offset, etc.
  - Entities: Zone
  - Services: Timezone_Lookup
  - Error types and Result monads

#### Application Layer
- **Purpose**: Orchestrate domain logic, define interfaces
- **Components**:
  - Use Cases: Find_By_Id, Discover_Sources, Export_Cache, etc.
  - Inbound Ports: Interfaces for external actors
  - Outbound Ports: Interfaces for infrastructure

#### Infrastructure Layer
- **Purpose**: Implement technical concerns, adapt external systems
- **Components**:
  - Adapters: File_System.Repository, TZif_Parser
  - Platform: POSIX and Windows implementations
  - Cache: Zone_Cache, Source_Cache

---

## 3. Detailed Design

### 3.1 Domain Layer Design

#### 3.1.1 Value Objects

**Zone_Id**:
- Immutable timezone identifier
- Validates format (e.g., "America/New_York")
- Maximum 64 characters
- Case-sensitive

**Epoch_Seconds**:
- Signed 64-bit Unix timestamp
- Represents seconds since 1970-01-01 00:00:00 UTC
- Range: -2^63 to 2^63-1

**UTC_Offset**:
- Signed offset from UTC in seconds
- Range: -12 hours to +14 hours

#### 3.1.2 Entities

**Zone**:
- Timezone entity with identity (Zone_Id)
- Contains transition data
- Immutable after construction

#### 3.1.3 Services

**Timezone_Lookup**:
- Domain service for timezone operations
- Finds transitions for given epochs
- Pure functions, no side effects

### 3.2 Application Layer Design

#### 3.2.1 Use Cases

Each use case implements a single user operation:

1. **Find_By_Id**: Retrieve zone by exact ID
2. **Find_By_Region**: Find zones in region
3. **Find_By_Pattern**: Pattern matching search
4. **Find_By_Regex**: Regex-based search
5. **Find_My_Id**: Detect local timezone
6. **Get_Transition_At_Epoch**: Lookup transition
7. **Discover_Sources**: Scan filesystem
8. **Load_Source**: Load timezone data
9. **Validate_Source**: Validate source
10. **Export_Cache**: Export to JSON
11. **Import_Cache**: Import from JSON
12. **List_All_Order_By_Id**: List all zones

#### 3.2.2 Port Design

**Inbound Ports**:
- Define use case interfaces
- Input types and result types
- No implementation

**Outbound Ports**:
- Define infrastructure interfaces
- Repository operations
- No implementation

### 3.3 Infrastructure Layer Design

#### 3.3.1 TZif Parser

**Responsibilities**:
- Read TZif binary files
- Parse header, transitions, types
- Handle all three format versions
- Validate data integrity

**Design**:
- State machine for parsing
- Sequential reading with backtracking
- Error handling via Result monad

#### 3.3.2 File System Repository

**Responsibilities**:
- Load zones from filesystem
- Discover timezone sources
- Navigate directory structure
- Resolve symbolic links

**Design**:
- Platform abstraction for symlinks
- Lazy loading of zone data
- Canonical path handling

#### 3.3.3 Cache System

**Zone_Cache**:
- In-memory cache of parsed zones
- Thread-safe operations
- LRU eviction (future)

**Source_Cache**:
- Cache of discovered sources
- Serialization to JSON
- Version tracking

---

## 4. Design Patterns

### 4.1 Railway-Oriented Programming

**Pattern**: Result monad for error handling
**Purpose**: Avoid exceptions, explicit error handling
**Implementation**: `Domain.Error.Result.Generic_Result`

**Usage**:
```ada
function Find_By_Id(Zone_Id) return Result_Type;
-- Returns: Ok(Zone) or Error(Error_Type)
```

### 4.2 Repository Pattern

**Pattern**: Abstract data access
**Purpose**: Decouple business logic from data storage
**Implementation**: `Application.Port.Outbound.Zone_Repository`

### 4.3 Adapter Pattern

**Pattern**: Adapt external systems to ports
**Purpose**: Implement infrastructure concerns
**Implementation**: `Infrastructure.Adapter.File_System.*`

### 4.4 Strategy Pattern

**Pattern**: Platform-specific implementations
**Purpose**: Support multiple operating systems
**Implementation**: `Infrastructure.Platform.POSIX/Windows`

---

## 5. Data Flow

### 5.1 Zone Lookup Flow

```
User Request
    ↓
Use Case (Find_By_Id)
    ↓
Domain Service (Timezone_Lookup)
    ↓
Repository Port
    ↓
File System Adapter
    ↓
TZif Parser
    ↓
Zone Entity ← Domain Value Objects
    ↓
Result(Zone) ← Error Handling
    ↓
User Response
```

### 5.2 Error Propagation

All errors propagate up via Result monad:
1. Infrastructure error occurs
2. Wrapped in domain error type
3. Returned as Error variant
4. Use case handles or propagates
5. User receives descriptive error

---

## 6. Concurrency Design

### 6.1 Thread Safety

- **Domain Layer**: Pure, stateless → thread-safe
- **Application Layer**: Stateless → thread-safe
- **Repository**: Protected operations → thread-safe

### 6.2 Parallelism Support

- CPU detection: `Infrastructure.CPU.Get_CPU_Count`
- Task count calculation: Optimal for current hardware
- Parallel source discovery (future)

---

## 7. Performance Design

### 7.1 Caching Strategy

- Parse zones once, cache in memory
- Export/import caches to avoid re-parsing
- Lazy loading of zone data

### 7.2 Optimization Techniques

- Bounded strings (no heap allocation)
- Stack allocation where possible
- Minimal copying of data structures

---

## 8. Security Design

### 8.1 Input Validation

- Validate all zone IDs
- Bounds checking on all inputs
- Path canonicalization to prevent traversal attacks

### 8.2 Error Information

- No sensitive data in error messages
- Safe error types for external display

---

## 9. Testing Strategy

### 9.1 Unit Tests

- Test each package in isolation
- Mock dependencies via ports
- 0 unit tests

### 9.2 Integration Tests

- Test full stack with real data
- Test error conditions
- 0 integration tests

### 9.3 Test Organization

```
test/
├── unit/           # Unit tests
├── integration/    # Integration tests
├── support/        # Test spies and helpers
└── common/         # Shared test framework
```

---

## 10. Build and Deployment

### 10.1 Build System

- **Tool**: Alire (Ada Library Repository)
- **Configuration**: alire.toml per layer
- **Build**: `alr build`

### 10.2 Project Structure

```
tzif/
├── src/
│   ├── domain/
│   ├── application/
│   └── infrastructure/
├── test/
├── examples/
├── docs/
└── scripts/
```

---

**Document Control**:
- Version: 1.0.0
- Last Updated: 2025-11-13
- Status: Released
- Copyright © 2025 Michael Gardner, A Bit of Help, Inc.
- License: BSD-3-Clause
