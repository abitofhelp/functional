# Software Requirements Specification (SRS)

**Project**: Functional - Type-Safe Error Handling Library for Ada 2022
**Version**: 2.0.0
**Date**: 2025-11-13
**Author**: Michael Gardner, A Bit of Help, Inc.
**Status**: Released

---

## 1. Introduction

### 1.1 Purpose

This Software Requirements Specification (SRS) describes the functional and non-functional requirements for TZif, a production-ready Ada 2022 library for parsing and querying IANA timezone information from TZif binary files.

### 1.2 Scope

TZif provides:
- Parsing of TZif binary format (versions 1, 2, and 3)
- Query operations for timezone data by ID, region, and pattern
- Timezone transition lookups for specific epochs
- Source discovery and validation
- Cache export/import functionality
- Thread-safe operations
- Railway-oriented error handling

### 1.3 Definitions and Acronyms

- **TZif**: Timezone Information Format (IANA standard binary format)
- **IANA**: Internet Assigned Numbers Authority
- **SRS**: Software Requirements Specification
- **API**: Application Programming Interface
- **UTC**: Coordinated Universal Time

### 1.4 References

- IANA Time Zone Database: https://www.iana.org/time-zones
- TZif Format Specification: RFC 8536
- Ada 2022 Language Reference Manual

---

## 2. Overall Description

### 2.1 Product Perspective

TZif is a standalone Ada library implementing hexagonal (ports and adapters) architecture with clean separation between domain logic, application use cases, and infrastructure adapters.

**Architecture Layers**:
- **Domain Layer**: Pure business logic, value objects, entities
- **Application Layer**: Use cases, ports (interfaces)
- **Infrastructure Layer**: Adapters for file system, parsing, caching

### 2.2 Product Features

1. **TZif Parsing**: Parse TZif v1, v2, v3 binary files
2. **Timezone Queries**: Find by ID, region, pattern, regex
3. **Transition Lookups**: Get timezone info for specific epoch
4. **Source Management**: Discover and validate timezone sources
5. **Caching**: Export/import zone caches for performance
6. **Error Handling**: Railway-oriented programming with Result monads

### 2.3 User Classes

- **Application Developers**: Integrate timezone functionality
- **System Administrators**: Configure timezone data sources
- **Library Maintainers**: Extend and maintain the codebase

### 2.4 Operating Environment

- **Platforms**: POSIX-compliant systems (Linux, macOS, BSD), Windows
- **Ada Compiler**: GNAT FSF 14.2+ or GNAT Pro 25.0+
- **Ada Version**: Ada 2022
- **Dependencies**: functional ^1.0.0 (Result/Option monads)

---

## 3. Functional Requirements

### 3.1 TZif Parsing (FR-01)

**Priority**: High
**Description**: Parse TZif binary files in all versions.

**Requirements**:
- FR-01.1: Parse TZif version 1 (legacy 32-bit)
- FR-01.2: Parse TZif version 2 (64-bit)
- FR-01.3: Parse TZif version 3 (with extensions)
- FR-01.4: Validate file format and magic numbers
- FR-01.5: Handle malformed files gracefully

### 3.2 Timezone Query Operations (FR-02)

**Priority**: High
**Description**: Provide query operations for timezone data.

**Requirements**:
- FR-02.1: Find timezone by exact ID (e.g., "America/New_York")
- FR-02.2: Find timezones by region (e.g., "America")
- FR-02.3: Find timezones by pattern matching
- FR-02.4: Find timezones by regex
- FR-02.5: List all available timezones
- FR-02.6: Get local timezone ID

### 3.3 Transition Lookups (FR-03)

**Priority**: High
**Description**: Retrieve timezone information for specific points in time.

**Requirements**:
- FR-03.1: Get transition info for given epoch seconds
- FR-03.2: Return UTC offset at specific time
- FR-03.3: Return timezone abbreviation (e.g., "PST", "PDT")
- FR-03.4: Handle times before/after transition data

### 3.4 Source Management (FR-04)

**Priority**: Medium
**Description**: Discover and validate timezone data sources.

**Requirements**:
- FR-04.1: Scan filesystem paths for timezone sources
- FR-04.2: Validate source directory structure
- FR-04.3: Check for required VERSION file
- FR-04.4: Count available zone files
- FR-04.5: Generate unique IDs for sources (ULID)

### 3.5 Cache Management (FR-05)

**Priority**: Medium
**Description**: Export and import zone caches for performance.

**Requirements**:
- FR-05.1: Export zone cache to JSON format
- FR-05.2: Import zone cache from JSON format
- FR-05.3: Validate cache integrity
- FR-05.4: Handle cache versioning

### 3.6 Error Handling (FR-06)

**Priority**: High
**Description**: Railway-oriented error handling without exceptions.

**Requirements**:
- FR-06.1: Use Result monad for all fallible operations
- FR-06.2: Provide descriptive error messages
- FR-06.3: Error codes for all failure modes
- FR-06.4: No exceptions in library code

---

## 4. Non-Functional Requirements

### 4.1 Performance (NFR-01)

- NFR-01.1: Parse TZif file in < 10ms
- NFR-01.2: Zone lookup in < 1ms (cached)
- NFR-01.3: Transition lookup in < 100μs

### 4.2 Reliability (NFR-02)

- NFR-02.1: Handle all malformed inputs gracefully
- NFR-02.2: No memory leaks
- NFR-02.3: Thread-safe repository operations

### 4.3 Portability (NFR-03)

- NFR-03.1: Support POSIX platforms (Linux, macOS, BSD)
- NFR-03.2: Support Windows
- NFR-03.3: No platform-specific code in domain/application layers

### 4.4 Maintainability (NFR-04)

- NFR-04.1: Hexagonal architecture with clear boundaries
- NFR-04.2: Comprehensive documentation (docstrings)
- NFR-04.3: > 90% test coverage
- NFR-04.4: Zero compiler warnings

### 4.5 Usability (NFR-05)

- NFR-05.1: Clear, intuitive API
- NFR-05.2: Working examples for all use cases
- NFR-05.3: Comprehensive error messages

---

## 5. System Requirements

### 5.1 Hardware Requirements

- **Minimum**:
  - CPU: Any modern processor
  - RAM: 64 MB
  - Disk: 10 MB

### 5.2 Software Requirements

- **Operating System**: Linux, macOS, BSD, or Windows
- **Compiler**: GNAT FSF 14.2+ or GNAT Pro 25.0+
- **Build System**: Alire 2.0+

---

## 6. Verification and Validation

### 6.1 Test Coverage

- Unit tests: 0 tests
- Integration tests: 0 tests
- Examples: 0 working examples

### 6.2 Verification Methods

- **Code Review**: All code reviewed before merge
- **Static Analysis**: Zero compiler warnings
- **Dynamic Testing**: All tests must pass
- **Coverage Analysis**: > 90% line coverage

---

## 7. Appendices

### 7.1 TZif Format Overview

TZif (Timezone Information Format) is a binary format defined by IANA for storing timezone data. The format includes:
- Header with version and counts
- Transition times
- Transition types
- Timezone abbreviations
- Leap second information
- Standard/wall indicators
- UTC/local indicators

### 7.2 Project Statistics

- Ada specification files: 5
- Ada implementation files: 4
- Total lines of code: ~15,000 (estimated)
- Architecture layers: 

---

**Document Control**:
- Version: 1.0.0
- Last Updated: 2025-11-13
- Status: Released
- Copyright © 2025 Michael Gardner, A Bit of Help, Inc.
- License: BSD-3-Clause
