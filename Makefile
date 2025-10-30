# =============================================================================
# Functional Library Makefile
# =============================================================================
# Project: functional
# Purpose: Type-safe error handling library for Ada 2022
#
# This Makefile provides:
#   - Build targets (build, clean, rebuild)
#   - Test infrastructure (test, test-coverage)
#   - Format/check targets (format, format-check, stats)
# =============================================================================

PROJECT_NAME := functional

.PHONY: all build build-dev build-opt build-release build-tests check clean \
        deep-clean deps format format-all format-check format-preview \
        format-src format-tests full help install install-tools quick rebuild \
        refresh stats test test-all test-coverage test-run

# =============================================================================
# OS Detection
# =============================================================================

UNAME := $(shell uname -s)

# =============================================================================
# Colors for Output
# =============================================================================

GREEN := \033[0;32m
YELLOW := \033[0;33m
RED := \033[0;31m
BLUE := \033[0;34m
ORANGE := \033[38;5;208m
CYAN := \033[0;36m
BOLD := \033[1m
NC := \033[0m

# =============================================================================
# Tool Paths
# =============================================================================

ALR := alr
GPRBUILD := gprbuild
GNATFORMAT := gnatformat
PYTHON3 := python3

# =============================================================================
# Directories
# =============================================================================

SRC_DIR := src
TESTS_DIR := tests
BUILD_DIR := obj
BIN_DIR := bin
COVERAGE_DIR := coverage

# =============================================================================
# Tool Flags
# =============================================================================
ALR_BUILD_FLAGS := -j8 --no-indirect-imports | grep -E 'warning:|style:|error:' || true

# =============================================================================
# Default Target
# =============================================================================

all: build

# =============================================================================
# Help Target
# =============================================================================

help: ## Display this help message
	@echo "$(ORANGE)$(BOLD)╔══════════════════════════════════════════════════╗$(NC)"
	@echo "$(ORANGE)$(BOLD)║  Functional Library - Ada 2022                   ║$(NC)"
	@echo "$(ORANGE)$(BOLD)╚══════════════════════════════════════════════════╝$(NC)"
	@echo ""
	@echo "$(YELLOW)Build Commands:$(NC)"
	@echo "  build              - Build library (development mode)"
	@echo "  build-dev          - Build with development settings"
	@echo "  build-opt          - Build with optimizations (-O2)"
	@echo "  build-release      - Build production release"
	@echo "  build-tests        - Build test suite only"
	@echo "  clean              - Remove build artifacts"
	@echo "  deep-clean         - Remove all artifacts including cache"
	@echo "  rebuild            - Clean and rebuild"
	@echo "  install            - Install via Alire"
	@echo ""
	@echo "$(YELLOW)Testing Commands:$(NC)"
	@echo "  test               - Run comprehensive test suite"
	@echo "  test-all           - Run all test executables"
	@echo "  test-run           - Run tests without building"
	@echo "  test-coverage      - Run tests with coverage (HTML report)"
	@echo ""
	@echo "$(YELLOW)Quality Commands:$(NC)"
	@echo "  check              - Run static analysis"
	@echo "  format-src         - Auto-format library source code (src/)"
	@echo "  format-tests       - Auto-format test code (tests/)"
	@echo "  format-all         - Auto-format all source code"
	@echo "  format             - Alias for format-all"
	@echo "  format-check       - Check if formatting needed"
	@echo "  format-preview     - Preview formatting changes"
	@echo ""
	@echo "$(YELLOW)Development Commands:$(NC)"
	@echo "  stats              - Display project statistics"
	@echo "  deps               - Display project dependencies"
	@echo "  refresh            - Refresh Alire dependencies"
	@echo ""
	@echo "$(YELLOW)Workflow Shortcuts:$(NC)"
	@echo "  all                - Build library (default)"
	@echo "  quick              - Quick build (skip clean)"
	@echo "  full               - Full build, test, and validation"

# =============================================================================
# Build Commands
# =============================================================================

build:
	@echo "$(GREEN)Building $(PROJECT_NAME) library...$(NC)"
	$(ALR) build -- $(ALR_BUILD_FLAGS)
	@echo "$(GREEN)✓ Build complete$(NC)"

build-dev:
	@echo "$(GREEN)Building $(PROJECT_NAME) (development mode)...$(NC)"
	$(ALR) build --development -- $(ALR_BUILD_FLAGS)
	@echo "$(GREEN)✓ Development build complete$(NC)"

build-opt:
	@echo "$(GREEN)Building $(PROJECT_NAME) (optimized -O2)...$(NC)"
	$(ALR) build -- -O2 $(ALR_BUILD_FLAGS)
	@echo "$(GREEN)✓ Optimized build complete$(NC)"

build-release:
	@echo "$(GREEN)Building $(PROJECT_NAME) (release mode)...$(NC)"
	$(ALR) build --release -- $(ALR_BUILD_FLAGS)
	@echo "$(GREEN)✓ Release build complete$(NC)"

build-tests:
	@echo "$(GREEN)Building test suite...$(NC)"
	@if [ -f "$(TESTS_DIR)/tests.gpr" ]; then \
		cd $(TESTS_DIR) && $(GPRBUILD) -P tests.gpr -p --no-indirect-imports; \
		echo "$(GREEN)✓ Test build complete$(NC)"; \
	else \
		echo "$(YELLOW)No test project found (tests/tests.gpr)$(NC)"; \
	fi

clean:
	@echo "$(YELLOW)Cleaning build artifacts...$(NC)"
	@$(ALR) clean
	@find . -name "*.backup" -delete 2>/dev/null || true
	@echo "$(GREEN)✓ Clean complete$(NC)"

deep-clean:
	@echo "$(YELLOW)Performing deep clean...$(NC)"
	@$(ALR) clean
	@rm -rf $(BUILD_DIR) $(BIN_DIR) lib alire .build $(COVERAGE_DIR)
	@rm -rf $(TESTS_DIR)/obj $(TESTS_DIR)/bin
	@find . -name "*.backup" -delete 2>/dev/null || true
	@find . -name "*.gcda" -o -name "*.gcno" -o -name "*.gcov" | \
	  xargs rm -f 2>/dev/null || true
	@echo "$(GREEN)✓ Deep clean complete$(NC)"

rebuild: clean build

install:
	@echo "$(GREEN)Installing $(PROJECT_NAME)...$(NC)"
	@$(ALR) install
	@echo "$(GREEN)✓ Installation complete$(NC)"

# =============================================================================
# Testing Commands
# =============================================================================

test: build build-tests
	@echo "$(GREEN)Running comprehensive test suite...$(NC)"
	@if [ -f "$(TESTS_DIR)/bin/test_runner" ]; then \
		$(TESTS_DIR)/bin/test_runner; \
		if [ $$? -eq 0 ]; then \
			echo "$(GREEN)✓ All tests passed$(NC)"; \
		else \
			echo "$(RED)✗ Tests failed$(NC)"; \
			exit 1; \
		fi; \
	else \
		echo "$(RED)Test runner not found. Build failed.$(NC)"; \
		exit 1; \
	fi

test-all: build build-tests
	@echo "$(GREEN)Running all test executables...$(NC)"
	@failed=0; \
	if [ -d "$(TESTS_DIR)/bin" ]; then \
		for test in $(TESTS_DIR)/bin/test_*; do \
			if [ -x "$$test" ] && [ -f "$$test" ]; then \
				echo "$(CYAN)Running $$test...$(NC)"; \
				$$test || failed=1; \
				echo ""; \
			fi; \
		done; \
	fi; \
	if [ $$failed -eq 0 ]; then \
		echo "$(GREEN)✓ All test suites passed$(NC)"; \
	else \
		echo "$(RED)✗ Some tests failed$(NC)"; \
		exit 1; \
	fi

test-run:
	@echo "$(GREEN)Running tests (no build)...$(NC)"
	@if [ -f "$(TESTS_DIR)/bin/test_runner" ]; then \
		$(TESTS_DIR)/bin/test_runner; \
	else \
		echo "$(YELLOW)Test runner not found$(NC)"; \
	fi

test-coverage:
	@echo "$(GREEN)Running tests with coverage analysis...$(NC)"
	@if command -v $(PYTHON3) >/dev/null 2>&1; then \
		if [ -f "scripts/run_coverage.py" ]; then \
			$(PYTHON3) scripts/run_coverage.py; \
		else \
			echo "$(YELLOW)Coverage script not found$(NC)"; \
		fi; \
	else \
		echo "$(RED)Python 3 required for coverage analysis$(NC)"; \
	fi

# =============================================================================
# Quality & Code Formatting Commands
# =============================================================================

check:
	@echo "$(GREEN)Running static analysis...$(NC)"
	$(ALR) build
	@echo "$(GREEN)✓ Static analysis complete$(NC)"

format-src:
	@echo "$(GREEN)Formatting library source code (src/)...$(NC)"
	@if command -v $(GNATFORMAT) >/dev/null 2>&1; then \
		if [ -d "$(SRC_DIR)" ]; then \
			find "$(SRC_DIR)" -name "*.ads" -o -name "*.adb" | \
			while read file; do \
				echo "  Formatting $$file..."; \
				$(GNATFORMAT) "$$file" || true; \
			done; \
		fi; \
		echo "$(GREEN)✓ Library source formatting complete$(NC)"; \
	else \
		echo "$(YELLOW)Warning: gnatformat not found$(NC)"; \
		echo "$(YELLOW)Install: alr get --build gnatformat$(NC)"; \
	fi

format-tests:
	@echo "$(GREEN)Formatting test code (tests/)...$(NC)"
	@if command -v $(GNATFORMAT) >/dev/null 2>&1; then \
		if [ -d "$(TESTS_DIR)/src" ]; then \
			cd $(TESTS_DIR) && \
			find src -name "*.ads" -o -name "*.adb" | \
			while read file; do \
				echo "  Formatting $$file..."; \
				$(GNATFORMAT) -P tests.gpr "$$file" || true; \
			done; \
		fi; \
		echo "$(GREEN)✓ Test code formatting complete$(NC)"; \
	else \
		echo "$(YELLOW)Warning: gnatformat not found$(NC)"; \
		echo "$(YELLOW)Install: alr get --build gnatformat$(NC)"; \
	fi

format-all: format-src format-tests
	@echo "$(GREEN)✓ All code formatting complete$(NC)"

format: format-all

format-preview:
	@echo "$(GREEN)Preview formatting changes...$(NC)"
	@echo "$(YELLOW)Preview mode not implemented for gnatformat$(NC)"
	@echo "$(YELLOW)Use git diff to see changes$(NC)"

format-check: format-preview
	@echo ""
	@echo "Run 'make format' to apply formatting"

# =============================================================================
# Development Commands
# =============================================================================

stats:
	@echo "$(BLUE)Project Statistics for $(PROJECT_NAME)$(NC)"
	@echo "$(YELLOW)════════════════════════════════════════$(NC)"
	@echo ""
	@echo "Ada Source Files:"
	@echo "  Library specs:  $$(find $(SRC_DIR) -name "*.ads" 2>/dev/null | wc -l)"
	@echo "  Library bodies: $$(find $(SRC_DIR) -name "*.adb" 2>/dev/null | wc -l)"
	@echo "  Test specs:     $$(find $(TESTS_DIR) -name "*.ads" 2>/dev/null | wc -l)"
	@echo "  Test bodies:    $$(find $(TESTS_DIR) -name "*.adb" 2>/dev/null | wc -l)"
	@echo ""
	@echo "Lines of Code:"
	@find $(SRC_DIR) $(TESTS_DIR) -name "*.ads" -o -name "*.adb" 2>/dev/null | \
	  xargs wc -l 2>/dev/null | tail -1 | awk '{printf "  Total: %d lines\n", $$1}' || \
	  echo "  Total: 0 lines"
	@echo ""
	@echo "Library Artifacts:"
	@if [ -f "./lib/libfunctional.a" ]; then \
		echo "  Library: $$(ls -lh ./lib/libfunctional.a 2>/dev/null | awk '{print $$5}')"; \
	else \
		echo "  No library found (run 'make build')"; \
	fi

deps: ## Display project dependencies
	@echo "$(CYAN)Project dependencies from alire.toml:$(NC)"
	@grep -A 10 "\[\[depends-on\]\]" alire.toml || echo "$(YELLOW)No dependencies found$(NC)"
	@echo ""
	@echo "$(CYAN)Alire dependency tree:$(NC)"
	@$(ALR) show --solve || echo "$(YELLOW)Could not resolve dependencies$(NC)"

refresh: ## Refresh Alire dependencies
	@echo "$(CYAN)Refreshing Alire dependencies...$(NC)"
	@$(ALR) update
	@echo "$(GREEN)✓ Dependencies refreshed$(NC)"

# =============================================================================
# Workflow Shortcuts
# =============================================================================

quick: build ## Quick build (skip clean)
	@echo "$(GREEN)✓ Quick build complete$(NC)"

full: clean build test check ## Full build, test, and validation
	@echo "$(GREEN)✓ Full validation complete$(NC)"

.DEFAULT_GOAL := help
