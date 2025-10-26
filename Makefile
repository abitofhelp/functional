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

.PHONY: all build clean deep-clean rebuild test test-all test-coverage check format \
        format-src format-tests format-all format-check format-preview stats help

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
GNATFORMAT := gnatformat
PYTHON3 := python3

# =============================================================================
# Directories
# =============================================================================

SRC_DIR := src
TESTS_DIR := tests
BUILD_DIR := obj
COVERAGE_DIR := coverage

# Directories to format (library src + tests)
FORMAT_DIRS := $(wildcard $(SRC_DIR)) $(wildcard $(TESTS_DIR))

# =============================================================================
# Default Target
# =============================================================================

all: build

# =============================================================================
# Help Target
# =============================================================================

help:
	@echo "$(ORANGE)$(BOLD)╔══════════════════════════════════════════════════╗$(NC)"
	@echo "$(ORANGE)$(BOLD)║  Functional Library - Ada 2022$(NC)"
	@echo "$(ORANGE)$(BOLD)╚══════════════════════════════════════════════════╝$(NC)"
	@echo ""
	@echo "$(YELLOW)Build Commands:$(NC)"
	@echo "  build         - Build functional library"
	@echo "  clean         - Remove build artifacts"
	@echo "  deep-clean    - Remove all artifacts including cache"
	@echo "  rebuild       - Clean and rebuild"
	@echo ""
	@echo "$(YELLOW)Testing Commands:$(NC)"
	@echo "  test          - Run comprehensive test suite"
	@echo "  test-all      - Run all individual test executables"
	@echo "  test-coverage - Run tests with coverage (HTML report)"
	@echo ""
	@echo "$(YELLOW)Quality Commands:$(NC)"
	@echo "  check         - Run static analysis"
	@echo "  format-src    - Auto-format library source code (src/)"
	@echo "  format-tests  - Auto-format test code (tests/)"
	@echo "  format-all    - Auto-format all source code"
	@echo "  format        - Alias for format-all"
	@echo "  format-check  - Check if formatting needed"
	@echo "  format-preview- Preview formatting changes"
	@echo ""
	@echo "$(YELLOW)Development Commands:$(NC)"
	@echo "  stats         - Display project statistics"
	@echo "  all           - Build library (default)"

# =============================================================================
# Build Commands
# =============================================================================

build:
	@echo "$(GREEN)Building $(PROJECT_NAME) library...$(NC)"
	$(ALR) build -- -j8
	@echo "$(GREEN)✓ Build complete$(NC)"

clean:
	@echo "$(YELLOW)Cleaning build artifacts...$(NC)"
	@$(ALR) clean
	@find . -name "*.backup" -delete 2>/dev/null || true
	@echo "$(GREEN)✓ Clean complete$(NC)"

deep-clean:
	@echo "$(YELLOW)Performing deep clean...$(NC)"
	@$(ALR) clean
	@rm -rf $(BUILD_DIR) lib alire .build $(COVERAGE_DIR)
	@find . -name "*.backup" -delete 2>/dev/null || true
	@find . -name "*.gcda" -o -name "*.gcno" -o -name "*.gcov" | \
	  xargs rm -f 2>/dev/null || true
	@echo "$(GREEN)✓ Deep clean complete$(NC)"

rebuild: clean build

# =============================================================================
# Testing Commands
# =============================================================================

test: build
	@echo "$(GREEN)Building and running comprehensive test suite...$(NC)"
	@rm -rf tests/obj tests/bin
	cd tests && gprbuild -P tests.gpr -p
	@if [ -f "tests/bin/test_runner" ]; then \
		tests/bin/test_runner; \
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

test-all: build
	@echo "$(GREEN)Running all test executables...$(NC)"
	@rm -rf tests/obj tests/bin
	cd tests && gprbuild -P tests.gpr -p
	@failed=0; \
	for test in tests/bin/test_*; do \
		if [ -x "$$test" ] && [ -f "$$test" ]; then \
			echo "$(CYAN)Running $$test...$(NC)"; \
			$$test || failed=1; \
			echo ""; \
		fi; \
	done; \
	if [ $$failed -eq 0 ]; then \
		echo "$(GREEN)✓ All test suites passed$(NC)"; \
	else \
		echo "$(RED)✗ Some tests failed$(NC)"; \
		exit 1; \
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
# Statistics
# =============================================================================

stats:
	@echo "$(BLUE)Project Statistics for $(PROJECT_NAME)$(NC)"
	@echo "$(YELLOW)════════════════════════════════════════$(NC)"
	@echo ""
	@echo "Ada Source Files:"
	@echo "  Source specs:  $$(find $(SRC_DIR) -name "*.ads" 2>/dev/null | \
	  wc -l)"
	@echo "  Source bodies: $$(find $(SRC_DIR) -name "*.adb" 2>/dev/null | \
	  wc -l)"
	@echo "  Test specs:    $$(find $(TESTS_DIR) -name "*.ads" 2>/dev/null | \
	  wc -l)"
	@echo "  Test bodies:   $$(find $(TESTS_DIR) -name "*.adb" 2>/dev/null | \
	  wc -l)"
	@echo ""
	@echo "Lines of Code:"
	@find $(SRC_DIR) $(TESTS_DIR) -name "*.ads" -o -name "*.adb" 2>/dev/null | xargs wc -l 2>/dev/null | tail -1 | awk '{printf "  Total: %d lines\n", $$1}' || echo "  Total: 0 lines"
	@echo ""
	@echo "Library Artifacts:"
	@if [ -f "./lib/libfunctional.a" ]; then \
		echo "  Library: $$(ls -lh ./lib/libfunctional.a 2>/dev/null | awk '{print $$5}')"; \
	else \
		echo "  No library found (run 'make build')"; \
	fi

.DEFAULT_GOAL := help
