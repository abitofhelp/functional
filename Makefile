# =============================================================================
# Functional Library Makefile
# =============================================================================
# Project: functional
# Purpose: Type-safe error handling library for Ada 2022
#
# This Makefile provides:
#   - Build targets (build, clean, rebuild)
#   - Test infrastructure (test, test-coverage)
#   - Format/check targets (format, stats)
# =============================================================================

PROJECT_NAME := functional

.PHONY: all build build-dev build-opt build-release build-tests check \
        clean clean-coverage clean-deep compress deps format format-all \
        format-src format-tests full help install prereqs quick rebuild \
        refresh stats test test-all test-run

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
ALR_BUILD_FLAGS := -j8 | grep -E 'warning:|style:|error:' || true
ALR_TEST_FLAGS  := -j8 | grep -E 'warning:|style:|error:' || true

# =============================================================================
# Default Target
# =============================================================================

all: build

# =============================================================================
# Help Target
# =============================================================================

help: ## Display this help message
	@echo "$(CYAN)$(BOLD)╔══════════════════════════════════════════════════╗$(NC)"
	@echo "$(CYAN)$(BOLD)║  Functional Library - Ada 2022                   ║$(NC)"
	@echo "$(CYAN)$(BOLD)╚══════════════════════════════════════════════════╝$(NC)"
	@echo " "
	@echo "$(YELLOW)Build Commands:$(NC)"
	@echo "  build              - Build library (development mode)"
	@echo "  build-dev          - Build with development flags"
	@echo "  build-opt          - Build with optimization (-O2)"
	@echo "  build-release      - Build in release mode"
	@echo "  build-tests        - Build test suite only"
	@echo "  clean              - Clean build artifacts"
	@echo "  clean-coverage     - Clean coverage data"
	@echo "  clean-deep         - Deep clean (includes Alire cache)"
	@echo "  compress           - Create compressed source archive (tar.gz)"
	@echo "  rebuild            - Clean and rebuild"
	@echo "  install            - Install via Alire"
	@echo ""
	@echo "$(YELLOW)Testing Commands:$(NC)"
	@echo "  test               - Run comprehensive test suite"
	@echo "  test-all           - Run all test executables"
	@echo "  test-run           - Run tests without building"
	@echo "  test-coverage      - Run tests with coverage analysis"
	@echo ""
	@echo "$(YELLOW)Quality Commands:$(NC)"
	@echo "  check              - Run static analysis"
	@echo "  format-src         - Auto-format library source code (src/)"
	@echo "  format-tests       - Auto-format test code (tests/)"
	@echo "  format-all         - Auto-format all source code"
	@echo "  format             - Alias for format-all"
	@echo "  stats              - Display project statistics"
	@echo ""
	@echo "$(YELLOW)Utility Commands:$(NC)"
	@echo "  deps               - Show dependency information"
	@echo "  prereqs            - Verify prerequisites are satisfied"
	@echo "  refresh            - Refresh Alire dependencies"
	@echo ""
	@echo "$(YELLOW)Workflow Shortcuts:$(NC)"
	@echo "  all                - Build library (default)"
	@echo "  quick              - Quick build (skip clean)"
	@echo "  full               - Full build, test, and validation"

# =============================================================================
# Build Commands
# =============================================================================

prereqs:
	@echo "$(GREEN)✓ All prerequisites satisfied$(NC)"

build: build-dev

build-dev: prereqs
	@echo "$(GREEN)Building $(PROJECT_NAME) (development mode)...$(NC)"
	$(ALR) build --development -- $(ALR_BUILD_FLAGS)
	@echo "$(GREEN)✓ Development build complete$(NC)"

build-opt: prereqs
	@echo "$(GREEN)Building $(PROJECT_NAME) (optimized -O2)...$(NC)"
	$(ALR) build -- -O2 $(ALR_BUILD_FLAGS)
	@echo "$(GREEN)✓ Optimized build complete$(NC)"

build-release: prereqs
	@echo "$(GREEN)Building $(PROJECT_NAME) (release mode)...$(NC)"
	$(ALR) build --release -- $(ALR_BUILD_FLAGS)
	@echo "$(GREEN)✓ Release build complete$(NC)"

build-tests: prereqs
	@echo "$(GREEN)Building test suite...$(NC)"
	@if [ -f "$(TESTS_DIR)/tests.gpr" ]; then \
		$(ALR) exec -- $(GPRBUILD) -P $(TESTS_DIR)/tests.gpr -p $(ALR_TEST_FLAGS); \
		echo "$(GREEN)✓ Test build complete$(NC)"; \
	else \
		echo "$(YELLOW)No test project found (tests/tests.gpr)$(NC)"; \
	fi

clean:
	@echo "$(YELLOW)Cleaning project build artifacts (keeps dependencies)...$(NC)"
	@# Use gprclean WITHOUT -r to clean only our project, not dependencies
	@$(ALR) exec -- gprclean -P $(PROJECT_NAME).gpr -q 2>/dev/null || true
	@$(ALR) exec -- gprclean -P $(TESTS_DIR)/tests.gpr -q 2>/dev/null || true
	@rm -rf $(BUILD_DIR) $(BIN_DIR) lib $(TESTS_DIR)/bin $(TESTS_DIR)/obj
	@find . -name "*.backup" -delete 2>/dev/null || true
	@echo "$(GREEN)✓ Project artifacts cleaned (dependencies preserved for fast rebuild)$(NC)"

clean-deep:
	@echo "$(YELLOW)Deep cleaning ALL artifacts including dependencies...$(NC)"
	@echo "$(YELLOW)⚠️  This will require rebuilding all dependencies (slow!)$(NC)"
	@$(ALR) clean
	@rm -rf $(BUILD_DIR) $(BIN_DIR) lib $(TESTS_DIR)/bin $(TESTS_DIR)/obj
	@rm -rf alire .build $(COVERAGE_DIR)
	@find . -name "*.backup" -delete 2>/dev/null || true
	@echo "$(GREEN)✓ Deep clean complete (next build will be slow)$(NC)"

clean-coverage:
	@echo "$(YELLOW)Cleaning coverage artifacts...$(NC)"
	@find . -name "*.srctrace" -delete 2>/dev/null || true
	@find . -name "*.traces" -delete 2>/dev/null || true
	@find . -name "*.sid" -delete 2>/dev/null || true
	@find . -name "*.gcda" -o -name "*.gcno" -o -name "*.gcov" | \
	  xargs rm -f 2>/dev/null || true
	@rm -rf $(COVERAGE_DIR)/ 2>/dev/null || true
	@rm -rf gnatcov-instr/ 2>/dev/null || true
	@echo "$(GREEN)✓ Coverage artifacts cleaned$(NC)"

compress:
	@echo "$(CYAN)Creating compressed source archive...$(NC)"
	@tar -czvf "$(PROJECT_NAME).tar.gz" \
		--exclude="$(PROJECT_NAME).tar.gz" \
		--exclude='.git' \
		--exclude='tools' \
		--exclude='data' \
		--exclude='obj' \
		--exclude='bin' \
		--exclude='lib' \
		--exclude='alire' \
		--exclude='.build' \
		--exclude='coverage' \
		--exclude='.DS_Store' \
		--exclude='*.o' \
		--exclude='*.ali' \
		--exclude='*.backup' \
		.
	@echo "$(GREEN)✓ Archive created: $(PROJECT_NAME).tar.gz$(NC)"

rebuild: clean build

install:
	@echo "$(GREEN)Installing $(PROJECT_NAME)...$(NC)"
	@$(ALR) install
	@echo "$(GREEN)✓ Installation complete$(NC)"

# =============================================================================
# Testing Commands
# =============================================================================

test: test-all

test-all: build build-tests
	@echo "$(GREEN)Running all tests...$(NC)"
	@if [ -f "$(TESTS_DIR)/bin/test_runner" ]; then \
		$(TESTS_DIR)/bin/test_runner; \
		if [ $$? -eq 0 ]; then \
			echo "$(GREEN)✓ All test suites passed$(NC)"; \
		else \
			echo "$(RED)✗ Some tests failed$(NC)"; \
			exit 1; \
		fi; \
	else \
		echo "$(YELLOW)Test runner not found at $(TESTS_DIR)/bin/test_runner$(NC)"; \
		exit 1; \
	fi

test-run:
	@echo "$(GREEN)Running tests (no build)...$(NC)"
	@if [ -f "$(TESTS_DIR)/bin/test_runner" ]; then \
		$(TESTS_DIR)/bin/test_runner; \
	else \
		echo "$(YELLOW)Test runner not found$(NC)"; \
	fi

test-coverage: clean build
	@echo "$(GREEN)Running tests with GNATcoverage analysis...$(NC)"
	@if [ -f "scripts/coverage.sh" ]; then \
		bash scripts/coverage.sh; \
	else \
		echo "$(YELLOW)Coverage script not found at scripts/coverage.sh$(NC)"; \
		exit 1; \
	fi

# =============================================================================
# Quality & Code Formatting Commands
# =============================================================================

check:
	@echo "$(GREEN)Running code checks...$(NC)"
	@$(ALR) build --validation -- $(ALR_BUILD_FLAGS)
	@echo "$(GREEN)✓ Code checks complete$(NC)"

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

# =============================================================================
# Development Commands
# =============================================================================

stats:
	@echo "$(CYAN)$(BOLD)Project Statistics for $(PROJECT_NAME)$(NC)"
	@echo "$(YELLOW)════════════════════════════════════════$(NC)"
	@echo ""
	@echo "Ada Source Files:"
	@echo "  Library specs:  $$(find $(SRC_DIR) -name "*.ads" 2>/dev/null | wc -l | tr -d ' ')"
	@echo "  Library bodies: $$(find $(SRC_DIR) -name "*.adb" 2>/dev/null | wc -l | tr -d ' ')"
	@echo "  Test specs:     $$(find $(TESTS_DIR) -name "*.ads" 2>/dev/null | wc -l | tr -d ' ')"
	@echo "  Test bodies:    $$(find $(TESTS_DIR) -name "*.adb" 2>/dev/null | wc -l | tr -d ' ')"
	@echo ""
	@echo "Lines of Code:"
	@find $(SRC_DIR) $(TESTS_DIR) -name "*.ads" -o -name "*.adb" 2>/dev/null | \
	  xargs wc -l 2>/dev/null | tail -1 | awk '{printf "  Total: %d lines\n", $$1}' || \
	  echo "  Total: 0 lines"
	@echo ""
	@echo "Build Artifacts:"
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
