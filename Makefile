# =============================================================================
# Functional Library Makefile
# =============================================================================
# Project: functional
# Purpose: Type-safe error handling library for Ada 2022
#
# This Makefile provides:
#   - Build targets (build, clean, rebuild)
#   - Test infrastructure (test, test-coverage)
#   - Quality/check targets (check, check-arch, stats)
# =============================================================================

PROJECT_NAME := functional

.PHONY: all build build-dev build-opt build-release build-tests check check-arch \
        clean clean-clutter clean-coverage clean-deep compress deps \
		help prereqs rebuild refresh stats test test-all test-coverage \
		test-unit test-integration test-e2e test-python \
		install-tools build-coverage-runtime
# FIX: ENABLE AFTER THE TARGETS CONVERT TO USING OUR ADAFMT TOOL, WHICH IS IN DEVELOPMENT.
#       format format-all format-src format-tests

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
# Tool Flags
# =============================================================================
ALR_BUILD_FLAGS := -j8 | grep -E 'warning:|(style)|error:' || true
ALR_TEST_FLAGS  := -j8 | grep -E 'warning:|(style)|error:' || true

# =============================================================================
# Directories
# =============================================================================

SRC_DIR := src
TEST_DIR := test
BUILD_DIR := obj
BIN_DIR := bin
COVERAGE_DIR := coverage

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
	@echo "  build-tests        - Build test suite"
	@echo "  clean              - Clean build artifacts"
	@echo "  clean-clutter      - Remove temporary files and backups"
	@echo "  clean-coverage     - Clean coverage data"
	@echo "  clean-deep         - Deep clean (includes Alire cache)"
	@echo "  compress           - Create compressed source archive (tar.gz)"
	@echo "  rebuild            - Clean and rebuild"
	@echo ""
	@echo "$(YELLOW)Testing Commands:$(NC)"
	@echo "  test               - Run comprehensive test suite"
	@echo "  test-all           - Run all test executables"
	@echo "  test-unit          - Run unit tests only"
	@echo "  test-integration   - Run integration tests only"
	@echo "  test-e2e           - Run E2E tests only"
	@echo "  test-python        - Run Python script tests (arch_guard.py validation)"
	@echo "  test-coverage      - Run tests with coverage analysis"
	@echo ""
	@echo "$(YELLOW)Quality & Architecture Commands:$(NC)"
	@echo "  check              - Run static analysis"
	@echo "  check-arch         - Validate architecture boundaries"
# FIX: ENABLE AFTER THE TARGETS CONVERT TO USING OUR ADAFMT TOOL, WHICH IS IN DEVELOPMENT.
# 	@echo "  format-src         - Auto-format source code only"
# 	@echo "  format-tests       - Auto-format test code only"
# 	@echo "  format-all         - Auto-format all code"
# 	@echo "  format             - Alias for format-all"
	@echo "  stats              - Display project statistics"
	@echo ""
	@echo "$(YELLOW)Utility Commands:$(NC)"
	@echo "  deps                    - Show dependency information"
	@echo "  prereqs                 - Verify prerequisites are satisfied"
	@echo "  refresh                 - Refresh Alire dependencies"
	@echo "  install-tools           - Install development tools (GMP, gcovr, gnatformat)"
	@echo "  build-coverage-runtime  - Build GNATcoverage runtime library"
	@echo ""
	@echo "$(YELLOW)Workflow Shortcuts:$(NC)"
	@echo "  all                - Build library (default)"
	@echo ""
	@echo "$(YELLOW)Submodule Commands:$(NC)"
	@echo "  submodule-init     - Initialize submodules after fresh clone"
	@echo "  submodule-update   - Pull latest from all submodule repos"
	@echo "  submodule-status   - Show submodule commit status"

# =============================================================================
# Build Commands
# =============================================================================

prereqs:
	@echo "$(GREEN)✓ All prerequisites satisfied$(NC)"

build: build-dev

build-dev: check-arch prereqs
	@echo "$(GREEN)Building $(PROJECT_NAME) (development mode)...$(NC)"
	$(ALR) build --development -- $(ALR_BUILD_FLAGS)
	@echo "$(GREEN)✓ Development build complete$(NC)"

build-opt: check-arch prereqs
	@echo "$(GREEN)Building $(PROJECT_NAME) (optimized -O2)...$(NC)"
	$(ALR) build -- -O2 $(ALR_BUILD_FLAGS)
	@echo "$(GREEN)✓ Optimized build complete$(NC)"

build-release: check-arch prereqs
	@echo "$(GREEN)Building $(PROJECT_NAME) (release mode)...$(NC)"
	$(ALR) build --release -- $(ALR_BUILD_FLAGS)
	@echo "$(GREEN)✓ Release build complete$(NC)"

build-tests: check-arch prereqs
	@echo "$(GREEN)Building test suites...$(NC)"
	@if [ -f "$(TEST_DIR)/unit/unit_tests.gpr" ]; then \
		$(ALR) exec -- $(GPRBUILD) -P $(TEST_DIR)/unit/unit_tests.gpr -p $(ALR_TEST_FLAGS); \
		echo "$(GREEN)✓ Unit tests built$(NC)"; \
	else \
		echo "$(YELLOW)Unit test project not found$(NC)"; \
	fi

clean:
	@echo "$(YELLOW)Cleaning project build artifacts (keeps dependencies)...$(NC)"
	@# Use gprclean WITHOUT -r to clean only our project, not dependencies
	@$(ALR) exec -- gprclean -P $(PROJECT_NAME).gpr -q 2>/dev/null || true
	@$(ALR) exec -- gprclean -P $(TEST_DIR)/unit/unit_tests.gpr -q 2>/dev/null || true
	@rm -rf $(BUILD_DIR) $(BIN_DIR) lib $(TEST_DIR)/bin $(TEST_DIR)/obj
	@find . -name "*.backup" -delete 2>/dev/null || true
	@echo "$(GREEN)✓ Project artifacts cleaned (dependencies preserved for fast rebuild)$(NC)"

clean-deep:
	@echo "$(YELLOW)Deep cleaning ALL artifacts including dependencies...$(NC)"
	@echo "$(YELLOW)⚠️  This will require rebuilding all dependencies (slow!)$(NC)"
	@$(ALR) clean
	@rm -rf $(BUILD_DIR) $(BIN_DIR) lib $(TEST_DIR)/bin $(TEST_DIR)/obj
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

clean-clutter: ## Remove temporary files, backups, and clutter
	@echo "$(CYAN)Cleaning temporary files and clutter...$(NC)"
	@$(PYTHON3) scripts/python/makefile/cleanup_temp_files.py
	@echo "$(GREEN)✓ Temporary files removed$(NC)"

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

# =============================================================================
# Testing Commands
# =============================================================================

test: test-all

test-all: build build-tests
	@echo "$(GREEN)Running all test executables...$(NC)"
	@failed=0; \
	if [ -d "$(TEST_DIR)/bin" ]; then \
		for test in $(TEST_DIR)/bin/*_runner; do \
			if [ -x "$$test" ] && [ -f "$$test" ]; then \
				echo "$(CYAN)Running $$test...$(NC)"; \
				$$test || failed=1; \
				echo ""; \
			fi; \
		done; \
	else \
		echo "$(YELLOW)No test executables found in $(TEST_DIR)/bin$(NC)"; \
	fi; \
	if [ $$failed -eq 0 ]; then \
		echo ""; \
		echo "\033[1;92m########################################"; \
		echo "###                                  ###"; \
		echo "###   ALL TEST SUITES: SUCCESS      ###"; \
		echo "###   All tests passed!              ###"; \
		echo "###                                  ###"; \
		echo "########################################\033[0m"; \
		echo ""; \
	else \
		echo ""; \
		echo "\033[1;91m########################################"; \
		echo "###                                  ###"; \
		echo "###   ALL TEST SUITES: FAILURE      ###"; \
		echo "###   Some tests failed!             ###"; \
		echo "###                                  ###"; \
		echo "########################################\033[0m"; \
		echo ""; \
		exit 1; \
	fi

test-unit: build build-tests
	@echo "$(GREEN)Running unit tests...$(NC)"
	@if [ -f "$(TEST_DIR)/bin/unit_runner" ]; then \
		$(TEST_DIR)/bin/unit_runner; \
		if [ $$? -eq 0 ]; then \
			echo "$(GREEN)✓ Unit tests passed$(NC)"; \
		else \
			echo "$(RED)✗ Unit tests failed$(NC)"; \
			exit 1; \
		fi; \
	else \
		echo "$(YELLOW)Unit test runner not found at $(TEST_DIR)/bin/unit_runner$(NC)"; \
		exit 1; \
	fi

test-integration: build build-tests
	@echo "$(GREEN)Running integration tests...$(NC)"
	@echo "$(YELLOW)No integration tests defined for functional library$(NC)"

test-e2e: build build-tests
	@echo "$(GREEN)Running E2E tests...$(NC)"
	@echo "$(YELLOW)No E2E tests defined for functional library$(NC)"

test-coverage: clean build build-coverage-runtime
	@echo "$(GREEN)Running tests with GNATcoverage analysis...$(NC)"
	@if [ -f "scripts/python/makefile/coverage.sh" ]; then \
		bash scripts/python/makefile/coverage.sh; \
	else \
		echo "$(YELLOW)Coverage script not found at scripts/python/makefile/coverage.sh$(NC)"; \
		exit 1; \
	fi

test-python: ## Run Python script tests (arch_guard.py validation)
	@echo "$(GREEN)Running Python script tests...$(NC)"
	@cd test/python && $(PYTHON3) -m pytest -v
	@echo "$(GREEN)✓ Python tests complete$(NC)"

# =============================================================================
# Quality & Code Formatting Commands
# =============================================================================

check:
	@echo "$(GREEN)Running code checks...$(NC)"
	@$(ALR) build --development -- $(ALR_BUILD_FLAGS)
	@echo "$(GREEN)✓ Code checks complete$(NC)"

check-arch: ## Validate architecture boundaries
	@echo "$(GREEN)Validating architecture boundaries...$(NC)"
	-@PYTHONPATH=scripts/python $(PYTHON3) -m arch_guard
	@echo "$(YELLOW)⚠ Architecture validation complete (violations are warnings, not errors)$(NC)"

# FIXME: REPLACE WITH THE ADAFMT TOOL WE ARE CREATING WHEN IT IS COMPLETED.
# THE CURRENT SCRIPT IS COMMENTING COMMENTS AND MESSING UP WITH INDEXED COMMENTS.
# format-src:
# 	@echo "$(GREEN)Formatting source code...$(NC)"
# 	@if [ ! -f "scripts/python/makefile/ada_formatter_pipeline.donotuse.py" ]; then \
# 		echo "$(RED)Error: scripts/python/makefile/ada_formatter_pipeline.donotuse.py not found$(NC)"; \
# 		exit 1; \
# 	fi
# 	@for dir in $(SRC_DIR); do \
# 		if [ -d "$$dir" ]; then \
# 			find "$$dir" -name "*.ads" -o -name "*.adb" | \
# 			while read file; do \
# 				echo "  Formatting $$file..."; \
# 				$(PYTHON3) scripts/python/makefile/ada_formatter_pipeline.donotuse.py "$(PWD)/$(PROJECT_NAME).gpr" --include-path "$(PWD)/$$file" || true; \
# 			done; \
# 		fi; \
# 	done
# 	@echo "$(GREEN)✓ Source formatting complete$(NC)"

# format-tests:
# 	@echo "$(GREEN)Formatting test code...$(NC)"
# 	@if [ ! -f "scripts/python/makefile/ada_formatter_pipeline.donotuse.py" ]; then \
# 		echo "$(RED)Error: scripts/python/makefile/ada_formatter_pipeline.donotuse.py not found$(NC)"; \
# 		exit 1; \
# 	fi
# 	@if [ -d "$(TEST_DIR)" ] && [ -f "$(TEST_DIR)/tests.gpr" ]; then \
# 		find $(TEST_DIR) -name "*.ads" -o -name "*.adb" | \
# 		while read file; do \
# 			echo "  Formatting $$file..."; \
# 			$(PYTHON3) scripts/python/makefile/ada_formatter_pipeline.donotuse.py "$(PWD)/$(TEST_DIR)/tests.gpr" --include-path "$(PWD)/$$file" || true; \
# 		done; \
# 		echo "$(GREEN)✓ Test formatting complete$(NC)"; \
# 	fi

# format-all: format-src format-tests
# 	@echo "$(GREEN)✓ All code formatting complete$(NC)"

# format: format-all

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
	@echo "  Test specs:     $$(find $(TEST_DIR) -name "*.ads" 2>/dev/null | wc -l | tr -d ' ')"
	@echo "  Test bodies:    $$(find $(TEST_DIR) -name "*.adb" 2>/dev/null | wc -l | tr -d ' ')"
	@echo ""
	@echo "Lines of Code:"
	@find $(SRC_DIR) $(TEST_DIR) -name "*.ads" -o -name "*.adb" 2>/dev/null | \
	  xargs wc -l 2>/dev/null | tail -1 | awk '{printf "  Total: %d lines\n", $$1}' || \
	  echo "  Total: 0 lines"
	@echo ""
	@echo "Build Artifacts:"
	@if [ -f "./lib/libfunctional.a" ]; then \
		echo "  Library: $$(ls -lh ./lib/libfunctional.a 2>/dev/null | awk '{print $$5}')"; \
	else \
		echo "  No library found (run 'make build')"; \
	fi

# =============================================================================
# Advanced Targets
# =============================================================================

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

install-tools: ## Install development tools (GMP, gcovr, gnatformat)
	@echo "$(CYAN)Installing development tools...$(NC)"
	@$(PYTHON3) scripts/python/makefile/install_tools.py
	@echo "$(GREEN)✓ Tool installation complete$(NC)"

build-coverage-runtime: ## Build GNATcoverage runtime library
	@echo "$(CYAN)Building GNATcoverage runtime...$(NC)"
	@$(PYTHON3) scripts/python/makefile/build_gnatcov_runtime.py

.DEFAULT_GOAL := help

## ---------------------------------------------------------------------------
## Submodule Management
## ---------------------------------------------------------------------------

.PHONY: submodule-update submodule-status submodule-init

submodule-init: ## Initialize submodules after fresh clone
	git submodule update --init --recursive

submodule-update: ## Pull latest from all submodule repos
	git submodule update --remote --merge
	@echo ""
	@echo "Submodules updated. Review changes, then run:"
	@echo "  git add docs/common scripts/python test/python"
	@echo "  git commit -m 'chore: update submodules'"
	@echo "  git push"

submodule-status: ## Show submodule commit status
	git submodule status
