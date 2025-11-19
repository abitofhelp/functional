#!/usr/bin/env bash
# ===========================================================================
# run_gnatcov.sh - GNATcoverage workflow for tzif project
# ===========================================================================
# Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
# SPDX-License-Identifier: BSD-3-Clause
#
# This script runs the complete GNATcoverage source-trace workflow:
#   1. Instrument test projects
#   2. Build instrumented tests
#   3. Run tests to collect .srctrace files
#   4. Generate coverage reports (HTML + text summary)
#
# Requirements:
#   - gnatcov in PATH
#   - gnatcov_rts runtime built and installed (done in tools/)
#
# Usage:
#   ./scripts/run_gnatcov.sh
# ===========================================================================

set -euo pipefail

# Project paths
ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
RUNTIME="$ROOT/tools/gnatcov_rts_install/share/gpr"
TRACES="$ROOT/coverage/traces"
REPORT="$ROOT/coverage/report"

# Configuration
COVERAGE_LEVEL="stmt+decision"
UNIT_TESTS_GPR="$ROOT/test/unit/unit_tests.gpr"
INTEGRATION_TESTS_GPR="$ROOT/test/integration/integration_tests.gpr"

# Colors for output
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

echo -e "${BLUE}========================================${NC}"
echo -e "${BLUE}  GNATcoverage for tzif${NC}"
echo -e "${BLUE}========================================${NC}"

# Setup directories
mkdir -p "$TRACES" "$REPORT"
mkdir -p "$ROOT/test/bin" "$ROOT/obj/test/unit" "$ROOT/obj/test/integration"
rm -f "$TRACES"/*.srctrace 2>/dev/null || true

# Export runtime path for gprbuild
export GPR_PROJECT_PATH="$RUNTIME"
export GNATCOV_TRACE_FILE="$TRACES/"

echo ""
echo -e "${YELLOW}Step 1: Instrumenting unit tests...${NC}"
gnatcov instrument \
  -P "$UNIT_TESTS_GPR" \
  --level="$COVERAGE_LEVEL" \
  --dump-trigger=atexit \
  --dump-channel=bin-file \
  --externally-built-projects 2>&1 | grep -v "warning:" || true

echo ""
echo -e "${YELLOW}Step 2: Instrumenting integration tests...${NC}"
gnatcov instrument \
  -P "$INTEGRATION_TESTS_GPR" \
  --level="$COVERAGE_LEVEL" \
  --dump-trigger=atexit \
  --dump-channel=bin-file \
  --externally-built-projects 2>&1 | grep -v "warning:" || true

echo ""
echo -e "${YELLOW}Step 3: Building instrumented unit tests...${NC}"
alr exec -- gprbuild -f -p \
  -P "$UNIT_TESTS_GPR" \
  --src-subdirs=gnatcov-instr \
  --implicit-with=gnatcov_rts_full.gpr

echo ""
echo -e "${YELLOW}Step 4: Building instrumented integration tests...${NC}"
alr exec -- gprbuild -f -p \
  -P "$INTEGRATION_TESTS_GPR" \
  --src-subdirs=gnatcov-instr \
  --implicit-with=gnatcov_rts_full.gpr

echo ""
echo -e "${YELLOW}Step 5: Running unit tests to collect traces...${NC}"
"$ROOT/test/bin/unit_runner" || {
  echo -e "${YELLOW}Warning: Some unit tests failed, but continuing with coverage...${NC}"
}

echo ""
echo -e "${YELLOW}Step 6: Running integration tests to collect traces...${NC}"
"$ROOT/test/bin/integration_runner" || {
  echo -e "${YELLOW}Warning: Some integration tests failed, but continuing with coverage...${NC}"
}

echo ""
echo -e "${YELLOW}Step 7: Counting trace files...${NC}"
TRACE_COUNT=$(find "$TRACES" -name "*.srctrace" | wc -l)
echo "Found $TRACE_COUNT trace file(s)"

if [ "$TRACE_COUNT" -eq 0 ]; then
  echo -e "${YELLOW}ERROR: No trace files generated!${NC}"
  exit 1
fi

echo ""
echo -e "${YELLOW}Step 8: Generating coverage report (HTML + summary)...${NC}"
gnatcov coverage \
  --level="$COVERAGE_LEVEL" \
  -P "$ROOT/src/domain/domain.gpr" \
  --annotate=dhtml \
  --annotate=report \
  --output-dir="$REPORT" \
  --ignore-source-files="test/**" \
  "$TRACES"/*.srctrace

echo ""
echo -e "${GREEN}========================================${NC}"
echo -e "${GREEN}  Coverage Analysis Complete!${NC}"
echo -e "${GREEN}========================================${NC}"
echo ""
echo "HTML Report:  $REPORT/index.html"
echo "Text Report:  $REPORT/index.txt"
echo ""
echo "Opening HTML report in browser..."
open "$REPORT/index.html" 2>/dev/null || {
  echo "To view: open $REPORT/index.html"
}
