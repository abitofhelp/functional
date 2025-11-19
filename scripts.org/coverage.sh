#!/usr/bin/env bash
# GNATcoverage end-to-end (macOS-friendly, source-trace workflow)
# Adapted for tzif project with separate unit/integration test projects
set -euo pipefail

ROOT="$(git rev-parse --show-toplevel 2>/dev/null || pwd)"
UNIT_TESTS_GPR="${UNIT_TESTS_GPR:-$ROOT/test/unit/unit_tests.gpr}"
INTEGRATION_TESTS_GPR="${INTEGRATION_TESTS_GPR:-$ROOT/test/integration/integration_tests.gpr}"
DOMAIN_GPR="$ROOT/src/domain/domain.gpr"

# 0) Check tools
# Note: We use 'alr exec -- gnatcov' so we don't need gnatcov in PATH
if ! command -v gprbuild >/dev/null 2>&1; then
  echo "ERROR: gprbuild not found on PATH." >&2
  exit 1
fi

# 1) Locate GNATcoverage runtime - use install prefix per AdaCore docs
GNATCOV_RTS_PREFIX="$ROOT/tools/gnatcov_rts_install"
if [ ! -d "$GNATCOV_RTS_PREFIX" ]; then
  echo "ERROR: Cannot find gnatcov_rts_install under $GNATCOV_RTS_PREFIX" >&2
  echo "Run: make test-coverage to build the runtime first." >&2
  exit 1
fi

export GPR_PROJECT_PATH="$GNATCOV_RTS_PREFIX${GPR_PROJECT_PATH:+:$GPR_PROJECT_PATH}"

TRACES="$ROOT/coverage/traces"
REPORT="$ROOT/coverage/report"
mkdir -p "$TRACES" "$REPORT"

# (Optional) Force trace output dir
export GNATCOV_TRACE_FILE="$TRACES/"

# 2) Clean previous coverage instrumentation
rm -rf "$ROOT/gnatcov-instr" 2>/dev/null || true

# 3) Instrument the tests (source-trace, write at exit)
# Important: Do NOT use --externally-built-projects so all units get instrumented together
echo "==> Instrumenting unit tests (project source only)..."
alr exec -- gnatcov instrument -P "$UNIT_TESTS_GPR" \
  --level=stmt+decision \
  --projects=unit_tests.gpr \
  --no-subprojects \
  --dump-trigger=atexit --dump-channel=bin-file \
  2>&1 | grep -v "warning:" || true

echo "==> Instrumenting integration tests (project source only)..."
alr exec -- gnatcov instrument -P "$INTEGRATION_TESTS_GPR" \
  --level=stmt+decision \
  --projects=integration_tests.gpr \
  --no-subprojects \
  --dump-trigger=atexit --dump-channel=bin-file \
  2>&1 | grep -v "warning:" || true

# 4) Build instrumented tests (use coverage runtime)
# gprbuild expects --src-subdirs=gnatcov-instr (not the full <project>-gnatcov-instr name)
echo "==> Building instrumented unit tests..."
alr exec -- gprbuild -f -p -P "$UNIT_TESTS_GPR" \
  --src-subdirs=gnatcov-instr \
  --implicit-with=gnatcov_rts_full.gpr

echo "==> Building instrumented integration tests..."
alr exec -- gprbuild -f -p -P "$INTEGRATION_TESTS_GPR" \
  --src-subdirs=gnatcov-instr \
  --implicit-with=gnatcov_rts_full.gpr

# 5) Run tests - use specific test runners
echo "==> Running unit tests..."
"$ROOT/test/bin/unit_runner" || { echo "Unit tests failed" >&2; }

echo "==> Running integration tests..."
"$ROOT/test/bin/integration_runner" || { echo "Integration tests failed" >&2; }

# 6) Collect all SID files (Source Instrumentation Data) into a list file
echo "==> Collecting SID files..."
SID_LIST="$ROOT/coverage/sid.list"
find "$ROOT/obj" -name "*.sid" > "$SID_LIST"
echo "Found $(wc -l < "$SID_LIST") SID files"

# 7) Build trace list file
echo "==> Building trace list..."
TRACE_LIST="$ROOT/coverage/traces.list"
find "$TRACES" -name "*.srctrace" > "$TRACE_LIST"

# 8) Analyze coverage (HTML and text summary)
# IMPORTANT: Use 'alr exec --' so gnatcov can find its libexec binaries
echo "==> Generating HTML coverage report..."
alr exec -- gnatcov coverage \
  --level=stmt+decision \
  --sid "@$SID_LIST" \
  --annotate=html \
  --output-dir "$REPORT" \
  "@$TRACE_LIST"

echo "==> Generating text summary..."
alr exec -- gnatcov coverage \
  --level=stmt+decision \
  --sid "@$SID_LIST" \
  --annotate=report \
  "@$TRACE_LIST" > "$ROOT/coverage/summary.txt"

echo
echo "✓ Coverage analysis complete!"
echo "  Text summary: $ROOT/coverage/summary.txt"
echo "  HTML report:  $REPORT/index.html"
echo
cat "$ROOT/coverage/summary.txt" | head -30
