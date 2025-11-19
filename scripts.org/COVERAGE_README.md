# GNATcoverage Quick Start (macOS, source-trace)

**Version:** 2.0.0
**Date:** November 13, 2025
**SPDX-License-Identifier:** BSD-3-Clause
**License File:** See the LICENSE file in the project root.
**Copyright:** © 2025 Michael Gardner, A Bit of Help, Inc.
**Status:** Released

## Prereqs
- `gnatcov` is on your `PATH` (Darwin x86_64 build).
- Your tests build via `tests.gpr`.

## One-time: tell the script where your tests live
If your project file is `tests.gpr` at the repo root, you're set.
If not, export:
```bash
export TESTS_GPR="/absolute/path/to/your/tests.gpr"
```

## Run
```bash
chmod +x coverage.sh
./coverage.sh
```

This will:
1. Instrument tests to emit `.srctrace` at exit
2. Build with the GNATcoverage runtime
3. Auto-run discovered `*test*` executables under `obj*/`
4. Generate:
   - Text summary: `coverage/summary.txt`
   - HTML report: `coverage/report/dhtml/index.html`

## Tips
- If auto-discovery misses your test binaries, set them explicitly:
  ```bash
  export TEST_BINS="/full/path/to/test_find_by_pattern /full/path/to/test_find_by_regex"
  ./coverage.sh
  ```
- To exclude additional sources from the report (e.g., test spies):
  adjust the `--ignore-source-files=` globs in the script.
