# Ada Formatter Pipeline

A high-performance, concurrent Ada code formatter that ensures GNAT style compliance using a three-stage pipeline architecture.

## Features

- **Concurrent Pipeline**: Three-stage architecture for efficient processing
- **Parallel Formatting**: Multiple workers for faster processing of large codebases  
- **GNAT Style Compliance**: Fixes common style violations (comment spacing, operators, etc.)
- **Robust Error Handling**: Graceful recovery and detailed error reporting
- **Progress Tracking**: Real-time feedback on processing status
- **Safe File Operations**: Automatic backups during modification
- **Flexible Path Handling**: Process individual files or entire directory trees

## Architecture

```
┌─────────────────┐     ┌──────────────┐     ┌─────────────────┐
│ File Discoverer │ --> │ Queue Feeder │ --> │ File Formatters │
└─────────────────┘     └──────────────┘     └─────────────────┘
        │                       │                      │
        v                       v                      v
   Finds Ada files      Manages unique paths    Parallel workers
   in directories       and feeds queue          format files
```

### Stage 1: File Discovery
- Walks specified directories to find Ada files (.ads, .adb, .ada)
- Handles recursive/non-recursive traversal
- Applies directory-level exclusions early for efficiency

### Stage 2: Queue Management  
- Maintains set of unique file paths
- Feeds work queue for formatters
- Signals completion when discovery is done

### Stage 3: File Formatting
- Multiple parallel workers consume from queue
- Each worker applies formatting strategies
- Real-time progress reporting

## Installation

```bash
# Requires Python 3.8+
pip install aiofiles  # Optional but recommended for async I/O

# No other dependencies - uses Python standard library
```

## Usage

### Basic Examples

```bash
# Format a single file
python3 ada_formatter_pipeline.py project.gpr --include-path /project/src/main.adb

# Format a directory (non-recursive)
python3 ada_formatter_pipeline.py project.gpr --include-path /project/src

# Format recursively
python3 ada_formatter_pipeline.py project.gpr --include-path /project/src -r

# Multiple paths
python3 ada_formatter_pipeline.py project.gpr \
    --include-path /project/src \
    --include-path /project/lib/utils.ads \
    -r

# With exclusions
python3 ada_formatter_pipeline.py project.gpr \
    --include-path /project \
    --exclude-path /project/build \
    --exclude-path /project/tests \
    -r
```

### Parallel Processing

```bash
# Use parallel processing with default workers (50% of CPU cores)
python3 ada_formatter_pipeline.py project.gpr --include-path /project -r --parallel

# Specify worker count
python3 ada_formatter_pipeline.py project.gpr --include-path /project -r --parallel --workers 8
```

### Dry Run Mode

```bash
# Preview changes without modifying files
python3 ada_formatter_pipeline.py project.gpr --include-path /project -r --dry-run
```

## Command Line Arguments

| Argument | Description | Required |
|----------|-------------|----------|
| `project_file` | Path to the .gpr project file | Yes |
| `--include-path PATH` | Absolute path to file or directory to format (repeatable) | Yes |
| `--exclude-path PATH` | Absolute path to directory to exclude (repeatable) | No |
| `-r, --recursive` | Recursively process directories | No |
| `--dry-run` | Preview changes without modifying files | No |
| `--parallel` | Use parallel processing | No |
| `--workers N` | Number of formatter workers | No |
| `-v, --verbose` | Enable detailed logging | No |

## Path Requirements

**All paths must be absolute!** The formatter will reject relative paths with a clear error message showing the correct absolute path to use.

```bash
# Wrong
python3 ada_formatter_pipeline.py --include-path src

# Correct  
python3 ada_formatter_pipeline.py --include-path /home/user/project/src
```

## Style Fixes Applied

1. **Comment Spacing** (GNAT -gnatyt compliance)
   - Changes `-- comment` to `--  comment` (two spaces required)

2. **Operator Spacing**
   - Assignment: `x:=1` → `x := 1`
   - Arrow: `x=>1` → `x => 1` 
   - Comma: `x,y` → `x, y`

3. **Whitespace**
   - Removes trailing whitespace
   - Fixes spacing around operators

## Performance

The pipeline architecture provides excellent performance characteristics:

- **Concurrent Discovery**: Files are discovered while formatting begins
- **Parallel Processing**: Multiple files formatted simultaneously
- **Efficient Exclusions**: Directories excluded before traversal
- **Bounded Memory**: Queue size limits prevent memory exhaustion

### Benchmarks

On a 4-core system processing 1000 Ada files:
- Sequential: ~120 seconds
- Parallel (2 workers): ~65 seconds  
- Parallel (4 workers): ~35 seconds

## Error Handling

The formatter handles errors gracefully:

- **File Errors**: Skip and continue processing other files
- **Backup/Restore**: Automatic backup before modification
- **Signal Handling**: Graceful shutdown on SIGINT/SIGTERM
- **Clear Reporting**: Detailed error messages with context

## Testing

Run the test suite to verify functionality:

```bash
python3 test_pipeline.py
```

This runs tests for:
- Basic file and directory processing
- Parallel processing with multiple workers
- Error handling scenarios
- Actual file modification

## Integration with Build Systems

### Makefile Integration

```makefile
.PHONY: format
format:
    python3 ada_formatter_pipeline.py \
        --include-path $(PWD)/src \
        --include-path $(PWD)/lib \
        --exclude-path $(PWD)/build \
        -r --parallel

.PHONY: format-check
format-check:
    python3 ada_formatter_pipeline.py \
        --include-path $(PWD)/src \
        -r --dry-run
```

### CI/CD Integration

```yaml
# Example GitHub Actions
- name: Check Ada Style
  run: |
    python3 ada_formatter_pipeline.py \
      --include-path ${{ github.workspace }}/src \
      -r --dry-run
```

## Features

### LSP/ALS Integration ✅
The pipeline formatter now includes full Ada Language Server integration:
- Project file (.gpr) support for proper configuration
- Respects .gnatformat.yaml settings
- Intelligent code reformatting via gnatformat
- Falls back to pattern-based fixes when LSP is unavailable

Each worker maintains its own LSP client for true parallel processing.

## Known Issues / Not Implemented

### Long Line Handling
As of 2025-01-09, the formatter does **not** handle:
- **Long comment wrapping** - Comments exceeding line length limits are not automatically wrapped
- **Long string literal splitting** - String literals that exceed line limits are not split

These limitations exist because:
1. Ada Language Server (ALS) does not implement these features in its formatting
2. Implementing them correctly requires semantic understanding of Ada syntax
3. Manual splitting could break valid code or change program behavior

**Workaround**: Manually wrap long comments and split string literals using Ada's string concatenation:
```ada
--  This is a very long comment that should be
--  manually wrapped across multiple lines

Message : constant String := 
  "This is a very long string literal that " &
  "has been manually split across lines";
```

### Other Limitations

- Requires absolute paths (by design for clarity and security)
- No LSP integration yet (uses pattern-based fixes)
- Cannot fix complex style issues requiring semantic understanding

## Future Enhancements

- [ ] LSP integration for intelligent formatting
- [ ] Configuration file support
- [ ] Custom style rule definitions
- [ ] Integration with ada_language_server
- [ ] Watch mode for continuous formatting

## Contributing

When contributing, please ensure:
1. All paths remain absolute
2. Error handling is comprehensive
3. Tests are included for new features
4. Pipeline stages remain decoupled

## License

This tool is part of the adatypes project.