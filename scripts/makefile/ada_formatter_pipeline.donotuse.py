#!/usr/bin/env python3
"""
Ada Formatter Pipeline - Production-ready Ada code formatter with concurrent processing.

This tool uses a three-stage pipeline architecture:
1. File Discovery - Finds Ada files in specified directories
2. Queue Feeding - Manages unique file paths and feeds work queue
3. File Formatting - Parallel workers format files using LSP and patterns

Features:
    - Concurrent file discovery and processing
    - Parallel formatting with multiple workers
    - Robust error handling and recovery
    - Graceful shutdown on signals
    - Real-time progress reporting
    - Dry-run mode for previewing changes

Usage:
    ada_formatter_pipeline project.gpr --include-path /absolute/path/to/src -r
    ada_formatter_pipeline project.gpr --include-path /path/file.adb --exclude-path /path/tests
"""

import argparse
import asyncio
import json
import logging
import os
import re
import shutil
import signal
import subprocess
import sys
import time
from abc import ABC, abstractmethod
from dataclasses import dataclass, field
from enum import Enum
from pathlib import Path
from typing import List, Set, Dict, Tuple, Optional
from concurrent.futures import ProcessPoolExecutor
import multiprocessing

# Configure logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger(__name__)

# Constants
MAX_FILE_SIZE = 10 * 1024 * 1024  # 10MB
MAX_QUEUE_SIZE = 100
DEFAULT_WORKER_RATIO = 0.5  # Use 50% of CPU cores


class FormatterError(Exception):
    """Base exception for formatter errors."""
    pass


class PathValidationError(FormatterError):
    """Raised when path validation fails."""
    pass


class LSPError(FormatterError):
    """Errors related to Language Server Protocol."""
    pass


class FixType(Enum):
    """Types of fixes that can be applied."""
    LSP_FORMAT = "lsp_format"
    COMMENT_SPACING = "comment_spacing"
    OPERATOR_SPACING = "operator_spacing"
    PATTERN_FIX = "pattern_fix"


@dataclass
class FixResult:
    """Result of applying a fix to a file."""
    success: bool
    fix_type: FixType
    description: str
    error: Optional[str] = None


@dataclass
class ProcessingResult:
    """Result of processing a single file."""
    file_path: Path
    success: bool
    fixes: List[FixResult] = field(default_factory=list)
    error: Optional[str] = None
    processing_time: float = 0.0


@dataclass
class PipelineMetrics:
    """Metrics tracking for the pipeline."""
    discovered: int = 0
    queued: int = 0
    processed: int = 0
    failed: int = 0
    skipped: int = 0
    validation_errors: int = 0
    total_fixes: int = 0
    start_time: float = field(default_factory=time.time)
    
    @property
    def elapsed(self) -> float:
        return time.time() - self.start_time
    
    @property
    def throughput(self) -> float:
        return self.processed / self.elapsed if self.elapsed > 0 else 0
    
    def summary(self) -> str:
        """Generate a summary report."""
        summary = f"""
Pipeline Summary:
  Files discovered: {self.discovered}
  Files processed: {self.processed}
  Files failed: {self.failed}
  Files skipped: {self.skipped}"""
        
        if self.validation_errors > 0:
            summary += f"\n  Validation errors: {self.validation_errors}"
        
        summary += f"""
  Total fixes applied: {self.total_fixes}
  Elapsed time: {self.elapsed:.1f}s
  Throughput: {self.throughput:.1f} files/sec
"""
        return summary


class PathValidator:
    """Validates and normalizes file paths."""
    
    @staticmethod
    def validate_absolute_path(path_str: str) -> Path:
        """Ensure path is absolute, exists, and is readable."""
        path = Path(path_str)
        
        if not path.is_absolute():
            raise PathValidationError(
                f"Path must be absolute: {path_str}\n"
                f"Use: {path.resolve()}"
            )
        
        if not path.exists():
            raise PathValidationError(f"Path does not exist: {path_str}")
        
        # Check readability
        if not os.access(path, os.R_OK):
            raise PathValidationError(f"Path is not readable: {path_str}")
        
        # For directories, check execute permission (needed to list contents)
        # Note: Directories only need R+X, not W. Files within need R+W.
        if path.is_dir() and not os.access(path, os.X_OK):
            raise PathValidationError(f"Directory is not accessible: {path_str}")
        
        return path.resolve()
    
    @staticmethod
    def validate_writable_file(file_path: Path) -> None:
        """Ensure file exists, is readable and writable."""
        if not file_path.exists():
            raise PathValidationError(f"File does not exist: {file_path}")
        
        if not file_path.is_file():
            raise PathValidationError(f"Not a file: {file_path}")
        
        if not os.access(file_path, os.R_OK):
            raise PathValidationError(f"File is not readable: {file_path}")
        
        if not os.access(file_path, os.W_OK):
            raise PathValidationError(f"File is not writable: {file_path}")
    
    @staticmethod
    def validate_file_size(file_path: Path) -> None:
        """Check that file size is within limits."""
        size = file_path.stat().st_size
        if size > MAX_FILE_SIZE:
            raise FormatterError(
                f"File too large: {file_path} ({size / 1024 / 1024:.1f}MB)"
            )


@dataclass
class TextEdit:
    """Represents a text edit from LSP."""
    start_line: int
    start_char: int
    end_line: int
    end_char: int
    new_text: str


class SimpleLSPClient:
    """Simplified LSP client for formatting only."""
    
    def __init__(self, project_file: str):
        # Ensure project file is absolute
        project_path = Path(project_file)
        if not project_path.is_absolute():
            raise ValueError(f"Project file must be an absolute path: {project_file}")
        self.project_file = str(project_path)
        self.project_dir = project_path.parent
        self.process = None
        self.msg_id = 0
        self.initialized = False
    
    async def start(self):
        """Start the Ada Language Server process."""
        als_path = shutil.which('ada_language_server')
        if not als_path:
            raise LSPError("Ada Language Server not found. Install with: alr toolchain --install als")
        
        self.process = await asyncio.create_subprocess_exec(
            als_path,
            stdin=asyncio.subprocess.PIPE,
            stdout=asyncio.subprocess.PIPE,
            stderr=asyncio.subprocess.PIPE
        )
        
        # Initialize LSP
        await self._initialize()
    
    async def stop(self):
        """Stop the LSP server."""
        if self.process:
            await self._send_request('shutdown', {})
            await self._send_notification('exit', {})
            try:
                await asyncio.wait_for(self.process.wait(), timeout=5.0)
            except asyncio.TimeoutError:
                self.process.terminate()
                await self.process.wait()
    
    async def _send_message(self, message: dict):
        """Send a message to LSP."""
        content = json.dumps(message)
        header = f"Content-Length: {len(content)}\r\n\r\n"
        full_message = (header + content).encode('utf-8')
        
        self.process.stdin.write(full_message)
        await self.process.stdin.drain()
    
    async def _read_message(self) -> Optional[dict]:
        """Read a message from LSP."""
        # Read headers
        headers = {}
        while True:
            line = await self.process.stdout.readline()
            if not line:
                return None
            line = line.decode('utf-8').strip()
            if not line:
                break
            if ':' in line:
                key, value = line.split(':', 1)
                headers[key.strip()] = value.strip()
        
        # Read content
        if 'Content-Length' in headers:
            content_length = int(headers['Content-Length'])
            content = await self.process.stdout.read(content_length)
            return json.loads(content.decode('utf-8'))
        
        return None
    
    async def _send_request(self, method: str, params: dict) -> dict:
        """Send request and wait for response."""
        self.msg_id += 1
        message = {
            'jsonrpc': '2.0',
            'id': self.msg_id,
            'method': method,
            'params': params
        }
        
        await self._send_message(message)
        
        # Wait for response with matching ID
        msg_id = self.msg_id
        while True:
            response = await self._read_message()
            if response and response.get('id') == msg_id:
                if 'error' in response:
                    raise LSPError(f"LSP error: {response['error']}")
                return response
    
    async def _send_notification(self, method: str, params: dict):
        """Send a notification (no response expected)."""
        message = {
            'jsonrpc': '2.0',
            'method': method,
            'params': params
        }
        await self._send_message(message)
    
    async def _initialize(self):
        """Initialize the LSP server."""
        init_params = {
            'processId': os.getpid(),
            'rootPath': str(self.project_dir),
            'rootUri': f'file://{self.project_dir}',
            'capabilities': {
                'textDocument': {
                    'formatting': {'dynamicRegistration': True}
                }
            },
            'initializationOptions': {
                'projectFile': str(Path(self.project_file).resolve())
            }
        }
        
        response = await self._send_request('initialize', init_params)
        await self._send_notification('initialized', {})
        
        # Give server time to load project
        await asyncio.sleep(2)
        self.initialized = True
    
    async def format_document(self, file_path: Path, content: str) -> Optional[List[TextEdit]]:
        """Format a document using LSP."""
        if not self.initialized:
            return None
        
        uri = f'file://{file_path.resolve()}'
        
        # Open document
        await self._send_notification('textDocument/didOpen', {
            'textDocument': {
                'uri': uri,
                'languageId': 'ada',
                'version': 1,
                'text': content
            }
        })
        
        try:
            # Request formatting
            response = await self._send_request('textDocument/formatting', {
                'textDocument': {'uri': uri},
                'options': {
                    'tabSize': 3,
                    'insertSpaces': True
                }
            })
            
            result = response.get('result', [])
            if result is None:
                return None
            
            # Convert to our TextEdit format
            edits = []
            for edit in result:
                range_info = edit.get('range', {})
                start = range_info.get('start', {})
                end = range_info.get('end', {})
                
                edits.append(TextEdit(
                    start_line=start.get('line', 0),
                    start_char=start.get('character', 0),
                    end_line=end.get('line', 0),
                    end_char=end.get('character', 0),
                    new_text=edit.get('newText', '')
                ))
            
            return edits
            
        finally:
            # Close document
            await self._send_notification('textDocument/didClose', {
                'textDocument': {'uri': uri}
            })


class FixStrategy(ABC):
    """Abstract base class for formatting strategies."""
    
    @abstractmethod
    async def apply(self, content: str, file_path: Path, lsp_client: Optional[SimpleLSPClient] = None) -> Tuple[str, List[FixResult]]:
        """Apply the fix strategy to file content."""
        pass


class LSPFormattingStrategy(FixStrategy):
    """Use LSP textDocument/formatting for comprehensive formatting."""
    
    async def apply(self, content: str, file_path: Path, lsp_client: Optional[SimpleLSPClient] = None) -> Tuple[str, List[FixResult]]:
        """Apply LSP formatting to the file."""
        if not lsp_client:
            return content, []
        
        try:
            edits = await lsp_client.format_document(file_path, content)
            if not edits:
                return content, []
            
            # Apply edits to content
            lines = content.split('\n')
            
            # Sort edits in reverse order
            sorted_edits = sorted(edits, key=lambda e: (e.start_line, e.start_char), reverse=True)
            
            for edit in sorted_edits:
                if edit.start_line >= len(lines):
                    continue
                
                if edit.start_line == edit.end_line:
                    # Single line edit
                    line = lines[edit.start_line]
                    lines[edit.start_line] = (
                        line[:edit.start_char] + 
                        edit.new_text + 
                        line[edit.end_char:]
                    )
                else:
                    # Multi-line edit
                    prefix = lines[edit.start_line][:edit.start_char]
                    suffix = lines[edit.end_line][edit.end_char:] if edit.end_line < len(lines) else ''
                    
                    # Remove old lines
                    del lines[edit.start_line:edit.end_line + 1]
                    
                    # Insert new content
                    new_lines = edit.new_text.split('\n')
                    if len(new_lines) == 1:
                        lines.insert(edit.start_line, prefix + new_lines[0] + suffix)
                    else:
                        lines.insert(edit.start_line, prefix + new_lines[0])
                        for i, line in enumerate(new_lines[1:-1], 1):
                            lines.insert(edit.start_line + i, line)
                        lines.insert(edit.start_line + len(new_lines) - 1, new_lines[-1] + suffix)
            
            new_content = '\n'.join(lines)
            
            fixes = []
            if new_content != content:
                fixes.append(FixResult(
                    success=True,
                    fix_type=FixType.LSP_FORMAT,
                    description=f"Applied {len(edits)} formatting changes from LSP/gnatformat"
                ))
            
            return new_content, fixes
            
        except Exception as e:
            logger.warning(f"LSP formatting failed: {e}")
            return content, []


class CommentSpacingStrategy(FixStrategy):
    """Fix Ada comment spacing (-- to --  ) for GNAT style compliance."""
    
    async def apply(self, content: str, file_path: Path, lsp_client: Optional[SimpleLSPClient] = None) -> Tuple[str, List[FixResult]]:
        # Pattern matches -- followed by single space and non-space
        pattern = r'^(.*?)--\s([^\s].*)$'
        new_content, count = re.subn(
            pattern, 
            r'\1--  \2', 
            content, 
            flags=re.MULTILINE
        )
        
        fixes = []
        if count > 0:
            fixes.append(FixResult(
                success=True,
                fix_type=FixType.COMMENT_SPACING,
                description=f"Fixed comment spacing in {count} lines"
            ))
        
        return new_content, fixes


class OperatorSpacingStrategy(FixStrategy):
    """Fix spacing around Ada operators."""
    
    def __init__(self):
        self.patterns = [
            (r'(\w)\s*:=\s*', r'\1 := ', "assignment operator"),
            (r'(\w)\s*=>\s*', r'\1 => ', "arrow operator"),
            (r',([^\s])', r', \1', "comma"),
            (r';--', ';  --', "semicolon before comment"),
            (r'\s+$', '', "trailing whitespace"),
        ]
    
    async def apply(self, content: str, file_path: Path, lsp_client: Optional[SimpleLSPClient] = None) -> Tuple[str, List[FixResult]]:
        fixes = []
        
        for pattern, replacement, description in self.patterns:
            new_content = re.sub(pattern, replacement, content, flags=re.MULTILINE)
            if new_content != content:
                fixes.append(FixResult(
                    success=True,
                    fix_type=FixType.OPERATOR_SPACING,
                    description=f"Fixed {description}"
                ))
                content = new_content
        
        return content, fixes


class FileFormatter:
    """Formats a single Ada file using configured strategies."""
    
    def __init__(self, strategies: List[FixStrategy], dry_run: bool = False, lsp_client: Optional[SimpleLSPClient] = None):
        self.strategies = strategies
        self.dry_run = dry_run
        self.lsp_client = lsp_client
    
    async def format_file(self, file_path: Path) -> ProcessingResult:
        """Format a single file and return results."""
        start_time = time.time()
        
        try:
            # Ensure we have absolute path
            if not file_path.is_absolute():
                raise FileOperationError(f"File path must be absolute: {file_path}")
            
            # Validate file exists and permissions
            if not self.dry_run:
                PathValidator.validate_writable_file(file_path)
            elif not file_path.exists():
                raise FileOperationError(f"File does not exist: {file_path}")
            
            # Validate file size
            PathValidator.validate_file_size(file_path)
            
            # Read content
            content = file_path.read_text(encoding='utf-8')
            original_content = content
            
            all_fixes = []
            
            # Apply strategies
            for strategy in self.strategies:
                try:
                    content, fixes = await strategy.apply(content, file_path, self.lsp_client)
                    all_fixes.extend(fixes)
                except Exception as e:
                    logger.warning(f"Strategy {strategy.__class__.__name__} failed: {e}")
            
            # Write back if changed
            if content != original_content and not self.dry_run:
                # Create backup
                backup_path = file_path.with_suffix(file_path.suffix + '.backup')
                shutil.copy2(file_path, backup_path)
                
                try:
                    file_path.write_text(content, encoding='utf-8')
                    backup_path.unlink()  # Remove backup on success
                except Exception:
                    shutil.move(backup_path, file_path)  # Restore on failure
                    raise
            
            return ProcessingResult(
                file_path=file_path,
                success=True,
                fixes=all_fixes,
                processing_time=time.time() - start_time
            )
            
        except Exception as e:
            logger.error(f"Failed to format {file_path}: {e}")
            return ProcessingResult(
                file_path=file_path,
                success=False,
                error=str(e),
                processing_time=time.time() - start_time
            )


class AdaFormatterPipeline:
    """Three-stage pipeline for formatting Ada files."""
    
    def __init__(self, project_file: str, num_workers: int, dry_run: bool = False):
        # Ensure project file is absolute
        project_path = Path(project_file)
        if not project_path.is_absolute():
            raise ValueError(f"Project file must be an absolute path: {project_file}")
        self.project_file = str(project_path)
        self.project_dir = project_path.parent
        self.num_workers = num_workers
        self.dry_run = dry_run
        
        # Pipeline components
        self.files_set = set()
        self.files_set_lock = asyncio.Lock()
        self.work_queue = asyncio.Queue(maxsize=MAX_QUEUE_SIZE)
        
        # State tracking
        self.discovery_done = False
        self.shutdown_event = asyncio.Event()
        self.metrics = PipelineMetrics()
        
        # Configure strategies (LSP first, then fallbacks)
        self.strategies = [
            LSPFormattingStrategy(),
            CommentSpacingStrategy(),
            OperatorSpacingStrategy(),
        ]
        
        # Results collection
        self.results = []
        self.results_lock = asyncio.Lock()
    
    async def file_discoverer(self, directories: Set[Path], files: Set[Path], 
                             excluded_dirs: Set[Path], recursive: bool):
        """Stage 1: Discover Ada files in directories and add direct files."""
        logger.info("Starting file discovery...")
        
        # Add directly specified files (unless in excluded directories)
        async with self.files_set_lock:
            for file_path in files:
                if file_path.suffix in ('.ads', '.adb', '.ada'):
                    # Check if file is in an excluded directory
                    if not any(file_path.is_relative_to(excl) for excl in excluded_dirs):
                        self.files_set.add(file_path)
                        self.metrics.discovered += 1
        
        # Discover files in directories
        for dir_path in directories:
            if self.shutdown_event.is_set():
                break
                
            try:
                # Patterns for Ada files
                patterns = ['**/*.ads', '**/*.adb', '**/*.ada'] if recursive else ['*.ads', '*.adb', '*.ada']
                
                for pattern in patterns:
                    for file_path in dir_path.glob(pattern):
                        if self.shutdown_event.is_set():
                            break
                        
                        # Check if file is in an excluded directory
                        if any(file_path.is_relative_to(excl) for excl in excluded_dirs):
                            continue
                        
                        # Validate file permissions
                        if not os.access(file_path, os.R_OK):
                            logger.error(f"Skipping unreadable file: {file_path}")
                            continue
                        
                        if not self.dry_run and not os.access(file_path, os.W_OK):
                            logger.error(f"Skipping read-only file: {file_path}")
                            continue
                        
                        # Check file size
                        try:
                            if file_path.stat().st_size > MAX_FILE_SIZE:
                                logger.error(f"Skipping file too large: {file_path} (>{MAX_FILE_SIZE/1024/1024:.0f}MB)")
                                continue
                        except Exception as e:
                            logger.error(f"Skipping file with stat error: {file_path}: {e}")
                            continue
                            
                        async with self.files_set_lock:
                            if file_path not in self.files_set:
                                self.files_set.add(file_path)
                                self.metrics.discovered += 1
                                
                                # Log progress every 100 files
                                if self.metrics.discovered % 100 == 0:
                                    logger.info(f"Discovered {self.metrics.discovered} files...")
                                    
            except Exception as e:
                logger.error(f"Error discovering files in {dir_path}: {e}")
        
        self.discovery_done = True
        logger.info(f"Discovery complete. Found {self.metrics.discovered} files.")
    
    async def queue_feeder(self):
        """Stage 2: Feed unique files from set to work queue."""
        logger.info("Starting queue feeder...")
        
        while not self.shutdown_event.is_set():
            async with self.files_set_lock:
                if self.files_set:
                    # Pop a file from the set
                    file_path = self.files_set.pop()
                    self.metrics.queued += 1
                elif self.discovery_done:
                    # No more files coming, send stop signals
                    logger.info("Sending stop signals to formatters...")
                    for _ in range(self.num_workers):
                        await self.work_queue.put(None)
                    break
                else:
                    # Discovery still running, wait briefly
                    await asyncio.sleep(0.01)
                    continue
            
            # Add to queue (outside lock to avoid blocking)
            await self.work_queue.put(file_path)
    
    async def file_formatter(self, formatter_id: int):
        """Stage 3: Format Ada files."""
        logger.info(f"Formatter {formatter_id} started")
        
        # Create LSP client for this worker
        lsp_client = None
        try:
            lsp_client = SimpleLSPClient(self.project_file)
            await lsp_client.start()
            logger.info(f"Formatter {formatter_id}: LSP initialized")
        except Exception as e:
            logger.warning(f"Formatter {formatter_id}: LSP unavailable ({e}), using fallback strategies")
        
        formatter = FileFormatter(self.strategies, self.dry_run, lsp_client)
        
        while not self.shutdown_event.is_set():
            try:
                # Get work from queue
                file_path = await self.work_queue.get()
                
                if file_path is None:  # Stop signal
                    self.work_queue.task_done()
                    break
                
                # Process file
                rel_path = file_path.relative_to(Path.cwd()) if file_path.is_relative_to(Path.cwd()) else file_path
                logger.debug(f"[Formatter {formatter_id}] Processing {rel_path}")
                
                result = await formatter.format_file(file_path)
                
                # Update metrics
                if result.success:
                    self.metrics.processed += 1
                    self.metrics.total_fixes += len(result.fixes)
                    
                    if result.fixes:
                        action = "would apply" if self.dry_run else "applied"
                        print(f"{rel_path}: {len(result.fixes)} fixes {action}")
                    else:
                        print(f"{rel_path}: OK")
                else:
                    self.metrics.failed += 1
                    print(f"{rel_path}: ERROR - {result.error}")
                
                # Store result
                async with self.results_lock:
                    self.results.append(result)
                
                self.work_queue.task_done()
                
            except asyncio.CancelledError:
                raise
            except Exception as e:
                logger.error(f"Formatter {formatter_id} error: {e}")
                self.metrics.failed += 1
                self.work_queue.task_done()
        
        # Clean up LSP client
        if lsp_client:
            try:
                await lsp_client.stop()
            except Exception as e:
                logger.warning(f"Formatter {formatter_id}: Error stopping LSP: {e}")
        
        logger.info(f"Formatter {formatter_id} stopped")
    
    async def run(self, directories: Set[Path], files: Set[Path], 
                  excluded_dirs: Set[Path], recursive: bool):
        """Run the complete pipeline."""
        # Filter directories based on exclusions
        filtered_dirs = set()
        for dir_path in directories:
            # Check if this directory should be excluded
            should_exclude = False
            for excl in excluded_dirs:
                if dir_path == excl or dir_path.is_relative_to(excl) or excl.is_relative_to(dir_path):
                    should_exclude = True
                    break
            
            if not should_exclude:
                filtered_dirs.add(dir_path)
            else:
                logger.info(f"Excluding directory: {dir_path}")
        
        # Setup signal handlers
        self._setup_signal_handlers()
        
        # Start all pipeline stages
        try:
            async with asyncio.TaskGroup() as tg:
                # Start file discovery
                tg.create_task(self.file_discoverer(filtered_dirs, files, excluded_dirs, recursive))
                
                # Start queue feeder
                tg.create_task(self.queue_feeder())
                
                # Start formatters
                for i in range(self.num_workers):
                    tg.create_task(self.file_formatter(i))
                    
        except Exception as e:
            logger.error(f"Pipeline error: {e}")
            raise
        
        # Print summary
        print(self.metrics.summary())
    
    def _setup_signal_handlers(self):
        """Setup handlers for graceful shutdown."""
        def signal_handler(signum, frame):
            logger.info(f"Received signal {signum}, shutting down...")
            self.shutdown_event.set()
        
        signal.signal(signal.SIGINT, signal_handler)
        signal.signal(signal.SIGTERM, signal_handler)


async def main():
    """Main entry point."""
    parser = argparse.ArgumentParser(
        description='Ada Formatter Pipeline - Concurrent Ada code formatting',
        epilog='All paths must be absolute.'
    )
    
    parser.add_argument(
        'project_file',
        help='Path to the .gpr project file'
    )
    
    parser.add_argument(
        '--include-path', 
        action='append', 
        required=True,
        help='Absolute path to file or directory to format (can be repeated)'
    )
    
    parser.add_argument(
        '--exclude-path', 
        action='append', 
        default=[],
        help='Absolute path to directory to exclude (can be repeated)'
    )
    
    parser.add_argument(
        '-r', '--recursive',
        action='store_true',
        help='Recursively process directories'
    )
    
    parser.add_argument(
        '--dry-run',
        action='store_true',
        help='Show what would be changed without modifying files'
    )
    
    parser.add_argument(
        '--parallel',
        action='store_true',
        help='Use parallel processing (recommended for large codebases)'
    )
    
    parser.add_argument(
        '--workers',
        type=int,
        default=0,
        help=f'Number of formatter workers (default: {int(multiprocessing.cpu_count() * DEFAULT_WORKER_RATIO)})'
    )
    
    parser.add_argument(
        '-v', '--verbose',
        action='store_true',
        help='Enable verbose output'
    )
    
    args = parser.parse_args()
    
    # Configure logging level
    if args.verbose:
        logging.getLogger().setLevel(logging.DEBUG)
    
    # Validate project file
    project_path = Path(args.project_file)
    
    # Check if path is absolute
    if not project_path.is_absolute():
        print(f"Error: Project file path must be absolute: {args.project_file}", file=sys.stderr)
        print(f"Use: {project_path.resolve()}", file=sys.stderr)
        return 1
    
    if not project_path.exists():
        print(f"Error: Project file not found: {args.project_file}", file=sys.stderr)
        return 1
    
    if not project_path.is_file():
        print(f"Error: Project file is not a file: {args.project_file}", file=sys.stderr)
        return 1
    
    if not os.access(project_path, os.R_OK):
        print(f"Error: Project file is not readable: {args.project_file}", file=sys.stderr)
        return 1
    
    # Project file should be read-only, no write access needed
    
    if not project_path.suffix == '.gpr':
        print(f"Warning: Project file does not have .gpr extension: {args.project_file}", file=sys.stderr)
    
    # Validate and categorize paths
    included_files = set()
    included_dirs = set()
    excluded_dirs = set()
    
    # Process includes with full validation
    for path_str in args.include_path:
        try:
            path = PathValidator.validate_absolute_path(path_str)
        except PathValidationError as e:
            print(f"Error in --include-path: {e}", file=sys.stderr)
            return 1
        
        if path.is_file():
            # Additional validation for files
            if not path.suffix in ('.ads', '.adb', '.ada'):
                print(f"Warning: File does not have Ada extension: {path}", file=sys.stderr)
            
            # For files, we need write permission unless dry-run
            if not args.dry_run and not os.access(path, os.W_OK):
                print(f"Error: File is not writable: {path}", file=sys.stderr)
                return 1
            
            included_files.add(path)
        elif path.is_dir():
            included_dirs.add(path)
        else:
            print(f"Error: Path is neither file nor directory: {path}", file=sys.stderr)
            return 1
    
    # Process excludes with full validation
    for path_str in args.exclude_path:
        try:
            path = PathValidator.validate_absolute_path(path_str)
        except PathValidationError as e:
            print(f"Error in --exclude-path: {e}", file=sys.stderr)
            return 1
        
        if not path.is_dir():
            print(f"Error: Exclude path must be a directory: {path}", file=sys.stderr)
            return 1
        
        excluded_dirs.add(path)
    
    # Determine worker count
    if args.workers > 0:
        num_workers = args.workers
    else:
        num_workers = max(1, int(multiprocessing.cpu_count() * DEFAULT_WORKER_RATIO))
    
    if args.parallel:
        print(f"Using {num_workers} parallel workers")
    else:
        num_workers = 1
        print("Using single worker (sequential processing)")
    
    if args.dry_run:
        print("DRY RUN - No files will be modified\n")
    
    # Run the pipeline
    try:
        pipeline = AdaFormatterPipeline(args.project_file, num_workers, args.dry_run)
        await pipeline.run(included_dirs, included_files, excluded_dirs, args.recursive)
        return 0
        
    except KeyboardInterrupt:
        print("\nInterrupted by user", file=sys.stderr)
        return 130
    except Exception as e:
        print(f"Error: {e}", file=sys.stderr)
        return 1


if __name__ == '__main__':
    sys.exit(asyncio.run(main()))