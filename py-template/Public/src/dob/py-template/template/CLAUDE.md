# CLAUDE.md

This file provides guidance to Claude Code when working with this repository.

## Project Overview

{{DESCRIPTION}}

## Technology Stack

- **Python 3.10+** - Core implementation
- **Click** - CLI framework
- **uv** - Package management and virtual environments
- **pytest** - Testing framework
- **ruff** - Linting and formatting

## Directory Structure

```
{{PROJECT_NAME}}/
├── src/{{PACKAGE_NAME}}/    # Source code
│   ├── __init__.py
│   └── cli.py               # CLI entry point
├── tests/                   # Test files
│   ├── conftest.py          # Shared fixtures
│   └── test_cli.py          # CLI tests
├── pyproject.toml           # Project configuration
├── Makefile                 # Development commands
└── CLAUDE.md                # This file
```

## Development Commands

```bash
make help          # Show all available commands
make dev-setup     # Complete development setup
make test          # Run tests
make lint          # Run linting
make format        # Format code
make check         # Run lint + format + test
```

## Testing

```bash
make test                    # Run all tests
make quick-test              # Run tests, stop on first failure
make test-coverage           # Run with coverage report
```

## Development Workflow

1. Write tests first (TDD)
2. Implement minimal code to pass
3. Run `make check` before committing
