.PHONY: help install install-dev test clean build upload upload-test check-version lint format
.DEFAULT_GOAL := help

# Configuration
PYTHON := python
UV := uv
PACKAGE_NAME := {{PROJECT_NAME}}

help: ## Show this help message
	@echo "Available targets:"
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "  %-20s %s\n", $$1, $$2}'

# Environment setup
venv: ## Create virtual environment
	$(UV) venv
	@echo "Activate with: source .venv/bin/activate"

install: venv ## Install package in development mode
	source .venv/bin/activate && $(UV) pip install -e .

install-dev: venv ## Install package with development dependencies
	source .venv/bin/activate && $(UV) pip install -e .
	source .venv/bin/activate && $(UV) pip install pytest build twine

# Testing
test: ## Run all tests
	source .venv/bin/activate && $(PYTHON) -m pytest tests/ -v

test-coverage: ## Run tests with coverage report
	source .venv/bin/activate && $(UV) pip install coverage
	source .venv/bin/activate && coverage run -m pytest tests/
	source .venv/bin/activate && coverage report
	source .venv/bin/activate && coverage html

# Code quality
lint: ## Run linting (install ruff if needed)
	source .venv/bin/activate && $(UV) pip install ruff
	source .venv/bin/activate && ruff check .

format: ## Format code (install ruff if needed)
	source .venv/bin/activate && $(UV) pip install ruff
	source .venv/bin/activate && ruff format .

lint-fix: ## Fix linting issues automatically
	source .venv/bin/activate && $(UV) pip install ruff
	source .venv/bin/activate && ruff check . --fix

lint-fix-unsafe: ## Fix linting issues including unsafe fixes
	source .venv/bin/activate && $(UV) pip install ruff
	source .venv/bin/activate && ruff check . --fix --unsafe-fixes

check: lint format test ## Run linting and tests

check-fix: lint-fix format test ## Fix linting issues, format, and test

# Build and distribution
clean: ## Clean build artifacts and cache files
	rm -rf build/
	rm -rf dist/
	rm -rf *.egg-info/
	rm -rf .pytest_cache/
	rm -rf .coverage
	rm -rf htmlcov/
	find . -type d -name __pycache__ -exec rm -rf {} +
	find . -type f -name "*.pyc" -delete
	find . -type f -name "*.pyo" -delete

build: clean ## Build distribution packages
	source .venv/bin/activate && $(PYTHON) -m build

check-version: ## Check if version needs to be updated for PyPI
	@echo "Current version in pyproject.toml:"
	@grep "version" pyproject.toml
	@echo ""
	@echo "Latest version on PyPI (if package exists):"
	@python -mpip index versions $(PACKAGE_NAME) || echo "Package not found on PyPI (this is normal for new packages)"

# PyPI publishing
upload-test: build ## Upload to TestPyPI
	source .venv/bin/activate && twine check dist/*
	source .venv/bin/activate && twine upload --repository testpypi dist/*
	@echo ""
	@echo "Test installation with:"
	@echo "pip install --index-url https://test.pypi.org/simple/ $(PACKAGE_NAME)"

upload: build ## Upload to PyPI (production)
	source .venv/bin/activate && twine check dist/*
	@echo "About to upload to PyPI. This cannot be undone!"
	@read -p "Are you sure? Type 'yes' to continue: " confirm && [ "$$confirm" = "yes" ]
	source .venv/bin/activate && twine upload dist/*

# Development workflow
dev-setup: install-dev ## Complete development setup
	@echo "Development environment ready!"
	@echo "Run 'source .venv/bin/activate' to activate the virtual environment"
	@echo "Run 'make test' to run tests"
	@echo "Run 'make check' to run linting and tests"

# CI/CD helpers
ci-test: ## Run tests in CI environment
	$(PYTHON) -m pytest tests/ -v --tb=short

ci-build: ## Build for CI (no virtual env)
	$(PYTHON) -m build

# Quick development commands
quick-test: ## Quick test run (no setup)
	source .venv/bin/activate && $(PYTHON) -m pytest tests/ -x

watch-test: ## Watch for changes and run tests
	source .venv/bin/activate && $(UV) pip install pytest-watch
	source .venv/bin/activate && ptw tests/ src/{{PACKAGE_NAME}}/
