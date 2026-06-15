"""Tests for CLI commands."""

from click.testing import CliRunner
from {{PACKAGE_NAME}}.cli import cli


def test_hello(runner):
    """Test hello command."""
    result = runner.invoke(cli, ["hello"])
    assert result.exit_code == 0
    assert "Hello from {{PROJECT_NAME}}" in result.output


def test_version(runner):
    """Test --version flag."""
    result = runner.invoke(cli, ["--version"])
    assert result.exit_code == 0
    assert "0.1.0" in result.output
