"""Command-line interface for {{PROJECT_NAME}}."""

import click


@click.group()
@click.version_option()
def cli():
    """{{DESCRIPTION}}"""
    pass


@cli.command()
def hello():
    """Say hello."""
    click.echo("Hello from {{PROJECT_NAME}}!")


if __name__ == "__main__":
    cli()
