#!/bin/bash
# Create a new Python project from template
#
# Usage: ./new-project.sh <project-name> [target-dir] [description]
#
# Examples:
#   ./new-project.sh my-tool
#   ./new-project.sh my-tool ~/projects/my-tool
#   ./new-project.sh my-tool ~/projects/my-tool "A useful CLI tool"

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
TEMPLATE_DIR="$SCRIPT_DIR/template"

# Parse arguments
PROJECT_NAME="${1:-}"
TARGET_DIR="${2:-$(pwd)/$PROJECT_NAME}"
DESCRIPTION="${3:-A Python CLI application}"

if [ -z "$PROJECT_NAME" ]; then
    echo "Usage: $0 <project-name> [target-dir] [description]"
    echo ""
    echo "Examples:"
    echo "  $0 my-tool"
    echo "  $0 my-tool ~/projects/my-tool"
    echo "  $0 my-tool ~/projects/my-tool \"A useful CLI tool\""
    exit 1
fi

# Convert project name to valid Python package name
# my-tool -> my_tool
PACKAGE_NAME=$(echo "$PROJECT_NAME" | tr '-' '_')

echo "Creating project: $PROJECT_NAME"
echo "  Package name: $PACKAGE_NAME"
echo "  Target: $TARGET_DIR"
echo "  Description: $DESCRIPTION"
echo ""

# Check if target exists
if [ -d "$TARGET_DIR" ]; then
    echo "Error: $TARGET_DIR already exists"
    exit 1
fi

# Copy template
cp -r "$TEMPLATE_DIR" "$TARGET_DIR"

# Rename the {{package}} directory
mv "$TARGET_DIR/src/{{package}}" "$TARGET_DIR/src/$PACKAGE_NAME"

# Replace placeholders in all files
find "$TARGET_DIR" -type f \( -name "*.py" -o -name "*.toml" -o -name "*.md" -o -name "Makefile" \) | while read -r file; do
    # Use | as delimiter since paths might contain /
    sed -i '' \
        -e "s|{{PROJECT_NAME}}|$PROJECT_NAME|g" \
        -e "s|{{PACKAGE_NAME}}|$PACKAGE_NAME|g" \
        -e "s|{{DESCRIPTION}}|$DESCRIPTION|g" \
        "$file"
done

echo "Project created!"
echo ""
echo "Next steps:"
echo "  cd $TARGET_DIR"
echo "  git init"
echo "  make dev-setup"
echo "  make test"
