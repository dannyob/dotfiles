---
name: life-capture
description: Use when user wants to record a todo, task, or note to their life system inbox from any directory or project
---

# Life Capture

Quick capture of items to Danny's life system inbox. Works from any directory.

## When to Use

- User mentions wanting to remember something for later
- User asks to "add a todo" or "note this down"
- Capturing ideas, tasks, or follow-ups during other work

## Quick Capture

```bash
life add "Task title" --type todo --description "Optional details"
```

**Types:** `todo` (default for tasks), `project`, `person`, `organization`, `concept`

**Status:** Items go to `inbox` by default. That's correct for capture.

## Examples

```bash
# Simple task
life add "Review PR for auth changes" --type todo

# With description
life add "Write blog post about MCP servers" --type todo --description "Cover setup, common patterns, debugging"

# Capture a person to follow up on
life add "Jane Smith" --type person --description "Met at conference, works on distributed systems"
```

## Richer Operations

For operations needing life system context (duplicate checking, queries, linking), use the `life` subagent:

```
Task tool:
  subagent_type: life
  prompt: "How many items are in the inbox?"
```

The `life` subagent can read files in the life directory and run `life` commands, but writes are restricted to the CLI.

### Heavy Workflows

For complex workflows (inbox processing, enrichment, project structuring), run from the life directory with full skills:

```bash
cd /Users/danny/Private/nextcloud/life
claude
```

Then invoke skills like `inbox-processing`, `enrich-life-entry`, `structuring-projects`, etc.
