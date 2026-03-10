---
name: hutt-mail
description: "Use when reading, searching, archiving, moving, or managing the user's email. Covers searching with mu queries, moving messages between maildirs, archiving (including Gmail-specific handling), changing flags, indexing, syncing with mbsync, and using hutt's TUI or CLI to view and triage mail. Also use when the user asks about their inbox, wants to find specific emails, or needs help with email workflow."
---

# hutt Mail Reference

hutt is a fast, keyboard-driven TUI email client using **mu** as its search
backend.

## First Steps

Before doing anything with email, read the user's hutt config to understand
their accounts, maildirs, and setup:

```bash
cat "$(hutt config path)"                # read config (accounts, folders, descriptions)
cat ~/.config/hutt/splits.*.toml         # split inbox definitions per account
ls ~/.config/hutt/smart-folders/         # smart folder definitions per account
```

Account configs include a `description` field with context about each account
(type, sync commands, special handling notes). Read it.

For up-to-date command references:
```bash
hutt --help                  # hutt CLI options and remote commands
mu find --help               # mu find options
mu info fields               # all searchable fields (canonical)
man mu-query                 # full query language reference
```

## Searching Mail

**When hutt is NOT running** — use `mu find` directly:
```bash
# Search a specific account (use muhome from config)
mu find --muhome ~/.cache/mu/ACCOUNT 'maildir:/Inbox AND from:alice' --fields 'd f s' -z

# Messages from last week with attachments
mu find --muhome ~/.cache/mu/ACCOUNT 'flag:attach AND date:1w..' --fields 'd f s'

# All mail from a domain (regex)
mu find --muhome ~/.cache/mu/ACCOUNT 'from:/.*@example\.com$/' --fields 'd f s'

# Analyze how mu parses a query
mu find --muhome ~/.cache/mu/ACCOUNT 'subject:"hello world" AND date:3m..' --analyze
```

**When hutt IS running** — mu locks the database; use `hutt remote` instead:
```bash
hutt r search 'from:alice'
hutt r search --account=NAME 'flag:unread AND date:7d..'
hutt r navigate /Inbox
hutt r open <message-id>
hutt r thread <message-id>
hutt r compose --to=bob@example.com --subject="Hello"
```

### Structured Output for Scripting

Remote commands support `--sexp` and `--json` flags for structured output.
By default output is silent (command executes in the TUI only).

```bash
# ndjson output — one JSON object per line, ISO 8601 dates
hutt r --json search 'from:alice date:today'

# Get file paths for messages
hutt r --json search 'from:alice' | jq -r '.path'

# mu-compatible S-expression output
hutt r --sexp search 'flag:unread'

# Wrapped in a single container (--wrapped)
hutt r --json --wrapped search 'from:alice'
# → {"headers":[...], "found": 42}

hutt r --sexp --wrapped search 'from:alice'
# → (:headers (...) :found 42)
```

Each envelope includes: `docid`, `message-id`, `subject`, `from`, `to`,
`date`, `flags`, `path` (full filesystem path to Maildir message file),
`maildir`, and thread metadata.

**What each command returns:**

| Command | Output |
|---------|--------|
| `search <query>` | All matching envelopes |
| `open <message-id>` | The matched envelope |
| `thread <message-id>` | All envelopes in the thread |
| `navigate <folder>` | All envelopes in the folder |
| `compose`, `quit` | Nothing (ok/error only) |

Errors are also structured on stdout (exit code 1):
```bash
hutt r --json open nonexistent@example.com
# → {"error":"message not found: nonexistent@example.com"}
```

### Query Syntax Quick Reference

```
from:alice                    # sender
to:bob@example.com            # recipient
subject:"weekly report"       # phrase match
body:capybara                 # body text
maildir:/Inbox                # specific folder
flag:unread                   # message flags
date:7d..                     # last 7 days (relative: s M h d w m y)
date:20250101..20250301       # absolute date range
size:1m..5m                   # size range (b k m g suffixes)
mime:application/pdf          # MIME type
flag:attach                   # has attachments
from:alice*                   # prefix wildcard
from:/.*@example\.com$/       # PCRE regex

# Operators: AND (implicit), OR, NOT, ( )
from:alice AND date:1m..
(from:alice OR from:bob) AND subject:meeting
NOT flag:seen
```

Run `mu info fields` for the complete field list and `man mu-query` for the
full language (date formats, regex details, quoting rules, etc.).

## Moving and Archiving Mail

### With mu CLI (hutt NOT running)

```bash
# Move to different maildir
mu move --muhome ~/.cache/mu/ACCOUNT /path/to/msg /Archive

# Change flags (relative: +S adds Seen, -D removes Draft)
mu move --muhome ~/.cache/mu/ACCOUNT /path/to/msg --flags +S

# Remove message from filesystem and database
mu remove --muhome ~/.cache/mu/ACCOUNT /path/to/msg
```

### Gmail-Specific Archiving — CRITICAL

Gmail IMAP uses labels, not folders. Messages in Inbox **already exist** in
`[Gmail]/All Mail`. Do NOT `mu move` from Inbox to All Mail — this creates
duplicate files causing mbsync `duplicate UID` errors.

```bash
# CORRECT: Remove from Inbox (message stays in All Mail automatically)
mu remove --muhome ~/.cache/mu/ACCOUNT /path/to/inbox/msg

# WRONG — creates duplicates:
# mu move ... "/[Gmail]/All Mail"
```

You can detect Gmail accounts by checking if the archive folder in the config
contains `[Gmail]`. hutt handles this automatically in its TUI.

### With hutt TUI (interactive)

- `e` — archive (Gmail: remove from Inbox; others: move to archive)
- `#` — trash
- `!` — spam
- `v` — move to folder (opens picker)
- `z` — undo last action (not available for Gmail archive)

## Inbox Analysis Recipes

```bash
# Top senders in inbox
mu find --muhome ~/.cache/mu/ACCOUNT 'maildir:/Inbox' --fields 'f' | \
  sed 's/.*<\(.*\)>/\1/' | sort | uniq -c | sort -rn | head 30

# Top sender domains
mu find --muhome ~/.cache/mu/ACCOUNT 'maildir:/Inbox' --fields 'f' | \
  sed 's/.*@//' | sed 's/>.*//' | sort | uniq -c | sort -rn | head 30

# Count messages matching a potential split query
mu find --muhome ~/.cache/mu/ACCOUNT 'maildir:/Inbox AND from:noreply' --fields i | wc -l

# With --json: analyze via jq (when hutt IS running)
hutt r --json search 'maildir:/Inbox' | jq -r '.from[0].email' | \
  sed 's/.*@//' | sort | uniq -c | sort -rn | head 30
```

## Syncing and Indexing

Check the account's `description` and `sync_command` fields in the config for
the correct sync command. General pattern:

```bash
mbsync CHANNEL              # sync mail from server
mu index --muhome ~/.cache/mu/ACCOUNT   # reindex after sync
```

In hutt: `Ctrl+r` triggers sync_command then reindex.

## hutt server — mu server proxy

`hutt server` is a drop-in replacement for `mu server` that proxies
through hutt's running mu server via IPC. Solves the Xapian exclusive
lock problem — external tools can query mu while hutt is running.

```bash
hutt server                              # interactive mode (stdin/stdout)
hutt server --eval '(ping)'             # single S-expression evaluation
hutt server --eval '(find :query "flag:unread" :sortfield :date :maxnum 100 :threads t)'
hutt server --muhome ~/.cache/mu/work   # route to a specific account
hutt server --account work              # same, by account name
hutt server --commands                  # list available mu commands
```

Falls back to standalone `mu server` when hutt isn't running or muhome
doesn't match any account — safe as a drop-in replacement everywhere.

Interactive mode speaks the standard mu wire protocol (length-prefixed
S-expression frames), so it works as a backend for mu4e or any tool
that expects `mu server`.
