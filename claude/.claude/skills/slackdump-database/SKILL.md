---
name: slackdump-database
description: Query and manipulate slackdump SQLite archives. Use when searching Slack messages, filtering by date/privacy, generating statistics, or creating smaller database subsets. Requires sqlite3 CLI only.
---

# Slackdump Database Skill

This skill enables querying and manipulating SQLite databases created by [slackdump](https://github.com/rusq/slackdump), a tool for archiving Slack workspaces.

## Prerequisites

- `sqlite3` command-line tool (pre-installed on macOS/Linux)
- A slackdump SQLite database (typically named `slackdump.sqlite`)

## Conceptual Overview

### Why This Structure Exists

Slackdump archives Slack data incrementally using a **chunk-based architecture**. Rather than storing data in normalized tables, it preserves the raw API responses as JSON blobs while extracting key fields for indexing. This design:

1. **Supports incremental updates** - New data appends as chunks without rewriting existing data
2. **Preserves full API fidelity** - The `DATA` blob contains complete Slack API responses
3. **Enables resume/recovery** - Sessions track progress; incomplete archives can be resumed
4. **Handles deduplication** - Multiple chunks may contain the same entity; views/queries select the latest

### Channel ID Prefixes

Slack uses prefixes to identify channel types:

| Prefix | Type | Privacy | Example |
|--------|------|---------|---------|
| `C` | Public channel | Public | `C01234ABCDE` |
| `D` | Direct message (1:1) | Private | `D01234ABCDE` |
| `G` | Group DM / Private channel | Private | `G01234ABCDE` |

**Privacy rule**: `C` channels are public; `D` and `G` channels are private.

### Chunk Types

Each piece of data is associated with a chunk type (stored in `CHUNK.TYPE_ID`):

| ID | Name | Description |
|----|------|-------------|
| 0 | MESSAGES | Channel messages |
| 1 | THREAD_MESSAGES | Thread replies |
| 2 | FILES | File metadata |
| 3 | USERS | User profiles |
| 4 | CHANNELS | Channel list |
| 5 | CHANNEL_INFO | Individual channel metadata |
| 6 | WORKSPACE_INFO | Workspace details |
| 7 | CHANNEL_USERS | Channel membership lists |

### Timestamps

Slack timestamps (`TS` fields) are strings in format `"1234567890.123456"` (Unix seconds with microseconds after the dot). To convert:

```sql
-- String timestamp to datetime
datetime(CAST(TS AS REAL), 'unixepoch')

-- With timezone (localtime)
datetime(CAST(TS AS REAL), 'unixepoch', 'localtime')

-- Filter by date range
CAST(TS AS REAL) BETWEEN strftime('%s', '2024-01-01') AND strftime('%s', '2024-12-31')
```

## Core Tables

### MESSAGE

The primary table for Slack messages.

| Column | Purpose |
|--------|---------|
| `ID` | Numeric timestamp (for sorting/joins) |
| `CHANNEL_ID` | Channel this message belongs to |
| `TS` | Slack timestamp string |
| `TXT` | Plain text content (extracted for searching) |
| `DATA` | Full JSON blob from Slack API |
| `IS_PARENT` | True if this is a thread parent |
| `THREAD_TS` | Thread timestamp (links replies to parent) |
| `PARENT_ID` | Numeric ID of parent message (for thread children) |

### CHANNEL

Channel metadata.

| Column | Purpose |
|--------|---------|
| `ID` | Channel ID (C/D/G prefix) |
| `NAME` | Channel name (null for DMs) |
| `DATA` | Full JSON with topic, purpose, members, etc. |

### S_USER

User profiles (S_ prefix = Slack User, avoiding SQL reserved word).

| Column | Purpose |
|--------|---------|
| `ID` | User ID (e.g., `U01234ABCDE`) |
| `USERNAME` | Display name |
| `DATA` | Full profile JSON (real_name, email, avatar, etc.) |

### CHUNK

Metadata about each data chunk (for internal tracking).

| Column | Purpose |
|--------|---------|
| `TYPE_ID` | Chunk type (see table above) |
| `CHANNEL_ID` | Associated channel (if applicable) |
| `SESSION_ID` | Which archive session created this |
| `FINAL` | True if this chunk completes a channel/thread |

## Common Queries

### Search Messages

```sql
-- Simple text search
SELECT
  datetime(CAST(m.TS AS REAL), 'unixepoch', 'localtime') as time,
  c.NAME as channel,
  u.USERNAME as author,
  m.TXT
FROM MESSAGE m
LEFT JOIN CHANNEL c ON m.CHANNEL_ID = c.ID
LEFT JOIN S_USER u ON json_extract(m.DATA, '$.user') = u.ID
WHERE m.TXT LIKE '%search term%'
ORDER BY m.TS DESC
LIMIT 50;
```

### Messages in Date Range

```sql
-- Messages from January 2024
SELECT
  datetime(CAST(m.TS AS REAL), 'unixepoch', 'localtime') as time,
  c.NAME as channel,
  m.TXT
FROM MESSAGE m
LEFT JOIN CHANNEL c ON m.CHANNEL_ID = c.ID
WHERE CAST(m.TS AS REAL) BETWEEN strftime('%s', '2024-01-01') AND strftime('%s', '2024-02-01')
ORDER BY m.TS;
```

### Public Channels Only

```sql
-- Filter to public channels (C prefix)
SELECT DISTINCT c.NAME, c.ID
FROM CHANNEL c
WHERE c.ID LIKE 'C%'
ORDER BY c.NAME;

-- Messages from public channels only
SELECT m.* FROM MESSAGE m
WHERE m.CHANNEL_ID LIKE 'C%';
```

### Private Channels/DMs Only

```sql
-- DMs and private channels (D and G prefixes)
SELECT m.* FROM MESSAGE m
WHERE m.CHANNEL_ID LIKE 'D%' OR m.CHANNEL_ID LIKE 'G%';
```

### Statistics

```sql
-- Messages per month
SELECT
  strftime('%Y-%m', datetime(CAST(TS AS REAL), 'unixepoch')) as month,
  COUNT(*) as messages
FROM MESSAGE
GROUP BY month
ORDER BY month;

-- Most active channels
SELECT
  c.NAME,
  COUNT(*) as message_count
FROM MESSAGE m
JOIN CHANNEL c ON m.CHANNEL_ID = c.ID
WHERE c.ID LIKE 'C%'  -- public only
GROUP BY m.CHANNEL_ID
ORDER BY message_count DESC
LIMIT 20;

-- Latest message date
SELECT datetime(MAX(CAST(TS AS REAL)), 'unixepoch', 'localtime') FROM MESSAGE;

-- Total counts
SELECT
  (SELECT COUNT(*) FROM MESSAGE) as messages,
  (SELECT COUNT(DISTINCT CHANNEL_ID) FROM MESSAGE) as channels_with_messages,
  (SELECT COUNT(DISTINCT ID) FROM S_USER) as users;
```

### Thread Context

```sql
-- Get a thread with all replies
SELECT
  datetime(CAST(m.TS AS REAL), 'unixepoch', 'localtime') as time,
  u.USERNAME,
  m.TXT
FROM MESSAGE m
LEFT JOIN S_USER u ON json_extract(m.DATA, '$.user') = u.ID
WHERE m.CHANNEL_ID = 'C01234ABCDE'
  AND (m.THREAD_TS = '1234567890.123456' OR m.TS = '1234567890.123456')
ORDER BY m.TS;
```

### User Lookup

```sql
-- Find user by name
SELECT ID, USERNAME, json_extract(DATA, '$.real_name') as real_name
FROM S_USER
WHERE USERNAME LIKE '%john%' OR json_extract(DATA, '$.real_name') LIKE '%John%';

-- Get user's profile details
SELECT
  json_extract(DATA, '$.real_name') as name,
  json_extract(DATA, '$.profile.email') as email,
  json_extract(DATA, '$.profile.title') as title,
  json_extract(DATA, '$.deleted') as is_deleted
FROM S_USER
WHERE ID = 'U01234ABCDE';
```

## Creating Smaller Databases

To create filtered copies, use SQLite's ability to attach databases and copy data.

### Remove Private Messages (Keep Public Only)

```sql
-- Create new database with public data only
-- Run: sqlite3 public_only.sqlite

ATTACH 'original.sqlite' AS src;

-- Copy schema
CREATE TABLE MESSAGE AS SELECT * FROM src.MESSAGE WHERE 0;
CREATE TABLE CHANNEL AS SELECT * FROM src.CHANNEL WHERE 0;
CREATE TABLE S_USER AS SELECT * FROM src.S_USER WHERE 0;
CREATE TABLE CHUNK AS SELECT * FROM src.CHUNK WHERE 0;

-- Copy public messages only
INSERT INTO MESSAGE SELECT * FROM src.MESSAGE WHERE CHANNEL_ID LIKE 'C%';

-- Copy public channels only
INSERT INTO CHANNEL SELECT * FROM src.CHANNEL WHERE ID LIKE 'C%';

-- Copy all users (needed for attribution)
INSERT INTO S_USER SELECT * FROM src.S_USER;

-- Copy relevant chunks
INSERT INTO CHUNK SELECT * FROM src.CHUNK
WHERE CHANNEL_ID LIKE 'C%' OR CHANNEL_ID IS NULL;

DETACH src;
VACUUM;
```

### Date Range Export

```sql
-- Export messages from specific date range
-- Run: sqlite3 date_range.sqlite

ATTACH 'original.sqlite' AS src;

CREATE TABLE MESSAGE AS SELECT * FROM src.MESSAGE WHERE 0;
CREATE TABLE CHANNEL AS SELECT * FROM src.CHANNEL;
CREATE TABLE S_USER AS SELECT * FROM src.S_USER;

-- Copy messages in date range (2024 only)
INSERT INTO MESSAGE
SELECT * FROM src.MESSAGE
WHERE CAST(TS AS REAL) BETWEEN strftime('%s', '2024-01-01') AND strftime('%s', '2025-01-01');

DETACH src;
VACUUM;
```

### Combined: Public + Date Range

```sql
-- Public messages from 2024 only
ATTACH 'original.sqlite' AS src;

CREATE TABLE MESSAGE AS SELECT * FROM src.MESSAGE WHERE 0;
CREATE TABLE CHANNEL AS SELECT * FROM src.CHANNEL WHERE ID LIKE 'C%';
CREATE TABLE S_USER AS SELECT * FROM src.S_USER;

INSERT INTO MESSAGE
SELECT * FROM src.MESSAGE
WHERE CHANNEL_ID LIKE 'C%'
  AND CAST(TS AS REAL) BETWEEN strftime('%s', '2024-01-01') AND strftime('%s', '2025-01-01');

DETACH src;
VACUUM;
```

## Working with JSON Data

The `DATA` columns contain full Slack API responses. Use `json_extract()` to access nested fields:

```sql
-- Message reactions
SELECT
  m.TXT,
  json_extract(m.DATA, '$.reactions') as reactions
FROM MESSAGE m
WHERE json_extract(m.DATA, '$.reactions') IS NOT NULL
LIMIT 10;

-- Messages with files
SELECT
  m.TXT,
  json_extract(m.DATA, '$.files[0].name') as filename
FROM MESSAGE m
WHERE json_extract(m.DATA, '$.files') IS NOT NULL;

-- Channel topic and purpose
SELECT
  NAME,
  json_extract(DATA, '$.topic.value') as topic,
  json_extract(DATA, '$.purpose.value') as purpose
FROM CHANNEL
WHERE ID LIKE 'C%';
```

## Slack Message Permalinks

To generate Slack permalinks from the database:

```
https://{workspace}.slack.com/archives/{channel_id}/p{timestamp_without_dot}
```

```sql
-- Generate permalink
SELECT
  'https://workspace.slack.com/archives/' ||
  CHANNEL_ID || '/p' || REPLACE(TS, '.', '') as permalink,
  TXT
FROM MESSAGE
WHERE TXT LIKE '%important%';
```

## Reference

For complete schema details including all columns, indexes, and views, see `references/schema.md`.
