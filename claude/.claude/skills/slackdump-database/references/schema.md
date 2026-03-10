# Slackdump Database Schema Reference

Complete schema documentation for slackdump SQLite archives.

## Tables

### SESSION

Tracks archive sessions (each run of slackdump).

```sql
CREATE TABLE SESSION (
    ID              INTEGER PRIMARY KEY,
    CREATED_AT      TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    UPDATED_AT      TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    PAR_SESSION_ID  INTEGER,           -- Parent session (for resume)
    FROM_TS         TIMESTAMP,         -- Start of time range
    TO_TS           TIMESTAMP,         -- End of time range
    FINISHED        SMALLINT NOT NULL DEFAULT FALSE,
    FILES_ENABLED   SMALLINT NOT NULL DEFAULT FALSE,
    AVATARS_ENABLED SMALLINT NOT NULL DEFAULT FALSE,
    MODE            TEXT NOT NULL,     -- 'archive', 'resume', etc.
    ARGS            TEXT               -- Command line arguments
);
```

### CHUNK

Every piece of data is stored as a chunk. This table tracks chunk metadata.

```sql
CREATE TABLE CHUNK (
    ID           INTEGER PRIMARY KEY,
    CREATED_AT   TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    UNIX_TS      INTEGER NOT NULL,     -- Unix timestamp when chunk was added
    SESSION_ID   INTEGER NOT NULL,     -- Which session created this
    TYPE_ID      SMALLINT NOT NULL,    -- Chunk type (see TYPES table)
    NUM_REC      INTEGER NOT NULL DEFAULT 0,  -- Number of records in chunk
    FINAL        SMALLINT NOT NULL DEFAULT FALSE,  -- Is this the final chunk for entity?
    CHANNEL_ID   TEXT,                 -- Associated channel (if applicable)
    SEARCH_QUERY TEXT,                 -- Search query (for search chunks)
    THREAD_ONLY  BOOLEAN,              -- Thread-only scraping flag
    FOREIGN KEY (SESSION_ID) REFERENCES SESSION (ID)
);

-- Key index for channel-based queries
CREATE INDEX CHUNK_I1 ON CHUNK (CHANNEL_ID, SESSION_ID, TYPE_ID, FINAL);
```

### MESSAGE

Primary message storage.

```sql
CREATE TABLE MESSAGE (
    ID           INTEGER NOT NULL,     -- Numeric timestamp (microseconds)
    CHUNK_ID     INTEGER NOT NULL,     -- Which chunk this came from
    LOAD_DTTM    TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    CHANNEL_ID   TEXT NOT NULL,        -- Channel ID (C/D/G prefix)
    TS           TEXT NOT NULL,        -- Slack timestamp string "1234567890.123456"
    PARENT_ID    INTEGER,              -- Numeric ID of parent (for thread replies)
    THREAD_TS    TEXT,                 -- Thread timestamp
    LATEST_REPLY TEXT,                 -- Most recent reply timestamp
    IS_PARENT    SMALLINT NOT NULL DEFAULT FALSE,  -- Is this a thread parent?
    IDX          INTEGER NOT NULL,     -- Index within the chunk
    NUM_FILES    INTEGER NOT NULL DEFAULT 0,  -- Attached file count
    TXT          TEXT,                 -- Extracted plain text
    DATA         BLOB NOT NULL,        -- Full JSON from Slack API
    PRIMARY KEY (ID, CHUNK_ID),
    FOREIGN KEY (CHUNK_ID) REFERENCES CHUNK (ID) ON DELETE CASCADE
);

CREATE INDEX MESSAGE_CHUNK_ID_IDX ON MESSAGE (CHUNK_ID);
CREATE INDEX MESSAGE_I1 ON MESSAGE (CHANNEL_ID, CHUNK_ID, IS_PARENT);
CREATE INDEX MESSAGE_I2 ON MESSAGE (CHANNEL_ID, PARENT_ID);
CREATE INDEX MESSAGE_I3 ON MESSAGE (IS_PARENT, LATEST_REPLY);
```

**DATA JSON structure** (example):
```json
{
  "type": "message",
  "user": "U01234ABCDE",
  "text": "Hello world",
  "ts": "1234567890.123456",
  "thread_ts": "1234567890.123456",
  "reply_count": 5,
  "reactions": [{"name": "thumbsup", "count": 3}],
  "files": [{"id": "F01234", "name": "doc.pdf"}],
  "blocks": [...],
  "attachments": [...]
}
```

### CHANNEL

Channel metadata.

```sql
CREATE TABLE CHANNEL (
    ID        TEXT NOT NULL,           -- Channel ID (C/D/G prefix)
    CHUNK_ID  INTEGER NOT NULL,
    LOAD_DTTM TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    NAME      TEXT,                    -- Channel name (null for DMs)
    IDX       INTEGER NOT NULL,
    DATA      BLOB NOT NULL,           -- Full JSON from Slack API
    PRIMARY KEY (ID, CHUNK_ID),
    FOREIGN KEY (CHUNK_ID) REFERENCES CHUNK (ID) ON DELETE CASCADE
);

CREATE INDEX CHANNEL_CHUNK_ID_IDX ON CHANNEL (CHUNK_ID);
```

**DATA JSON structure** (example):
```json
{
  "id": "C01234ABCDE",
  "name": "general",
  "created": 1543601727,
  "creator": "U01234ABCDE",
  "is_private": false,
  "is_archived": false,
  "is_channel": true,
  "is_im": false,
  "is_mpim": false,
  "topic": {"value": "Team discussions", "creator": "U01234"},
  "purpose": {"value": "General conversation", "creator": "U01234"},
  "num_members": 150
}
```

### S_USER

User profiles (S_ prefix avoids SQL reserved word "USER").

```sql
CREATE TABLE S_USER (
    ID        TEXT NOT NULL,           -- User ID (U prefix)
    CHUNK_ID  INTEGER NOT NULL,
    LOAD_DTTM TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    IDX       INTEGER NOT NULL,
    USERNAME  TEXT NOT NULL,           -- Display name
    DATA      BLOB NOT NULL,           -- Full JSON from Slack API
    PRIMARY KEY (ID, CHUNK_ID),
    FOREIGN KEY (CHUNK_ID) REFERENCES CHUNK (ID) ON DELETE CASCADE
);

CREATE INDEX S_USER_CHUNK_ID_IDX ON S_USER (CHUNK_ID);
```

**DATA JSON structure** (example):
```json
{
  "id": "U01234ABCDE",
  "name": "jsmith",
  "real_name": "John Smith",
  "deleted": false,
  "is_bot": false,
  "is_admin": false,
  "tz": "America/New_York",
  "profile": {
    "email": "john@example.com",
    "title": "Software Engineer",
    "display_name": "John",
    "real_name": "John Smith",
    "image_72": "https://..."
  }
}
```

### CHANNEL_USER

Channel membership mapping.

```sql
CREATE TABLE CHANNEL_USER (
    CHANNEL_ID TEXT NOT NULL,
    USER_ID    TEXT NOT NULL,
    CHUNK_ID   INTEGER NOT NULL,
    LOAD_DTTM  TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    IDX        INTEGER NOT NULL,
    PRIMARY KEY (CHANNEL_ID, USER_ID, CHUNK_ID),
    FOREIGN KEY (CHUNK_ID) REFERENCES CHUNK (ID) ON DELETE CASCADE
);

CREATE INDEX CHANNEL_USER_I1 ON CHANNEL_USER (USER_ID, CHUNK_ID);
CREATE INDEX CHANNEL_USER_CHUNK_ID_IDX ON CHANNEL_USER (CHUNK_ID);
```

### FILE

File metadata (actual files stored in `__uploads/` directory).

```sql
CREATE TABLE FILE (
    ID         TEXT NOT NULL,          -- File ID (F prefix)
    CHUNK_ID   INTEGER NOT NULL,
    LOAD_DTTM  TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    CHANNEL_ID TEXT NOT NULL,
    MESSAGE_ID INTEGER,                -- Parent message ID
    THREAD_ID  INTEGER,                -- Thread ID if in thread
    IDX        INTEGER NOT NULL,
    MODE       TEXT NOT NULL,          -- 'hosted', 'external', 'quip'
    FILENAME   TEXT,
    URL        TEXT,
    DATA       BLOB NOT NULL,
    PRIMARY KEY (ID, CHUNK_ID),
    FOREIGN KEY (CHUNK_ID) REFERENCES CHUNK (ID) ON DELETE CASCADE
);

CREATE INDEX FILE_CHUNK_ID_IDX ON FILE (CHUNK_ID);
CREATE INDEX FILE_I1 ON FILE (MESSAGE_ID, THREAD_ID);
```

### WORKSPACE

Workspace information.

```sql
CREATE TABLE WORKSPACE (
    ID            INTEGER PRIMARY KEY,
    CHUNK_ID      INTEGER NOT NULL,
    LOAD_DTTM     TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    TEAM          TEXT NOT NULL,       -- Workspace name
    USERNAME      TEXT,
    TEAM_ID       TEXT NOT NULL,
    USER_ID       TEXT NOT NULL,
    ENTERPRISE_ID TEXT,                -- For Enterprise Grid
    URL           TEXT NOT NULL,       -- Workspace URL
    DATA          BLOB NOT NULL,
    FOREIGN KEY (CHUNK_ID) REFERENCES CHUNK (ID) ON DELETE CASCADE
);
```

### TYPES

Lookup table for chunk types.

```sql
CREATE TABLE TYPES (
    ID   INT,
    NAME TEXT
);

-- Values:
-- 0  MESSAGES
-- 1  THREAD_MESSAGES
-- 2  FILES
-- 3  USERS
-- 4  CHANNELS
-- 5  CHANNEL_INFO
-- 6  WORKSPACE_INFO
-- 7  CHANNEL_USERS
-- 8  STARRED_ITEMS
-- 9  BOOKMARKS
-- 10 SEARCH_MESSAGES
-- 11 SEARCH_FILES
```

### Search Tables

For search result storage:

```sql
CREATE TABLE SEARCH_MESSAGE (
    ID           INTEGER PRIMARY KEY,
    CHUNK_ID     INTEGER NOT NULL,
    LOAD_DTTM    TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    CHANNEL_ID   TEXT NOT NULL,
    CHANNEL_NAME TEXT,
    TS           TEXT NOT NULL,
    TXT          TEXT,
    IDX          INTEGER NOT NULL,
    DATA         BLOB NOT NULL,
    FOREIGN KEY (CHUNK_ID) REFERENCES CHUNK (ID) ON DELETE CASCADE
);

CREATE TABLE SEARCH_FILE (
    ID        INTEGER PRIMARY KEY,
    CHUNK_ID  INTEGER NOT NULL,
    LOAD_DTTM TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    FILE_ID   TEXT NOT NULL,
    IDX       INTEGER NOT NULL,
    DATA      BLOB NOT NULL,
    FOREIGN KEY (CHUNK_ID) REFERENCES CHUNK (ID) ON DELETE CASCADE
);
```

## Views

The database includes several views for common operations:

### V_LATEST_MESSAGE

Gets the most recent message per channel.

```sql
SELECT CHANNEL_ID, TS, ID FROM V_LATEST_MESSAGE;
```

### V_LATEST_THREAD

Gets the most recent state of each thread.

```sql
SELECT CHANNEL_ID, THREAD_TS, TS, PARENT_ID, ID FROM V_LATEST_THREAD;
```

### V_CHANNEL_THREADS

Counts threads per channel per session.

```sql
SELECT SESSION_ID, CHANNEL_ID, THREADS FROM V_CHANNEL_THREADS;
```

### V_UNFINISHED_CHANNELS

Identifies channels with incomplete thread fetching.

```sql
SELECT SESSION_ID, CHANNEL_ID, REF_COUNT FROM V_UNFINISHED_CHANNELS;
```

### V_ORPHAN_THREADS

Finds threads that were started but not completed.

```sql
SELECT CHANNEL_ID, THREAD_TS FROM V_ORPHAN_THREADS;
```

## Useful JSON Paths

Common `json_extract()` paths for the DATA columns:

### MESSAGE.DATA

| Path | Description |
|------|-------------|
| `$.user` | User ID who sent message |
| `$.text` | Message text |
| `$.ts` | Timestamp |
| `$.thread_ts` | Thread timestamp |
| `$.reply_count` | Number of replies |
| `$.reactions` | Array of reactions |
| `$.files` | Array of attached files |
| `$.files[0].name` | First file's name |
| `$.attachments` | Link unfurls, etc. |
| `$.subtype` | Message subtype (join, leave, etc.) |

### CHANNEL.DATA

| Path | Description |
|------|-------------|
| `$.name` | Channel name |
| `$.topic.value` | Channel topic |
| `$.purpose.value` | Channel purpose |
| `$.creator` | Creator user ID |
| `$.created` | Creation timestamp |
| `$.is_private` | Privacy flag |
| `$.is_archived` | Archive status |
| `$.num_members` | Member count |

### S_USER.DATA

| Path | Description |
|------|-------------|
| `$.real_name` | Full name |
| `$.deleted` | Is account deleted |
| `$.is_bot` | Is bot account |
| `$.profile.email` | Email address |
| `$.profile.title` | Job title |
| `$.profile.display_name` | Display name |
| `$.profile.image_72` | Avatar URL (72px) |
| `$.tz` | Timezone |

## Performance Tips

1. **Use indexes**: Queries on `CHANNEL_ID`, `TS`, and `CHUNK_ID` are indexed
2. **Limit results**: Always use `LIMIT` for exploratory queries
3. **Avoid `SELECT *`**: Specify needed columns, especially avoiding `DATA` when not needed
4. **Use EXPLAIN**: Check query plans with `EXPLAIN QUERY PLAN SELECT ...`
5. **VACUUM after deletes**: Run `VACUUM;` after bulk deletions to reclaim space
