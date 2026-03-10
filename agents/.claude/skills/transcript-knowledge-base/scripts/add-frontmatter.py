#!/usr/bin/env -S uv run --script
# /// script
# requires-python = ">=3.10"
# ///
"""
Add YAML frontmatter to tidied transcripts using yt-dlp info.json metadata.

Extracts title, URL, upload date, duration, description, and optionally
infers the event name from description text or upload date.

CUSTOMIZATION:
- extract_event_info(): Add regex patterns for your conference/event names
- infer_event_from_date(): Add date->event mappings for known events
"""

import json
import os
import re
import sys
from datetime import datetime
from pathlib import Path

# Default directories — override with positional args:
#   ./add-frontmatter.py [assets_dir] [tidied_dir]
ASSETS_DIR = sys.argv[1] if len(sys.argv) > 1 else "assets"
TIDIED_DIR = sys.argv[2] if len(sys.argv) > 2 else "tidied"


def find_info_json(yt_id):
    """Find the info.json file for a given YouTube ID."""
    for f in os.listdir(ASSETS_DIR):
        if f.endswith(".info.json") and f"[{yt_id}]" in f:
            return os.path.join(ASSETS_DIR, f)
    return None


def format_date(upload_date_str):
    """Convert YYYYMMDD to YYYY-MM-DD."""
    if not upload_date_str or len(upload_date_str) != 8:
        return None
    return f"{upload_date_str[:4]}-{upload_date_str[4:6]}-{upload_date_str[6:8]}"


def extract_event_info(description):
    """Try to extract event name and date from description text.

    CUSTOMIZE: Add regex patterns for your conferences/events.
    Each pattern should capture: group(1) = event name, group(2) = date (optional).
    """
    if not description:
        return None, None

    # ========================================================================
    # CUSTOMIZE: Add patterns for your events. Examples:
    #
    #   # "presented at PyCon US 2024"
    #   m = re.search(r'at\s+(PyCon\s+\w+\s+\d{4})', description, re.IGNORECASE)
    #   if m:
    #       return m.group(1), None
    #
    #   # "KubeCon Europe 2024, Paris"
    #   m = re.search(r'(KubeCon\s+\w+\s+\d{4})', description, re.IGNORECASE)
    #   if m:
    #       return m.group(1), None
    # ========================================================================

    # Generic: look for "at EVENT_NAME 2024" or "EVENT_NAME Conference 2024"
    # This catches many common patterns
    m = re.search(r'(?:at|from)\s+([A-Z][\w\s]+?\d{4})\b', description)
    if m:
        return m.group(1).strip(), None

    return None, None


def infer_event_from_date(upload_date, description, channel):
    """Infer event from upload date when description doesn't help.

    CUSTOMIZE: Add date->event mappings for known events in your domain.
    """
    if not upload_date:
        return None, None

    # ========================================================================
    # CUSTOMIZE: Add date-based event inference. Examples:
    #
    #   # PyCon US 2024 - May 2024
    #   if upload_date.startswith("202405") and channel == "PyCon US":
    #       return "PyCon US 2024", "May 2024"
    #
    #   # KubeCon NA 2024 - November 2024
    #   if upload_date.startswith("202411"):
    #       return "KubeCon NA 2024", "November 2024"
    # ========================================================================

    return None, None


def format_duration(seconds):
    """Format seconds as human-readable duration."""
    if not seconds:
        return None
    h = int(seconds // 3600)
    m = int((seconds % 3600) // 60)
    s = int(seconds % 60)
    if h > 0:
        return f"{h}h{m:02d}m"
    return f"{m}m{s:02d}s"


def build_frontmatter(info):
    """Build YAML frontmatter from info.json data."""
    fm = {}

    fm["title"] = info.get("title", "")
    fm["youtube_url"] = info.get("webpage_url", "")
    fm["youtube_id"] = info.get("id", "")
    fm["channel"] = info.get("channel", "")

    upload_date = format_date(info.get("upload_date"))
    if upload_date:
        fm["upload_date"] = upload_date

    duration = info.get("duration")
    if duration:
        fm["duration_seconds"] = duration
        fm["duration"] = format_duration(duration)

    desc = info.get("description", "")
    if desc:
        # Clean up description for frontmatter (first meaningful line)
        desc_lines = [l.strip() for l in desc.split("\n") if l.strip() and not l.strip().startswith("http")]
        # Skip separator lines
        clean_lines = []
        for l in desc_lines:
            if l.startswith("--") or l.startswith("=="):
                break
            clean_lines.append(l)
        if clean_lines:
            fm["description"] = " ".join(clean_lines[:3])

    event, event_date = extract_event_info(desc)
    if not event:
        event, event_date = infer_event_from_date(
            info.get("upload_date"), desc, info.get("channel", "")
        )
    if event:
        fm["event"] = event
    if event_date:
        fm["event_date"] = event_date

    view_count = info.get("view_count")
    if view_count:
        fm["views"] = view_count

    return fm


def yaml_escape(val):
    """Escape a value for YAML."""
    if isinstance(val, (int, float)):
        return str(val)
    s = str(val)
    if any(c in s for c in ":{}[]#&*!|>'\"%@`,?") or s.startswith("-") or s.startswith(" "):
        return f'"{s}"'
    return s


def frontmatter_to_yaml(fm):
    """Convert frontmatter dict to YAML string."""
    lines = ["---"]
    for key, val in fm.items():
        lines.append(f"{key}: {yaml_escape(val)}")
    lines.append("---")
    return "\n".join(lines)


def main():
    if not os.path.isdir(TIDIED_DIR):
        print(f"Tidied directory not found: {TIDIED_DIR}")
        sys.exit(1)
    if not os.path.isdir(ASSETS_DIR):
        print(f"Assets directory not found: {ASSETS_DIR}")
        sys.exit(1)

    tidied_files = sorted(f for f in os.listdir(TIDIED_DIR) if f.endswith(".md"))
    print(f"Processing {len(tidied_files)} tidied transcripts\n")

    updated = 0
    for fname in tidied_files:
        filepath = os.path.join(TIDIED_DIR, fname)

        # Extract YouTube ID from filename: "Title [VIDEO_ID].md"
        m = re.search(r'\[([A-Za-z0-9_-]+)\]\.md$', fname)
        if not m:
            print(f"  SKIP (no YT ID): {fname[:60]}")
            continue
        yt_id = m.group(1)

        # Find info.json
        info_path = find_info_json(yt_id)
        if not info_path:
            print(f"  SKIP (no info.json): {yt_id}")
            continue

        with open(info_path) as f:
            info = json.load(f)

        fm = build_frontmatter(info)
        fm_yaml = frontmatter_to_yaml(fm)

        # Read existing file
        with open(filepath) as f:
            content = f.read()

        # Strip existing frontmatter if present
        if content.startswith("---\n"):
            end = content.index("\n---\n", 4)
            content = content[end + 5:]

        # Write with frontmatter
        with open(filepath, "w") as f:
            f.write(fm_yaml)
            f.write("\n")
            f.write(content)

        event_str = f" [{fm.get('event', '')}]" if fm.get("event") else ""
        print(f"  {fm.get('upload_date', '?')}{event_str} {fname[:55]}")
        updated += 1

    print(f"\nUpdated {updated}/{len(tidied_files)} files")


if __name__ == "__main__":
    main()
