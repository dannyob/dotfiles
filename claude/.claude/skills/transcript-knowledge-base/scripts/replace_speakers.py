#!/usr/bin/env -S uv run --script
# /// script
# requires-python = ">=3.10"
# ///
"""
Replace SPEAKER_XX labels with identified names in transcript files.

CUSTOMIZATION REQUIRED — this script is ALWAYS project-specific.
After transcription + diarization, read the first few minutes of each transcript
to identify speakers, then build the SPEAKER_MAP below.

Tips:
- Speakers often self-introduce in the first 30 seconds
- Cross-reference with video titles and descriptions
- Pyannote assigns SPEAKER_XX IDs arbitrarily — SPEAKER_00 is NOT always first
- Solo talks: SPEAKER_00 = presenter, others = "Audience"
- Panel discussions need careful identification from context
"""

import os
import re
import json
import sys
from pathlib import Path

# Default to ./transcripts relative to project root, override with argv
TRANSCRIPT_DIR = sys.argv[1] if len(sys.argv) > 1 else "transcripts"

# ============================================================================
# CUSTOMIZE: Per-file speaker mappings.
# Key: substring that uniquely matches the filename
# Value: dict of SPEAKER_XX -> real name
#
# Example:
#   "Keynote Opening": {
#       "SPEAKER_00": "Jane Smith",
#       "SPEAKER_01": "Moderator",
#       "SPEAKER_02": "Audience",
#   },
#   "Panel Discussion": {
#       "SPEAKER_00": "Alice Chen",
#       "SPEAKER_01": "Bob Garcia",
#       "SPEAKER_02": "Carol Kim",
#       "SPEAKER_03": "Audience",
#   },
# ============================================================================
SPEAKER_MAP = {
    # TODO: Fill in after reviewing transcripts
    # "unique filename substring": {
    #     "SPEAKER_00": "Real Name",
    #     "SPEAKER_01": "Other Name",
    # },
}


def find_mapping(filename):
    """Find the speaker mapping for a given filename."""
    for key, mapping in SPEAKER_MAP.items():
        if key in filename:
            return mapping
    return None


def replace_in_md(filepath, mapping):
    """Replace SPEAKER_XX with names in markdown files."""
    with open(filepath, "r") as f:
        content = f.read()

    original = content
    for speaker_id, name in mapping.items():
        # Replace **SPEAKER_XX** format in markdown
        content = content.replace(f"**{speaker_id}**", f"**{name}**")

    if content != original:
        with open(filepath, "w") as f:
            f.write(content)
        return True
    return False


def replace_in_json(filepath, mapping):
    """Replace SPEAKER_XX with names in JSON files."""
    with open(filepath, "r") as f:
        data = json.load(f)

    changed = False
    for seg in data.get("segments", []):
        speaker = seg.get("speaker", "")
        if speaker in mapping:
            seg["speaker"] = mapping[speaker]
            changed = True

    if changed:
        with open(filepath, "w") as f:
            json.dump(data, f, indent=2)

    return changed


def main():
    if not os.path.isdir(TRANSCRIPT_DIR):
        print(f"Transcript directory not found: {TRANSCRIPT_DIR}")
        sys.exit(1)

    md_files = sorted(f for f in os.listdir(TRANSCRIPT_DIR) if f.endswith(".md") and not f.startswith("_"))
    json_files = sorted(f for f in os.listdir(TRANSCRIPT_DIR) if f.endswith(".json") and not f.startswith("_"))

    print(f"Found {len(md_files)} markdown files, {len(json_files)} JSON files\n")

    md_changed = 0
    json_changed = 0
    unmatched = []

    for fname in md_files:
        mapping = find_mapping(fname)
        if mapping is None:
            unmatched.append(fname)
            continue
        filepath = os.path.join(TRANSCRIPT_DIR, fname)
        if replace_in_md(filepath, mapping):
            md_changed += 1
            print(f"  MD: {fname[:70]}...")

    for fname in json_files:
        mapping = find_mapping(fname)
        if mapping is None:
            continue
        filepath = os.path.join(TRANSCRIPT_DIR, fname)
        if replace_in_json(filepath, mapping):
            json_changed += 1
            print(f"  JSON: {fname[:70]}...")

    print(f"\nUpdated {md_changed} markdown files, {json_changed} JSON files")

    if unmatched:
        print(f"\nUnmatched files ({len(unmatched)}):")
        for f in unmatched:
            print(f"  - {f}")

    # Also update the Speakers count in md files to list names
    print("\nUpdating speaker lists in headers...")
    for fname in md_files:
        mapping = find_mapping(fname)
        if mapping is None:
            continue
        filepath = os.path.join(TRANSCRIPT_DIR, fname)
        named = [n for n in mapping.values() if n != "Audience"]
        if len(named) > 1:
            with open(filepath, "r") as f:
                content = f.read()
            # Replace "**Speakers**: N" with named list
            old_pat = re.compile(r'\*\*Speakers\*\*: \d+')
            names_str = ", ".join(named)
            new_val = f"**Speakers**: {names_str}"
            new_content = old_pat.sub(new_val, content)
            if new_content != content:
                with open(filepath, "w") as f:
                    f.write(new_content)
                print(f"  Header: {fname[:60]}...")


if __name__ == "__main__":
    main()
