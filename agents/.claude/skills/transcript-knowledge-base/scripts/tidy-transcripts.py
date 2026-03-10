#!/usr/bin/env -S uv run --script
# /// script
# dependencies = ["llm", "llm-openrouter"]
# ///
"""
Convert WhisperX JSON transcripts to tidied markdown via LLM.

Timestamps are stripped before sending to the LLM (which only sees numbered
turn markers), then re-inserted from the original WhisperX data afterward.
This avoids LLM-mangled timestamps and saves tokens.

CUSTOMIZATION:
- MODEL: Change to your preferred LLM
- SYSTEM_PROMPT rule 9: Add domain context for your topic
- TURN_INTERVAL: Adjust timestamp density (lower = more timestamps)
"""

import json
import os
import re
import sys
import time
from pathlib import Path

import llm

# Default directories — override with positional args:
#   ./tidy-transcripts.py [transcript_dir] [output_dir]
TRANSCRIPT_DIR = sys.argv[1] if len(sys.argv) > 1 else "transcripts"
OUTPUT_DIR = sys.argv[2] if len(sys.argv) > 2 else "tidied"

MODEL = "openrouter/google/gemini-3-flash-preview"

# Within a single speaker's monologue, start a new turn every N seconds
# so the output has reasonably frequent timestamp anchors.
# 30s gives ~2 timestamps/minute — dense enough for useful video navigation.
TURN_INTERVAL = 30

# ============================================================================
# CUSTOMIZE: Update rule 9 with your domain context.
# This helps the LLM preserve technical terms accurately.
# ============================================================================
SYSTEM_PROMPT = """\
You are given a transcript with numbered turn markers and speakers. \
Your task is to rewrite and lightly edit the transcript so that it is \
cleaner, more concise, and easier to read, while keeping the conversational feel.

Rules:
 1. Preserve every turn marker (e.g. «1», «2») and speaker label EXACTLY. \
Never add, remove, reorder, or renumber them.
 2. Remove filler words ("um," "uh," "like," "you know") unless they add meaning.
 3. Streamline repetition — cut redundant phrases but keep the core idea.
 4. Polish grammar and flow. Keep it sounding like spoken dialogue.
 5. Restructure long rambling passages into shorter, clearer sentences.
 6. Retain meaning and nuance. Do not drop substantive content.
 7. If a speaker references another person, preserve that explicitly.
 8. Smooth incomplete thoughts into full sentences reflecting the intended meaning.
 9. These are technical talks. Preserve all technical terms accurately. \
[TODO: Add domain context, e.g. "Topics include machine learning, transformer \
architectures, and distributed systems."]
10. Output ONLY the cleaned transcript. No preamble, no commentary."""


def format_ts(seconds):
    """Format seconds as M:SS or H:MM:SS."""
    if seconds is None:
        return "0:00"
    h = int(seconds // 3600)
    m = int((seconds % 3600) // 60)
    s = int(seconds % 60)
    if h > 0:
        return f"{h}:{m:02d}:{s:02d}"
    return f"{m}:{s:02d}"


def segments_to_turns(segments):
    """Group WhisperX segments into speaker turns.

    A new turn starts when:
    - The speaker changes, OR
    - TURN_INTERVAL seconds have passed since the turn started

    Returns list of dicts: {number, speaker, start, text}
    """
    turns = []
    current_speaker = None
    turn_start = -999
    turn_texts = []

    def flush():
        if turn_texts:
            turns.append({
                "number": len(turns) + 1,
                "speaker": current_speaker or "",
                "start": turn_start,
                "text": " ".join(turn_texts),
            })

    for seg in segments:
        speaker = seg.get("speaker", "")
        start = seg.get("start", 0) or 0
        text = seg.get("text", "").strip()
        if not text:
            continue

        speaker_changed = speaker != current_speaker
        time_elapsed = start - turn_start >= TURN_INTERVAL

        if speaker_changed or time_elapsed:
            flush()
            current_speaker = speaker
            turn_start = start
            turn_texts = [text]
        else:
            turn_texts.append(text)

    flush()
    return turns


def turns_to_llm_input(turns):
    """Format turns for the LLM: «N» Speaker: text"""
    lines = []
    for t in turns:
        marker = f"\u00ab{t['number']}\u00bb"
        if t["speaker"]:
            lines.append(f"{marker} {t['speaker']}: {t['text']}")
        else:
            lines.append(f"{marker} {t['text']}")
    return "\n\n".join(lines)


def tidy_via_llm(llm_input):
    """Send transcript through LLM for tidying."""
    model = llm.get_model(MODEL)
    response = model.prompt(llm_input, system=SYSTEM_PROMPT)
    return response.text().strip()


def reinsert_timestamps(tidied_text, turns):
    """Replace «N» markers in tidied text with [M:SS] timestamps.

    Returns (output_text, warnings_list).
    """
    # Build lookup: turn number -> timestamp string
    ts_lookup = {t["number"]: format_ts(t["start"]) for t in turns}
    expected = set(ts_lookup.keys())

    # Find all markers in the tidied text
    found = set()
    warnings = []

    def replace_marker(m):
        n = int(m.group(1))
        found.add(n)
        ts = ts_lookup.get(n)
        if ts is None:
            warnings.append(f"LLM invented marker «{n}» (no matching turn)")
            return m.group(0)  # leave as-is
        return f"[{ts}]"

    output = re.sub(r'\u00ab(\d+)\u00bb', replace_marker, tidied_text)

    # Check for dropped markers
    dropped = expected - found
    if dropped:
        warnings.append(f"LLM dropped {len(dropped)} marker(s): {sorted(dropped)}")

    # Check for any remaining «» that didn't match the pattern
    stray = re.findall(r'\u00ab[^»]*\u00bb', output)
    if stray:
        warnings.append(f"Unparsed markers in output: {stray}")

    return output, warnings


def write_output(final_text, json_data, json_path, output_path):
    """Write the tidied transcript as markdown."""
    name = json_data.get("file", Path(json_path).stem)
    yt_id = json_data.get("youtube_id", "")

    if "[" in name and name.endswith("]"):
        clean_name = name[: name.rindex("[")].strip(" -")
    else:
        clean_name = name

    with open(output_path, "w") as f:
        f.write(f"# {clean_name}\n\n")
        if yt_id:
            f.write(f"**YouTube**: https://youtube.com/watch?v={yt_id}\n\n")
        f.write(f"**Transcribed**: whisper-large-v3 + WhisperX align/diarize\n")
        f.write(f"**Tidied**: {time.strftime('%Y-%m-%d')} via {MODEL}\n")
        f.write("\n---\n\n")
        f.write(final_text)
        f.write("\n")


def main():
    os.makedirs(OUTPUT_DIR, exist_ok=True)

    if not os.path.isdir(TRANSCRIPT_DIR):
        print(f"Transcript directory not found: {TRANSCRIPT_DIR}")
        sys.exit(1)

    json_files = sorted(
        f for f in os.listdir(TRANSCRIPT_DIR)
        if f.endswith(".json") and not f.startswith("_")
    )

    print(f"Found {len(json_files)} JSON transcripts")
    print(f"Model: {MODEL}")
    print(f"Output: {OUTPUT_DIR}\n")

    results = []
    for i, fname in enumerate(json_files, 1):
        json_path = os.path.join(TRANSCRIPT_DIR, fname)
        stem = Path(fname).stem
        output_path = os.path.join(OUTPUT_DIR, f"{stem}.md")

        if os.path.exists(output_path) and os.path.getsize(output_path) > 100:
            print(f"[{i}/{len(json_files)}] SKIP: {stem[:65]}")
            results.append({"file": stem, "status": "skipped"})
            continue

        print(f"[{i}/{len(json_files)}] Processing: {stem[:65]}")

        with open(json_path) as f:
            json_data = json.load(f)

        segments = json_data.get("segments", [])
        if not segments:
            print("  Empty transcript, skipping")
            results.append({"file": stem, "status": "empty"})
            continue

        # Step 1: Group segments into turns
        turns = segments_to_turns(segments)
        llm_input = turns_to_llm_input(turns)

        word_count = len(llm_input.split())
        print(f"  {len(turns)} turns, {word_count} words, sending to LLM...")

        # Step 2: Tidy via LLM (no timestamps in the text)
        start_t = time.time()
        try:
            tidied = tidy_via_llm(llm_input)
            elapsed = time.time() - start_t

            # Step 3: Re-insert real timestamps from original data
            final_text, warnings = reinsert_timestamps(tidied, turns)
            for w in warnings:
                print(f"  WARNING: {w}")

            out_words = len(final_text.split())
            print(f"  Done in {elapsed:.1f}s ({out_words} words out)")

            # Step 4: Write output
            write_output(final_text, json_data, json_path, output_path)
            results.append({"file": stem, "status": "ok", "time": elapsed,
                            "warnings": len(warnings)})

        except Exception as e:
            elapsed = time.time() - start_t
            print(f"  ERROR after {elapsed:.1f}s: {e}")
            results.append({"file": stem, "status": "error", "error": str(e)})

    # Summary
    ok = sum(1 for r in results if r.get("status") == "ok")
    skipped = sum(1 for r in results if r.get("status") == "skipped")
    err = sum(1 for r in results if r.get("status") == "error")
    warn_count = sum(r.get("warnings", 0) for r in results)
    total_time = sum(r.get("time", 0) for r in results)
    print(f"\nDone! {ok} tidied, {skipped} skipped, {err} failed.")
    if warn_count:
        print(f"  {warn_count} timestamp warning(s) — check output above")
    print(f"Total LLM time: {total_time:.0f}s")
    print(f"Output in: {OUTPUT_DIR}")


if __name__ == "__main__":
    main()
