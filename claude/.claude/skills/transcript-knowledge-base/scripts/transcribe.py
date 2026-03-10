#!/usr/bin/env python3
"""
Batch transcription using faster-whisper + WhisperX on GPU.
- faster-whisper: transcription with initial_prompt for hotword biasing
- WhisperX: word-level alignment + pyannote diarization
- Post-processing: correct common domain-specific misrecognitions

CUSTOMIZATION REQUIRED:
- INITIAL_PROMPT: Add 50-100 domain terms (proper nouns, acronyms, jargon)
- CORRECTIONS: Add regex patterns for systematic misrecognitions
  (run a first pass without corrections, grep output for errors, then add fixes)

Runs on GPU (RunPod, Lambda, or local NVIDIA). See references/transcription-setup.md.
"""

import gc
import glob
import json
import os
import re
import sys
import time
from pathlib import Path

import numpy as np
import torch
import whisperx
from faster_whisper import WhisperModel

AUDIO_DIR = "/workspace/audio"
TRANSCRIPT_DIR = "/workspace/transcripts"
DEVICE = "cuda"
COMPUTE_TYPE = "float16"
BATCH_SIZE = 16
MODEL_SIZE = "large-v3"

HF_TOKEN = os.environ.get("HF_TOKEN", "")

# ============================================================================
# CUSTOMIZE: Vocabulary prompt — biases the Whisper decoder toward these terms.
# Add proper nouns, acronyms, technical jargon, and speaker names that Whisper
# commonly gets wrong. This acts as fake "prior context" for decoder biasing.
#
# Example for a machine learning conference:
#   "transformer, attention mechanism, BERT, GPT, fine-tuning, LoRA, ..."
# ============================================================================
INITIAL_PROMPT = (
    # TODO: Replace with your domain-specific terms
    "Example Term, Another Term, PersonName, "
    "API, SDK, CLI, "
)

# ============================================================================
# CUSTOMIZE: Post-processing corrections for systematic misrecognitions.
# Each entry is (regex_pattern, replacement). Case-insensitive by default.
#
# Run a first pass without corrections, then grep the output for common errors
# and add them here. Examples:
#   (r'\blib P2P\b', 'libp2p'),       # Whisper splits compound words
#   (r'\bCuber Nettis\b', 'Kubernetes'),  # Phonetic mishearing
# ============================================================================
CORRECTIONS = [
    # TODO: Add project-specific corrections after first transcription pass
    # (r'\bMisheard Term\b', 'Correct Term'),
]


def apply_corrections(text: str) -> str:
    """Apply domain-specific post-processing corrections."""
    for pattern, replacement in CORRECTIONS:
        text = re.sub(pattern, replacement, text, flags=re.IGNORECASE)
    return text


def format_timestamp(seconds: float) -> str:
    h = int(seconds // 3600)
    m = int((seconds % 3600) // 60)
    s = int(seconds % 60)
    if h > 0:
        return f"{h}:{m:02d}:{s:02d}"
    return f"{m}:{s:02d}"


def transcribe_file(
    audio_path: str,
    fw_model: WhisperModel,
    align_model,
    align_metadata,
    diarize_model,
) -> dict:
    """
    Transcribe with faster-whisper (for initial_prompt support),
    then align + diarize with WhisperX.
    """
    # 1. Transcribe with faster-whisper (supports initial_prompt)
    segments_gen, info = fw_model.transcribe(
        audio_path,
        language="en",
        initial_prompt=INITIAL_PROMPT,
        beam_size=5,
        vad_filter=True,
        vad_parameters=dict(min_silence_duration_ms=500),
    )

    # Collect segments into the format WhisperX expects
    raw_segments = []
    for seg in segments_gen:
        raw_segments.append({
            "start": seg.start,
            "end": seg.end,
            "text": apply_corrections(seg.text),
        })

    # 2. Align with WhisperX (word-level timestamps)
    audio = whisperx.load_audio(audio_path)
    result = whisperx.align(
        raw_segments,
        align_model,
        align_metadata,
        audio,
        DEVICE,
        return_char_alignments=False,
    )

    # 3. Diarize if available
    if diarize_model is not None:
        try:
            diarize_segments = diarize_model(audio)
            result = whisperx.assign_word_speakers(diarize_segments, result)
        except Exception as e:
            print(f"  Diarization failed (continuing without): {e}")

    return result


def write_markdown(result: dict, audio_file: str, output_path: str):
    name = Path(audio_file).stem
    yt_id = ""
    if "[" in name and name.endswith("]"):
        yt_id = name[name.rindex("[") + 1 : -1]
        clean_name = name[: name.rindex("[")].strip(" -")
    else:
        clean_name = name

    segments = result.get("segments", [])

    with open(output_path, "w") as f:
        f.write(f"# {clean_name}\n\n")
        if yt_id:
            f.write(f"**YouTube**: https://youtube.com/watch?v={yt_id}\n\n")
        f.write(f"**Transcribed**: {time.strftime('%Y-%m-%d %H:%M UTC')}\n")
        f.write(f"**Model**: whisper-large-v3 + WhisperX align/diarize\n")

        if segments:
            duration = segments[-1].get("end", 0)
            f.write(f"**Duration**: {format_timestamp(duration)}\n")

        has_speakers = any("speaker" in s for s in segments)
        if has_speakers:
            speakers = set(s.get("speaker", "") for s in segments if s.get("speaker"))
            f.write(f"**Speakers**: {len(speakers)}\n")

        f.write("\n---\n\n")
        f.write("## Transcript\n\n")

        current_speaker = None
        for seg in segments:
            ts = format_timestamp(seg.get("start", 0))
            speaker = seg.get("speaker", "")
            text = seg.get("text", "").strip()

            if has_speakers and speaker and speaker != current_speaker:
                current_speaker = speaker
                f.write(f"\n**{speaker}** ({ts})\n\n")
            elif not has_speakers:
                f.write(f"[{ts}] ")

            f.write(f"{text}\n")

        f.write("\n")

    # Raw JSON for later processing
    json_path = output_path.replace(".md", ".json")
    with open(json_path, "w") as f:
        json.dump(
            {
                "file": name,
                "youtube_id": yt_id,
                "segments": [
                    {
                        "start": s.get("start"),
                        "end": s.get("end"),
                        "text": s.get("text", "").strip(),
                        "speaker": s.get("speaker", ""),
                    }
                    for s in segments
                ],
            },
            f,
            indent=2,
        )


def main():
    os.makedirs(TRANSCRIPT_DIR, exist_ok=True)

    audio_files = sorted(
        glob.glob(os.path.join(AUDIO_DIR, "*.opus"))
        + glob.glob(os.path.join(AUDIO_DIR, "*.mp3"))
        + glob.glob(os.path.join(AUDIO_DIR, "*.wav"))
    )
    if not audio_files:
        print(f"No audio files found in {AUDIO_DIR}")
        sys.exit(1)

    print(f"Found {len(audio_files)} audio files")
    print(f"Model: {MODEL_SIZE}, Device: {DEVICE}, Compute: {COMPUTE_TYPE}")
    print(f"HF Token: {'set' if HF_TOKEN else 'NOT SET (no diarization)'}")
    print(f"Initial prompt: {INITIAL_PROMPT[:80]}...")
    print(f"Post-processing corrections: {len(CORRECTIONS)} rules")
    print()

    # Load faster-whisper model (for transcription with initial_prompt)
    print("Loading faster-whisper model...")
    fw_model = WhisperModel(MODEL_SIZE, device=DEVICE, compute_type=COMPUTE_TYPE)

    # Load WhisperX alignment model
    print("Loading alignment model...")
    align_model, align_metadata = whisperx.load_align_model(
        language_code="en", device=DEVICE
    )

    # Load diarization model if token available
    diarize_model = None
    if HF_TOKEN:
        print("Loading diarization model (pyannote)...")
        from whisperx.diarize import DiarizationPipeline
        diarize_model = DiarizationPipeline(token=HF_TOKEN, device=DEVICE)
        print("Diarization model loaded!")
    else:
        print("No HF token — skipping diarization")

    print("All models loaded!\n")

    results = []
    for i, audio_file in enumerate(audio_files, 1):
        name = Path(audio_file).stem
        output_path = os.path.join(TRANSCRIPT_DIR, f"{name}.md")

        if os.path.exists(output_path) and os.path.getsize(output_path) > 100:
            print(f"[{i}/{len(audio_files)}] SKIP: {name[:70]}")
            results.append({"file": name, "status": "skipped"})
            continue

        print(f"[{i}/{len(audio_files)}] Transcribing: {name[:70]}")
        start = time.time()

        try:
            result = transcribe_file(
                audio_file, fw_model, align_model, align_metadata, diarize_model
            )
            elapsed = time.time() - start

            n_segments = len(result.get("segments", []))
            has_speakers = any("speaker" in s for s in result.get("segments", []))
            print(
                f"  Done in {elapsed:.1f}s — {n_segments} segments, "
                f"speakers={'yes' if has_speakers else 'no'}"
            )

            write_markdown(result, audio_file, output_path)
            results.append({"file": name, "status": "ok", "time": elapsed})

            gc.collect()
            torch.cuda.empty_cache()

        except Exception as e:
            elapsed = time.time() - start
            print(f"  ERROR after {elapsed:.1f}s: {e}")
            import traceback
            traceback.print_exc()
            results.append({"file": name, "status": "error", "error": str(e)})

    # Summary
    summary_path = os.path.join(TRANSCRIPT_DIR, "_summary.json")
    with open(summary_path, "w") as f:
        json.dump(results, f, indent=2)

    ok = sum(1 for r in results if r.get("status") == "ok")
    skipped = sum(1 for r in results if r.get("status") == "skipped")
    err = sum(1 for r in results if r["status"] not in ("ok", "skipped"))
    total_time = sum(r.get("time", 0) for r in results)
    print(f"\nDone! {ok} transcribed, {skipped} skipped, {err} failed.")
    print(f"Total transcription time: {total_time:.0f}s ({total_time/60:.1f}min)")
    print(f"Transcripts in: {TRANSCRIPT_DIR}")


if __name__ == "__main__":
    main()
