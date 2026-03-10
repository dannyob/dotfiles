---
name: transcript-knowledge-base
description: Use when user wants to build a browsable knowledge base from YouTube videos - transcribing, diarizing, tidying, and generating an interactive static site with embedded player and clickable timestamps. Also use when user has a collection of talks, lectures, or conference videos to make searchable and readable.
---

# Transcript Knowledge Base

Build an interactive, searchable knowledge base from a set of YouTube videos. Produces diarized, LLM-tidied transcripts with clickable timestamps and an embedded YouTube player, all in a single static HTML file.

## When NOT to Use

- Single video transcription (just use WhisperX directly)
- User only needs raw subtitles (just use yt-dlp `--write-subs`)
- Live/streaming transcription

## Prerequisites

Before starting, verify these tools are available. Run `scripts/check-prereqs.sh` or check manually:

| Tool | Check | Install |
|------|-------|---------|
| yt-dlp | `yt-dlp --version` | `brew install yt-dlp` |
| ffmpeg | `ffmpeg -version` | `brew install ffmpeg` |
| uv | `uv --version` | `brew install uv` |
| GPU access | RunPod / Lambda / local NVIDIA | See `references/transcription-setup.md` |
| runpodctl | `runpodctl --version` (if using RunPod) | See runpod.io docs |

**LLM access**: The tidying step uses Simon Willison's `llm` as a Python library (not CLI). Scripts declare `llm` + provider plugins as inline PEP 723 dependencies — `uv run --script` handles installation automatically. You just need an API key configured:

```bash
# If llm CLI is installed (optional, just for key management):
llm keys set openrouter    # paste OpenRouter API key
# Or set env var directly:
export OPENROUTER_KEY=sk-or-...
```

The `llm` library reads keys from `~/Library/Application Support/io.datasette.llm/keys.json` (macOS) or `~/.config/io.datasette.llm/keys.json` (Linux). Recommended models:
- `openrouter/google/gemini-3-flash-preview` — fast, cheap, good at instruction-following
- `gemini/gemini-2.5-flash` — direct Gemini API, also good

## Pipeline Overview

```
1. COLLECT    YouTube URLs → yt-dlp → MKV + metadata (.info.json)
2. EXTRACT    MKV → ffmpeg stream copy → audio (opus/aac)
3. TRANSCRIBE audio → WhisperX on GPU → segments JSON + raw markdown
4. IDENTIFY   SPEAKER_XX → manual/contextual → real names
5. TIDY       raw transcript → LLM via `llm` CLI → cleaned markdown
6. ENRICH     + YAML frontmatter from YouTube metadata
7. CURATE     user marks `important: N` in frontmatter
8. GENERATE   markdown files → single-file static HTML site
9. PUBLISH    (optional) pin to IPFS, deploy to hosting
```

Steps 1-2 and 5-8 run locally. Step 3 requires a GPU (remote or local). Step 4 is interactive. Each step is idempotent — safe to re-run.

## Template Scripts

**⚠️ These are TEMPLATES, not ready-to-run scripts.** Each one contains `# CUSTOMIZE` blocks with TODO placeholders that MUST be filled in for your specific topic. Running them unmodified will produce empty or poor-quality results (blank hotword lists, no speaker mappings, generic system prompts). Copy them into your project directory and edit the marked sections before running.

All pipeline scripts are bundled in `scripts/` within this skill directory.

| Script | Purpose | Customization |
|--------|---------|---------------|
| `scripts/transcribe.py` | GPU transcription via faster-whisper + WhisperX | `INITIAL_PROMPT` (domain terms), `CORRECTIONS` (regex fixes) |
| `scripts/replace_speakers.py` | Replace SPEAKER_XX → real names | `SPEAKER_MAP` — always project-specific |
| `scripts/tidy-transcripts.py` | LLM-based transcript cleanup | `SYSTEM_PROMPT` rule 9 (domain context), `MODEL` |
| `scripts/add-frontmatter.py` | Inject YAML frontmatter from info.json | `extract_event_info()`, `infer_event_from_date()` |
| `scripts/generate-site.py` | Single-file HTML site generator | `--title`, `--subtitle`, `--accent`, `--sidebar` CLI args |
| `scripts/check-prereqs.sh` | Verify local tools are installed | Usually no changes needed |

Each script has `CUSTOMIZE` comments marking the sections to adapt. Read them before generating new ones — adapt, don't copy blindly.

## Step-by-Step

### 1. Collect Videos

```bash
yt-dlp --write-info-json --write-auto-subs --sub-lang en \
  -o "assets/%(title)s [%(id)s].%(ext)s" URL1 URL2...
```

For playlists: `yt-dlp --write-info-json --write-auto-subs <playlist-url>`

### 2. Extract Audio

Stream-copy (near-instant, no re-encoding — never use MP3 re-encode):

```bash
mkdir -p audio
for f in assets/*.mkv; do
  base="$(basename "$f" .mkv)"
  ffmpeg -i "$f" -vn -c:a copy "audio/${base}.opus" -y
done
```

### 3. Transcribe on GPU

Requires GPU. Copy `scripts/transcribe.py` into the project and customize:
- **INITIAL_PROMPT**: 50-100 domain terms (proper nouns, acronyms, jargon) as fake "prior context" for Whisper decoder biasing
- **CORRECTIONS**: Regex list for systematic misrecognitions (run first pass to discover these)
- **HF_TOKEN**: For diarization. User must accept model terms at huggingface.co/pyannote/speaker-diarization-community-1

Key dependencies on GPU pod: `faster-whisper`, `whisperx`, `torch`, `transformers==4.44.2`

Read `references/transcription-setup.md` for RunPod setup details.

**Output**: `transcripts/*.json` (segments with timestamps, speakers, text) + `transcripts/*.md`

### 4. Identify Speakers

Diarization produces anonymous SPEAKER_XX labels. To identify:

1. Read first few minutes of each transcript — speakers often self-introduce
2. Cross-reference with video titles/descriptions
3. Copy `scripts/replace_speakers.py` and fill in the `SPEAKER_MAP` with per-video mappings
4. Solo talks: SPEAKER_00 = presenter, others = "Audience"
5. Pyannote assigns IDs arbitrarily — SPEAKER_00 is NOT always the first speaker

This step is interactive — confirm identifications with the user.

### 5. Tidy Transcripts via LLM

Copy `scripts/tidy-transcripts.py` into the project. It uses `llm` as a Python library with inline PEP 723 deps (`llm`, `llm-openrouter`). Just run with `uv run --script` — dependencies install automatically. Ensure an API key is configured (see Prerequisites).

**Timestamp handling**: The script strips timestamps before sending to the LLM. WhisperX segments are grouped into speaker turns, each tagged with a numbered marker (`«1»`, `«2»`, ...) instead of `[M:SS]`. After tidying, markers are replaced with the real timestamps from the original WhisperX data. This avoids LLM-mangled timestamps, saves tokens, and guarantees correct timecodes.

Customize rule 9 of the system prompt with domain context.

### 6. Enrich with Metadata

Fetch any missing `.info.json` files:
```bash
yt-dlp --skip-download --write-info-json -o "assets/%(title)s [%(id)s].%(ext)s" URL...
```

Then run the project copy of `add-frontmatter.py` to inject YAML frontmatter (title, youtube_url, upload_date, duration, description, event, views). Customize `extract_event_info()` and `infer_event_from_date()` for your conference dates.

### 7. Curate

User edits frontmatter to prioritize videos:

```yaml
important: 1    # Number = priority (1 highest), appears at top
important: true # Boolean = emphasized, no priority ordering
```

### 8. Generate Static Site

Run `generate-site.py` with project title/subtitle and brand colors:

```bash
uv run --script generate-site.py --title "My Conference" --subtitle "Talk transcripts" --accent "#0090FF"
```

Produces a single self-contained HTML file with YouTube IFrame API, clickable timestamps, active line highlighting, search, and responsive layout.

### 9. Publish (optional)

IPFS/Filecoin: `filecoin-pin add index.html --auto-fund` (if filecoin-pin skill available)
Static hosting: rsync, GitHub Pages, Netlify, etc.

## Common Issues

| Problem | Fix |
|---------|-----|
| Audio extraction takes forever | Use `-c:a copy` (stream copy), never re-encode |
| WhisperX `transformers` error | `pip install transformers==4.44.2` |
| Diarization 403 error | Accept pyannote model terms on HuggingFace |
| SPEAKER_XX ≠ SPEAKER_00 at start | Pyannote assigns IDs arbitrarily; check who speaks first |
| LLM drops turn markers | Check for `«N»` markers in output; script warns on dropped markers |
| No API key for LLM | `llm keys set openrouter` or `export OPENROUTER_KEY=...` |
| `uv` not found | `brew install uv` or `curl -LsSf https://astral.sh/uv/install.sh \| sh` |

## Adapting for a New Topic

1. **Gather URLs** — playlist or manual list
2. **Build hotword list** — domain terms for INITIAL_PROMPT
3. **First pass** — transcribe, then grep for misrecognitions to build CORRECTIONS
4. **Speaker mapping** — project-specific, do after transcription
5. **Event inference** — add date→event mappings for your conferences
6. **Branding** — pass `--title`, `--subtitle`, `--accent`, `--sidebar` to generate-site.py
