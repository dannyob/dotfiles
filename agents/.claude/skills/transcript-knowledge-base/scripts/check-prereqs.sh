#!/usr/bin/env bash
# Check prerequisites for the transcript knowledge base pipeline.
# Exit 0 if all good, exit 1 if anything missing.

set -euo pipefail

ok=0
warn=0
fail=0

check() {
    local name="$1" cmd="$2" install="$3"
    if eval "$cmd" >/dev/null 2>&1; then
        echo "  ✓ $name"
        ok=$((ok + 1))
    else
        echo "  ✗ $name — install with: $install"
        fail=$((fail + 1))
    fi
}

check_warn() {
    local name="$1" cmd="$2" install="$3"
    if eval "$cmd" >/dev/null 2>&1; then
        echo "  ✓ $name"
        ok=$((ok + 1))
    else
        echo "  ~ $name (optional) — $install"
        warn=$((warn + 1))
    fi
}

echo "=== Local Tools ==="
check "yt-dlp" "command -v yt-dlp" "brew install yt-dlp"
check "ffmpeg" "command -v ffmpeg" "brew install ffmpeg"
check "uv" "command -v uv" "brew install uv"

echo ""
echo "=== LLM API Key ==="
# Scripts use llm as a Python library (auto-installed by uv).
# Just need an API key in the llm key store or env.
KEY_DIR="${XDG_CONFIG_HOME:-$HOME/Library/Application Support}/io.datasette.llm"
if [ -f "$KEY_DIR/keys.json" ] && python3 -c "
import json, sys
keys = json.load(open(sys.argv[1]))
if any(k in keys for k in ('openrouter', 'gemini', 'openai')):
    sys.exit(0)
sys.exit(1)
" "$KEY_DIR/keys.json" 2>/dev/null; then
    echo "  ✓ API key in llm key store"
    ok=$((ok + 1))
elif [ -n "${OPENROUTER_KEY:-}" ] || [ -n "${GEMINI_API_KEY:-}" ] || [ -n "${OPENAI_API_KEY:-}" ]; then
    echo "  ✓ API key in environment"
    ok=$((ok + 1))
else
    echo "  ✗ No API key found"
    echo "      Option 1: llm keys set openrouter  (if llm CLI installed)"
    echo "      Option 2: export OPENROUTER_KEY=sk-or-..."
    fail=$((fail + 1))
fi

# Quick smoke test via uv + llm library
echo ""
echo "=== LLM Smoke Test ==="
if uv run --script - <<'PYEOF' 2>/dev/null
# /// script
# dependencies = ["llm", "llm-openrouter"]
# ///
import llm
m = llm.get_model("openrouter/google/gemini-3-flash-preview")
r = m.prompt("Reply with just the word 'ok'")
assert "ok" in r.text().lower()
PYEOF
then
    echo "  ✓ LLM responds via uv + llm library"
    ok=$((ok + 1))
else
    echo "  ~ LLM smoke test failed (may need different model or API key)"
    warn=$((warn + 1))
fi

echo ""
echo "=== GPU Transcription (optional — needed for step 3) ==="
check_warn "runpodctl" "command -v runpodctl" "see runpod.io docs for CLI install"
check_warn "NVIDIA GPU" "command -v nvidia-smi && nvidia-smi" "use RunPod/Lambda for remote GPU"

echo ""
echo "=== Summary ==="
echo "  $ok passed, $warn optional, $fail required"

if [ "$fail" -gt 0 ]; then
    echo ""
    echo "Fix the required items above before proceeding."
    exit 1
else
    echo ""
    echo "Ready to go!"
    exit 0
fi
