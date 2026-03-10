# GPU Transcription Setup

## RunPod Setup

### Create Pod

```bash
# PyTorch 2.4 template is recommended (pre-cached, fast startup)
runpodctl create pod \
  --name transcribe \
  --gpuType "NVIDIA RTX A5000" \
  --gpuCount 1 \
  --templateId 29eoq2gx7x \
  --disk 50 \
  --ports "22/tcp,8888/http"
```

Wait for pod to reach RUNNING state:
```bash
runpodctl get pod
```

### Upload Audio Files

```bash
# Get pod SSH info
runpodctl get pod  # note the external IP and port

# Upload audio files
rsync -avz -e "ssh -p PORT" audio/ root@IP:/workspace/audio/
```

### Install Dependencies

SSH into pod and run:
```bash
pip install faster-whisper whisperx torch torchvision torchaudio
pip install transformers==4.44.2  # whisperx needs this specific version
```

### Upload and Run Transcription Script

```bash
scp -P PORT transcribe.py root@IP:/workspace/
ssh -p PORT root@IP "cd /workspace && python3 transcribe.py"
```

### Download Results

```bash
rsync -avz -e "ssh -p PORT" root@IP:/workspace/transcripts/ transcripts/
```

### Delete Pod

```bash
runpodctl remove pod POD_ID
```

## Dependencies on GPU Pod

| Package | Version | Notes |
|---------|---------|-------|
| faster-whisper | latest | CTranslate2-based Whisper, supports `initial_prompt` |
| whisperx | latest | Word-level alignment + diarization |
| torch | 2.4+ | Pre-installed on PyTorch template |
| torchvision | match torch | May need `pip install --force-reinstall` |
| torchaudio | match torch | Same |
| transformers | 4.44.2 | Newer versions break whisperx Pipeline import |
| pyannote.audio | latest | Installed by whisperx for diarization |

## Diarization Setup

Speaker diarization requires a HuggingFace token with access to pyannote models:

1. Create HF token at huggingface.co/settings/tokens
2. Accept model terms at:
   - huggingface.co/pyannote/speaker-diarization-community-1
   - huggingface.co/pyannote/segmentation-3.0
3. Set token: `export HF_TOKEN=hf_xxx` on the GPU pod

## transcribe.py Template Structure

The transcription script has three customization points:

### INITIAL_PROMPT (domain vocabulary)
```python
INITIAL_PROMPT = """
Proper nouns and technical terms for this domain:
Term1, Term2, PersonName, Acronym, ...
"""
```
This is prepended as fake "prior context" to bias Whisper's decoder toward your vocabulary.

### CORRECTIONS (post-processing regex)
```python
CORRECTIONS = [
    (r'\bCommon Misheard\b', 'Correct Term'),
    (r'\bCuber Nettis\b', 'Kubernetes'),
    ...
]
```
Run a first pass without corrections, grep output for systematic errors, then add fixes.

### Pipeline stages
1. **faster-whisper** transcribes with initial_prompt + VAD filter
2. **WhisperX** aligns segments to word-level timestamps
3. **pyannote** diarizes (assigns speakers) — optional, needs HF_TOKEN

The alignment step occasionally fails with ZeroDivisionError on some files. Retry without alignment (transcription + diarization only) as fallback.

## Cost Estimates

- RunPod A5000: ~$0.59/hr on-demand
- ~8 hours of audio: ~15 min transcription time
- Two runs (initial + diarization): ~$0.60 total
