#!/usr/bin/env -S uv run --script
# /// script
# requires-python = ">=3.10"
# dependencies = ["pyyaml"]
# ///
"""
Generate a single-file static HTML site from tidied video transcripts.

Reads all .md files with YAML frontmatter in the tidied directory, produces
index.html with:
- Sidebar video list grouped by event
- Embedded YouTube player with IFrame API
- Clickable timestamps that seek the player
- Active line highlighting during playback
- Full-text search across videos and transcripts
- Mini player that pins to corner when scrolling

Usage:
  ./generate-site.py [options]

Options:
  --title TEXT       Site title (default: "Video Transcripts")
  --subtitle TEXT    Site subtitle (default: "Interactive transcript browser")
  --tidied-dir DIR   Source directory (default: ./tidied)
  --output FILE      Output file (default: ./index.html)
  --accent COLOR     Accent color hex (default: #0090FF)
  --sidebar COLOR    Sidebar background hex (default: #192C39)
"""

import argparse
import json
import re
import html
from pathlib import Path
import yaml


def parse_args():
    p = argparse.ArgumentParser(description="Generate transcript knowledge base site")
    p.add_argument("--title", default="Video Transcripts", help="Site title")
    p.add_argument("--subtitle", default="Interactive transcript browser", help="Site subtitle")
    p.add_argument("--tidied-dir", default="tidied", help="Source directory with tidied .md files")
    p.add_argument("--output", default="index.html", help="Output HTML file")
    p.add_argument("--accent", default="#0090FF", help="Accent color (hex)")
    p.add_argument("--sidebar", default="#192C39", help="Sidebar background color (hex)")
    return p.parse_args()


def parse_frontmatter(text: str) -> tuple[dict, str]:
    """Extract YAML frontmatter and body from markdown text."""
    if not text.startswith("---"):
        return {}, text
    end = text.index("---", 3)
    fm = yaml.safe_load(text[3:end])
    body = text[end + 3:].strip()
    return fm or {}, body


def parse_transcript(body: str) -> list[dict]:
    """Parse transcript lines into structured segments.

    Each segment: {time_seconds, time_display, speaker, text}

    Handles two patterns:
    1. `[M:SS] Speaker: text` — new speaker turn
    2. `[M:SS] text` (no speaker) — continuation, inherits previous speaker

    Both patterns can appear at start of line or inline within paragraphs.
    """
    marker_pat = re.compile(
        r'\[(\d+):(\d{2})\]\s+(?:([A-Z][^:\[\]]{0,40}):\s+)?',
    )
    markers = list(marker_pat.finditer(body))

    if not markers:
        return []

    segments = []
    current_speaker = "Speaker"

    for i, m in enumerate(markers):
        minutes, seconds = int(m.group(1)), int(m.group(2))
        total_seconds = minutes * 60 + seconds

        if m.group(3):
            current_speaker = m.group(3).strip()

        start = m.end()
        end = markers[i + 1].start() if i + 1 < len(markers) else len(body)
        raw_text = body[start:end].strip()
        raw_text = re.sub(r'\s*\n\s*', ' ', raw_text)

        if not raw_text:
            continue

        segments.append({
            "time_seconds": total_seconds,
            "time_display": f"{minutes}:{seconds:02d}",
            "speaker": current_speaker,
            "text": raw_text,
        })

    return segments


def sort_key_for_important(fm: dict):
    imp = fm.get("important")
    if isinstance(imp, (int, float)):
        return (0, imp)
    elif imp:
        return (1, 0)
    else:
        return (2, 0)


def event_sort_key(event_name: str, videos: list[dict]) -> str:
    dates = [v["fm"].get("upload_date", "9999-99-99") for v in videos]
    earliest = min(str(d) for d in dates)
    return earliest


def load_videos(tidied_dir: Path) -> list[dict]:
    videos = []
    for md_file in sorted(tidied_dir.glob("*.md")):
        text = md_file.read_text(encoding="utf-8")
        fm, body = parse_frontmatter(text)
        if not fm.get("youtube_id"):
            continue
        segments = parse_transcript(body)
        videos.append({
            "fm": fm,
            "segments": segments,
            "slug": md_file.stem,
        })
    return videos


def group_and_sort(videos: list[dict]) -> list[tuple[str, list[dict]]]:
    recommended = []
    groups: dict[str, list[dict]] = {}
    for v in videos:
        if v["fm"].get("important"):
            recommended.append(v)
        else:
            event = v["fm"].get("event", "Other")
            groups.setdefault(event, []).append(v)

    recommended.sort(key=lambda v: (
        sort_key_for_important(v["fm"]),
        str(v["fm"].get("upload_date", "9999-99-99")),
    ))

    for event, vids in groups.items():
        vids.sort(key=lambda v: str(v["fm"].get("upload_date", "9999-99-99")))

    sorted_groups = sorted(
        groups.items(),
        key=lambda pair: event_sort_key(pair[0], pair[1]),
    )

    if recommended:
        sorted_groups.insert(0, ("Recommended", recommended))

    return sorted_groups


def build_video_data(groups: list[tuple[str, list[dict]]]) -> list[dict]:
    all_videos = []
    for event, vids in groups:
        for v in vids:
            fm = v["fm"]
            slug = re.sub(r'[^a-z0-9]+', '-', fm.get("title", "untitled").lower()).strip('-')
            all_videos.append({
                "id": fm["youtube_id"],
                "slug": slug,
                "title": fm.get("title", "Untitled"),
                "event": event,
                "duration": fm.get("duration", ""),
                "duration_seconds": fm.get("duration_seconds", 0),
                "description": fm.get("description", ""),
                "upload_date": str(fm.get("upload_date", "")),
                "event_date": fm.get("event_date", ""),
                "important": fm.get("important"),
                "segments": v["segments"],
            })
    return all_videos


def build_html(video_data: list, title: str, subtitle: str,
               accent: str, sidebar_bg: str) -> str:
    """Build the complete HTML page."""

    # Compute a few derived colors from the accent
    video_json = json.dumps(video_data, ensure_ascii=False)

    return f"""\
<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>{html.escape(title)}</title>
<style>
* {{ margin: 0; padding: 0; box-sizing: border-box; }}

:root {{
  --primary-blue: {accent};
  --dark-navy: {sidebar_bg};
  --medium-blue: #044498;
  --light-blue: #44B4F4;
  --light-gray: #A0B5C2;
  --bg: #f8f9fa;
  --sidebar-bg: {sidebar_bg};
  --sidebar-width: 380px;
}}

body {{
  font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif;
  background: var(--bg);
  color: #222;
  height: 100vh;
  overflow: hidden;
  display: flex;
}}

/* Sidebar */
.sidebar {{
  width: var(--sidebar-width);
  min-width: var(--sidebar-width);
  background: var(--sidebar-bg);
  color: #dde4ea;
  overflow-y: auto;
  height: 100vh;
  display: flex;
  flex-direction: column;
}}

.sidebar-header {{
  padding: 20px 18px 14px;
  border-bottom: 1px solid rgba(255,255,255,0.1);
  flex-shrink: 0;
}}

.sidebar-header h1 {{
  font-size: 18px;
  font-weight: 700;
  color: #fff;
  letter-spacing: -0.3px;
}}

.sidebar-header p {{
  font-size: 12px;
  color: var(--light-gray);
  margin-top: 4px;
}}

.sidebar-search {{
  padding: 10px 18px;
  flex-shrink: 0;
}}

.sidebar-search input {{
  width: 100%;
  padding: 8px 12px;
  border: 1px solid rgba(255,255,255,0.15);
  border-radius: 6px;
  background: rgba(255,255,255,0.07);
  color: #fff;
  font-size: 13px;
  outline: none;
}}

.sidebar-search input::placeholder {{ color: var(--light-gray); }}
.sidebar-search input:focus {{ border-color: var(--primary-blue); }}

.video-list {{
  flex: 1;
  overflow-y: auto;
  padding-bottom: 20px;
}}

.event-group-label {{
  padding: 14px 18px 6px;
  font-size: 11px;
  font-weight: 700;
  text-transform: uppercase;
  letter-spacing: 0.8px;
  color: var(--light-blue);
}}

a.video-item {{
  display: block;
  text-decoration: none;
  color: inherit;
}}

.video-item {{
  padding: 10px 18px;
  cursor: pointer;
  border-left: 3px solid transparent;
  transition: background 0.15s, border-color 0.15s;
}}

.permalink {{
  font-size: 13px;
  color: rgba(255,255,255,0.2);
  font-weight: 700;
  flex-shrink: 0;
  transition: color 0.15s;
}}

.video-item:hover .permalink {{
  color: var(--light-blue);
}}

.video-item:hover {{
  background: rgba(255,255,255,0.06);
}}

.video-item.active {{
  background: rgba(0,144,255,0.15);
  border-left-color: var(--primary-blue);
}}

.video-item.important {{
  border-left-color: var(--light-blue);
  background: rgba(68,180,244,0.08);
}}

.video-item.important.active {{
  background: rgba(0,144,255,0.2);
  border-left-color: var(--primary-blue);
}}

.video-item-title {{
  font-size: 13px;
  font-weight: 600;
  color: #fff;
  line-height: 1.35;
  display: -webkit-box;
  -webkit-line-clamp: 2;
  -webkit-box-orient: vertical;
  overflow: hidden;
}}

.video-item-meta {{
  display: flex;
  align-items: center;
  gap: 8px;
  margin-top: 4px;
  font-size: 11px;
  color: var(--light-gray);
}}

.duration-badge {{
  background: rgba(255,255,255,0.12);
  padding: 1px 6px;
  border-radius: 3px;
  font-size: 10px;
  font-weight: 600;
}}

.video-item-desc {{
  font-size: 11px;
  color: rgba(255,255,255,0.5);
  margin-top: 4px;
  display: -webkit-box;
  -webkit-line-clamp: 2;
  -webkit-box-orient: vertical;
  overflow: hidden;
  line-height: 1.4;
}}

/* Main content */
.main {{
  flex: 1;
  overflow-y: auto;
  height: 100vh;
  display: flex;
  flex-direction: column;
}}

.player-container {{
  background: #000;
  flex-shrink: 0;
  position: relative;
}}

.player-container.mini {{
  position: fixed;
  bottom: 16px;
  right: 16px;
  width: 320px;
  z-index: 100;
  border-radius: 8px;
  overflow: hidden;
  box-shadow: 0 4px 20px rgba(0,0,0,0.4);
  border: 1px solid rgba(255,255,255,0.1);
}}

.player-container.mini .player-wrapper {{
  padding-bottom: 56.25%;
}}

.player-spacer {{
  flex-shrink: 0;
  background: #000;
  height: 0;
  overflow: hidden;
}}

.mini-close {{
  display: none;
  position: absolute;
  top: 4px;
  right: 4px;
  z-index: 101;
  background: rgba(0,0,0,0.7);
  color: #fff;
  border: none;
  border-radius: 50%;
  width: 24px;
  height: 24px;
  font-size: 14px;
  line-height: 24px;
  text-align: center;
  cursor: pointer;
  opacity: 0;
  transition: opacity 0.2s;
}}

.player-container.mini .mini-close {{ display: block; }}
.player-container.mini:hover .mini-close {{ opacity: 1; }}
.mini-close:hover {{ background: var(--primary-blue); }}

.player-wrapper {{
  position: relative;
  width: 100%;
  padding-bottom: 56.25%;
}}

.player-wrapper iframe,
.player-wrapper .placeholder,
.player-wrapper #playerDiv {{
  position: absolute;
  top: 0; left: 0;
  width: 100%; height: 100%;
}}

.placeholder {{
  display: flex;
  align-items: center;
  justify-content: center;
  background: var(--dark-navy);
  color: var(--light-gray);
  font-size: 16px;
}}

.meta-header {{
  padding: 20px 28px;
  border-bottom: 1px solid #e0e4e8;
  background: #fff;
  flex-shrink: 0;
}}

.meta-header h2 {{
  font-size: 20px;
  font-weight: 700;
  color: var(--dark-navy);
  line-height: 1.3;
}}

.meta-tags {{
  display: flex;
  flex-wrap: wrap;
  gap: 8px;
  margin-top: 8px;
}}

.meta-tag {{
  font-size: 12px;
  padding: 2px 10px;
  border-radius: 12px;
  background: #e8f4fd;
  color: var(--medium-blue);
  font-weight: 500;
}}

.meta-description {{
  font-size: 13px;
  color: #555;
  margin-top: 10px;
  line-height: 1.5;
}}

/* Transcript */
.transcript {{
  flex: 1;
  padding: 20px 28px 60px;
  background: #fff;
}}

.transcript-line {{
  display: flex;
  gap: 12px;
  padding: 6px 8px;
  border-radius: 4px;
  transition: background 0.2s;
  line-height: 1.55;
}}

.transcript-line:hover {{
  background: #f0f4f8;
}}

.transcript-line.active {{
  background: rgba(0,144,255,0.08);
}}

.transcript-line.active .ts {{
  color: var(--primary-blue);
  font-weight: 700;
}}

.ts {{
  flex-shrink: 0;
  width: 48px;
  font-size: 12px;
  font-family: "SF Mono", "Fira Code", monospace;
  color: var(--primary-blue);
  cursor: pointer;
  padding-top: 2px;
  text-align: right;
}}

.ts:hover {{ text-decoration: underline; }}

mark {{
  background: rgba(0,144,255,0.25);
  color: inherit;
  padding: 1px 2px;
  border-radius: 2px;
}}

.sidebar mark {{
  background: rgba(68,180,244,0.35);
  color: #fff;
}}

.speaker {{
  font-weight: 600;
  color: var(--dark-navy);
  font-size: 14px;
}}

.seg-text {{
  font-size: 14px;
  color: #333;
}}

/* Welcome state */
.welcome {{
  display: flex;
  flex-direction: column;
  align-items: center;
  justify-content: center;
  flex: 1;
  color: var(--light-gray);
  text-align: center;
  padding: 40px;
}}

.welcome h2 {{
  font-size: 22px;
  color: var(--dark-navy);
  margin-bottom: 8px;
}}

.welcome p {{
  font-size: 14px;
  max-width: 400px;
}}

/* Responsive */
@media (max-width: 800px) {{
  :root {{ --sidebar-width: 100vw; }}
  body {{ flex-direction: column; }}
  .sidebar {{ width: 100%; height: auto; max-height: 45vh; min-width: 0; }}
  .main {{ height: auto; overflow-y: auto; }}
}}
</style>
</head>
<body>

<div class="sidebar">
  <div class="sidebar-header">
    <h1>{html.escape(title)}</h1>
    <p>{html.escape(subtitle)}</p>
  </div>
  <div class="sidebar-search">
    <input type="text" id="search" placeholder="Search videos and transcripts..." />
  </div>
  <div class="video-list" id="videoList"></div>
</div>

<div class="main" id="mainArea">
  <div class="player-spacer" id="playerSpacer"></div>
  <div class="player-container" id="playerContainer">
    <button class="mini-close" id="miniClose" title="Close mini player">&times;</button>
    <div class="player-wrapper" id="playerWrapper">
      <div class="placeholder" id="playerPlaceholder">Select a video to begin</div>
      <div id="playerDiv"></div>
    </div>
  </div>
  <div id="metaHeader" class="meta-header" style="display:none"></div>
  <div id="transcript" class="transcript" style="display:none"></div>
  <div class="welcome" id="welcome">
    <h2>Welcome</h2>
    <p>Choose a video from the sidebar to watch with an interactive, searchable transcript.</p>
  </div>
</div>

<script>
var VIDEOS = {video_json};
</script>
<script src="https://www.youtube.com/iframe_api"></script>
<script>
(function() {{
  var player = null;
  var apiReady = false;
  var pendingVideo = null;
  var currentVideoId = null;
  var currentVideoData = null;
  var pollTimer = null;
  var pendingHash = null;

  window.onYouTubeIframeAPIReady = function() {{
    apiReady = true;
    if (pendingVideo !== null) {{ loadVideo(pendingVideo); pendingVideo = null; }}
    else if (pendingHash) {{ loadVideoBySlug(pendingHash); pendingHash = null; }}
    else {{ handleHash(); }}
  }};

  function buildSidebar(filter) {{
    var list = document.getElementById('videoList');
    list.innerHTML = '';
    var currentEvent = null;
    var filterLower = (filter || '').toLowerCase();

    VIDEOS.forEach(function(v, idx) {{
      if (filterLower) {{
        var haystack = (v.title + ' ' + v.description + ' ' + v.event + ' ' +
          v.segments.map(function(s) {{ return s.speaker + ' ' + s.text; }}).join(' ')
        ).toLowerCase();
        if (haystack.indexOf(filterLower) === -1) return;
      }}

      if (v.event !== currentEvent) {{
        currentEvent = v.event;
        var label = document.createElement('div');
        label.className = 'event-group-label';
        label.textContent = currentEvent;
        list.appendChild(label);
      }}

      var link = document.createElement('a');
      link.href = '#' + v.slug;
      link.className = 'video-item' + (v.important ? ' important' : '') +
        (v.id === currentVideoId ? ' active' : '');
      link.setAttribute('data-idx', idx);
      link.setAttribute('data-slug', v.slug);

      var titleRow = document.createElement('div');
      titleRow.style.display = 'flex';
      titleRow.style.alignItems = 'baseline';
      titleRow.style.gap = '6px';

      var title = document.createElement('div');
      title.className = 'video-item-title';
      title.style.flex = '1';
      title.innerHTML = highlight(v.title, filterLower);

      var permalink = document.createElement('span');
      permalink.className = 'permalink';
      permalink.textContent = '#';
      permalink.title = 'Permalink';

      titleRow.appendChild(title);
      titleRow.appendChild(permalink);

      var meta = document.createElement('div');
      meta.className = 'video-item-meta';
      if (v.duration) {{
        var badge = document.createElement('span');
        badge.className = 'duration-badge';
        badge.textContent = v.duration;
        meta.appendChild(badge);
      }}
      if (v.event_date) {{
        var dateSpan = document.createElement('span');
        dateSpan.textContent = v.event_date;
        meta.appendChild(dateSpan);
      }}

      link.appendChild(titleRow);
      link.appendChild(meta);

      if (v.description) {{
        var desc = document.createElement('div');
        desc.className = 'video-item-desc';
        desc.innerHTML = highlight(v.description, filterLower);
        link.appendChild(desc);
      }}

      list.appendChild(link);
    }});
  }}

  function loadVideoBySlug(slug) {{
    for (var i = 0; i < VIDEOS.length; i++) {{
      if (VIDEOS[i].slug === slug) {{ loadVideo(i); return; }}
    }}
  }}

  function handleHash() {{
    var hash = location.hash.replace('#', '');
    if (hash) loadVideoBySlug(hash);
  }}

  window.addEventListener('hashchange', function() {{
    if (!apiReady) {{ pendingHash = location.hash.replace('#', ''); return; }}
    handleHash();
  }});

  function loadVideo(idx) {{
    var v = VIDEOS[idx];
    currentVideoId = v.id;
    currentVideoData = v;

    if (location.hash !== '#' + v.slug) {{
      history.replaceState(null, '', '#' + v.slug);
    }}

    document.querySelectorAll('.video-item').forEach(function(el) {{
      var elSlug = el.getAttribute('data-slug');
      var isActive = elSlug === v.slug;
      el.classList.toggle('active', isActive);
      if (isActive) el.scrollIntoView({{ block: 'nearest' }});
    }});

    document.getElementById('playerPlaceholder').style.display = 'none';
    document.getElementById('welcome').style.display = 'none';
    miniDismissed = false;
    playerContainer.classList.remove('mini');
    playerSpacer.style.height = '0';
    requestAnimationFrame(function() {{
      playerHeight = playerContainer.offsetHeight;
    }});

    if (player) {{
      player.loadVideoById(v.id);
    }} else {{
      player = new YT.Player('playerDiv', {{
        width: '100%',
        height: '100%',
        videoId: v.id,
        playerVars: {{ rel: 0, modestbranding: 1, origin: window.location.origin }},
        events: {{
          onReady: function() {{ startPolling(); }},
          onError: function(e) {{ console.log('YT player error code:', e.data); }}
        }}
      }});
    }}

    var mh = document.getElementById('metaHeader');
    mh.style.display = 'block';
    var tagsHtml = '';
    if (v.event) tagsHtml += '<span class="meta-tag">' + esc(v.event) + '</span>';
    if (v.duration) tagsHtml += '<span class="meta-tag">' + esc(v.duration) + '</span>';
    if (v.upload_date) tagsHtml += '<span class="meta-tag">' + esc(v.upload_date) + '</span>';
    mh.innerHTML = '<h2>' + esc(v.title) + '</h2>' +
      '<div class="meta-tags">' + tagsHtml + '</div>' +
      (v.description ? '<div class="meta-description">' + esc(v.description) + '</div>' : '');

    var tc = document.getElementById('transcript');
    tc.style.display = 'block';
    tc.innerHTML = '';
    v.segments.forEach(function(seg, si) {{
      var line = document.createElement('div');
      line.className = 'transcript-line';
      line.setAttribute('data-time', seg.time_seconds);
      line.setAttribute('data-si', si);

      var ts = document.createElement('div');
      ts.className = 'ts';
      ts.textContent = '[' + seg.time_display + ']';
      ts.addEventListener('click', function(e) {{
        e.stopPropagation();
        if (player && player.seekTo) player.seekTo(seg.time_seconds, true);
      }});

      var content = document.createElement('div');
      content.innerHTML = '<span class="speaker">' + esc(seg.speaker) + ':</span> ' +
        '<span class="seg-text">' + highlight(seg.text, currentFilter) + '</span>';

      line.appendChild(ts);
      line.appendChild(content);
      tc.appendChild(line);
    }});

    if (currentFilter) {{
      var firstMark = tc.querySelector('mark');
      if (firstMark) {{
        firstMark.closest('.transcript-line').scrollIntoView({{ behavior: 'smooth', block: 'center' }});
      }}
    }}

    startPolling();
  }}

  function startPolling() {{
    if (pollTimer) clearInterval(pollTimer);
    pollTimer = setInterval(function() {{
      if (!player || typeof player.getCurrentTime !== 'function') return;
      if (!currentVideoData) return;
      var t = player.getCurrentTime();
      var segs = currentVideoData.segments;
      var activeIdx = -1;
      for (var i = segs.length - 1; i >= 0; i--) {{
        if (t >= segs[i].time_seconds) {{ activeIdx = i; break; }}
      }}
      document.querySelectorAll('.transcript-line').forEach(function(el) {{
        var si = parseInt(el.getAttribute('data-si'));
        el.classList.toggle('active', si === activeIdx);
      }});
    }}, 1000);
  }}

  var currentFilter = '';

  var mainArea = document.getElementById('mainArea');
  var playerContainer = document.getElementById('playerContainer');
  var playerSpacer = document.getElementById('playerSpacer');
  var miniDismissed = false;
  var playerHeight = 0;

  mainArea.addEventListener('scroll', function() {{
    if (!currentVideoId || miniDismissed) return;
    var spacerRect = playerSpacer.getBoundingClientRect();
    var shouldMini = playerHeight > 0 && spacerRect.bottom < 60;
    if (shouldMini && !playerContainer.classList.contains('mini')) {{
      playerSpacer.style.height = playerHeight + 'px';
      playerContainer.classList.add('mini');
    }} else if (!shouldMini && playerContainer.classList.contains('mini')) {{
      playerContainer.classList.remove('mini');
      playerSpacer.style.height = '0';
    }}
  }});

  document.getElementById('miniClose').addEventListener('click', function(e) {{
    e.stopPropagation();
    miniDismissed = true;
    playerContainer.classList.remove('mini');
    playerSpacer.style.height = '0';
  }});

  function esc(s) {{
    var d = document.createElement('div');
    d.textContent = s || '';
    return d.innerHTML;
  }}

  function highlight(text, query) {{
    if (!query) return esc(text);
    var escaped = esc(text);
    var qEsc = query.replace(/[.*+?^${{}}()|[\\]\\\\]/g, '\\\\$&');
    var re = new RegExp('(' + qEsc + ')', 'gi');
    return escaped.replace(re, '<mark>$1</mark>');
  }}

  function updateTranscriptHighlight() {{
    var tc = document.getElementById('transcript');
    if (!currentVideoData || tc.style.display === 'none') return;
    var lines = tc.querySelectorAll('.seg-text');
    currentVideoData.segments.forEach(function(seg, si) {{
      if (lines[si]) lines[si].innerHTML = highlight(seg.text, currentFilter);
    }});
  }}

  document.getElementById('search').addEventListener('input', function() {{
    currentFilter = this.value;
    buildSidebar(this.value);
    updateTranscriptHighlight();
  }});

  buildSidebar('');
}})();
</script>
</body>
</html>
"""


def main():
    args = parse_args()
    tidied_dir = Path(args.tidied_dir)

    if not tidied_dir.is_dir():
        print(f"Error: tidied directory not found: {tidied_dir}")
        return

    videos = load_videos(tidied_dir)
    groups = group_and_sort(videos)
    video_data = build_video_data(groups)

    final_html = build_html(video_data, args.title, args.subtitle,
                            args.accent, args.sidebar)

    output = Path(args.output)
    output.write_text(final_html, encoding="utf-8")
    print(f"Generated {output} with {len(video_data)} videos")
    for event, vids in groups:
        print(f"  {event}: {len(vids)} videos")


if __name__ == "__main__":
    main()
