---
name: fresh-reader-review
description: Use when polishing human-facing prose before publishing -- blog posts, announcements, docs, READMEs, important emails, landing copy -- especially to check it doesn't read as AI-written, doesn't lose a fresh reader to jargon, doesn't rebut questions no reader is asking, and matches a target venue's voice.
---

# Fresh-Reader Review

## Overview

You cannot see your own draft the way a first-time reader will. You know what you
meant, you remember every edit, and you have stopped noticing the jargon. The fix
is to dispatch an **independent** subagent -- one with NONE of your authoring
context -- to read the piece cold as a specific member of the target audience and
report back concrete edits.

Core principle: **independence plus a real persona surface problems the author is
blind to.** A fork or a self-review inherits your assumptions; a fresh agent does
not.

A capable agent already spots surface clichés on its own (more so when a tropes
list is in context). The value of this skill is the harder layer a generic
"review this" misses: reading as a real reader *with stakes*, catching rebuttals
to questions nobody asked, matching a venue's actual voice, and returning
paste-able edits instead of vibes.

## When to use

- Any prose a human will read for real: blog posts, launch announcements, docs,
  READMEs, important emails, landing copy.
- Especially after heavy editing -- that is when "unasked-question fossils"
  accumulate (see lenses).
- When the piece must sit beside existing published work and match its voice.

Skip for throwaway internal notes, your own scratch text, or code comments.

## The technique

Dispatch a **fresh general-purpose agent** -- NOT a fork. A fork inherits your
context and therefore your blind spots; independence is the entire point. Give it
four things: the draft, a persona, the lenses, and an output contract.

### 1. A representative reader with stakes

Do not say "review this." Say WHO is reading and WHY it matters to them:

> "Read this as a [specific audience member] who is deciding whether to [real
> action with consequences]. You are NOT an insider."

Stakes make confusion legible. A reader "deciding whether to apply for a grant"
notices exactly where the call-to-action goes vague; a generic reviewer does not.

### 2. The lenses (name them explicitly)

- **AI-tells.** Point it at the two references bundled with this skill (pass their
  full paths to the reviewer): `TROPES.md` -- common AI writing tells, from
  [tropes.fyi](https://tropes.fyi) -- and `elements-of-style.md` -- Strunk's rules,
  public domain. Require flagged lines, not vibes.
- **Unasked-question fossils.** Passages that rebut a doubt no fresh reader holds
  -- usually artifacts of the editing path. "These are real teams, not proposals"
  only parses if you know an earlier draft *was* a proposal; a fresh reader never
  had the doubt, so the rebuttal reads as defensive noise. This is the
  highest-value check and generic reviews miss it.
- **Comprehension blockers.** Which terms/acronyms actually stop the persona cold,
  versus ones they tolerate.
- **Tonal match** (only when publishing beside existing work). Give it 1-2 real
  published pieces from the venue and have it compare register, vocabulary,
  sentence rhythm, and calibrate "AI-ish" against THAT house style -- not an
  abstract ideal. If the venue uses em-dashes and some flourish, those are not
  tells there.

### 3. The output contract

- **Honest but not brutal.** Calibrate to a draft that has already had work; do
  not invent problems to seem rigorous. If something works, say so and move on.
- **Every criticism ships with a concrete, paste-able rewrite.** "This is vague"
  is useless; "change X to Y" is the deliverable.
- Ask for: a one-line verdict, blocking issues, line-level nits, and gaps.

## Second pass

After you revise, **resume the same reviewer** (do not start a fresh one) so it
can verify its earlier points landed, catch anything the edits broke, and add a
new check -- typically tonal match against the reference pieces. Loop until the
verdict is clean.

## Prompt template

```
You are an independent editorial reviewer. Read the draft below COLD.

PERSONA: Read as <specific audience member> who is deciding whether to
<real action with stakes>. You are NOT an insider; jargon an insider glosses
over may stop you.

APPLY THESE LENSES:
1. AI-tells -- read the bundled references listed below (TROPES.md and
   elements-of-style.md) and flag specific lines that violate them.
2. Unasked-question fossils -- passages that rebut a doubt a fresh reader would
   never have (often editing artifacts). Quote and cut/rewrite them.
3. Comprehension blockers -- terms/acronyms that actually stop you; note which
   block vs. which you tolerate.
4. Tonal match -- [attach 1-2 published pieces from the target venue] -- does the
   draft belong beside these? Compare register, vocabulary, rhythm. Calibrate
   "AI-ish" against THIS house style, not an ideal.

TONE: Honest but not brutal; this draft has had work. Note what already works.

OUTPUT: (1) one-line verdict, (2) blocking issues, (3) line-level nits,
(4) gaps. Every criticism gets a concrete, paste-able rewrite.

REFERENCES (read first): <skill-dir>/TROPES.md and <skill-dir>/elements-of-style.md

DRAFT:
<paste, or give the file path>
```

## Common mistakes

| Mistake | Fix |
|---|---|
| Using a fork (or reviewing it yourself) | Use a fresh agent -- a fork inherits your blind spots |
| "Review this" with no persona | Name the reader and their stake; otherwise you get generic copyediting |
| Vague criticism | Require paste-able edits for every point |
| Calibrating tone against an abstract ideal | Give it the actual venue's published pieces; in-house style is not a tell |
| Pasting the reviewer's fixes verbatim | It can suggest its own clichés (e.g. an invented label) -- apply judgment |

## Bundled references

This skill ships two reference files; hand their full paths to the reviewer.

- `TROPES.md` -- catalogue of AI writing tells. Source: [tropes.fyi](https://tropes.fyi) by ossama.is.
- `elements-of-style.md` -- William Strunk Jr.'s *The Elements of Style* (1918), public domain (Project Gutenberg #37134).
