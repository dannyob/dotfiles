---
name: writing-readmes
description: Use when writing or editing a README, or reviewing a README draft - especially when the draft feels generic, marketing-shaped, balanced into symmetric sections, or front-loads features and abstractions before showing the tool actually working. Also use when starting a new project README from scratch.
---

# Writing READMEs

## Overview

A README is a front desk, not a whitepaper. The strongest READMEs are concrete, lightly opinionated, and optimized for time-to-first-understanding. AI defaults tend the other way: abstract, exhaustive, marketing-shaped, suspiciously balanced.

Readers silently ask four questions in the first thirty seconds:

1. What is this?
2. Why should I care?
3. Can I make it work quickly?
4. Does this feel maintained by competent humans?

Every section earns its place by answering one of these.

## When to Use

- Drafting a new README
- Rewriting a README that reads as "documentation-shaped" rather than lived-in
- Reviewing a README before merging
- Diagnosing a README that has feature lists before examples, balanced sections, marketing adjectives, badge carpets, or no admitted limitations

**When NOT to apply:** mature infrastructure projects (curl, sqlite, gcc) where completeness and historical density are themselves trust signals. Those follow different rules earned over years.

## Recommended Structure

In this order. Asymmetric is correct - the first three items deserve real space, the rest can be terse.

1. **Title** + one brutally concrete sentence
2. **Demo** - real command output, screenshot, GIF, or copy-paste runnable example
3. **Install** - one or two commands
4. **Quickstart** - the minimum path to a visible result
5. **Why this exists** (only if non-obvious; skip otherwise)
6. **Core concepts** / **Configuration** - only what users actually need
7. **Development** / **Contributing** (optional, terse)
8. **License**

Avoid the AI-default order: Vision -> Features -> Architecture -> Roadmap -> Philosophy -> Installation. Front-loading abstractions before any working example is the strongest "AI README" tell.

A `## Features` section before any demo is a red flag - delete it and let the demo and quickstart show what the tool does. (If you keep a feature list, every bullet should be a concrete claim with a number, comparison, or link, like uv's Highlights - not loose adjectives.)

## Core Principles

**Show before explaining.** Real output beats any description. A paste-and-run snippet beats a feature list. Examples are documentation compression.

**Specificity creates trust.** "Fast," "small," "weird," "incomplete," "Unixy," "single-file" beat "robust," "powerful," "comprehensive," "modern," "seamless."

**Asymmetric is correct.** Real maintainers emphasize what matters. Equal-length sections and exhaustive symmetry signal a template.

**Earned friendliness beats declared friendliness.** Clear setup and answered objections feel more welcoming than "We welcome contributions from everyone!"

**Slightly incomplete is correct.** Link out for details. The README is not a tutorial series, a whitepaper, or generated reference docs.

**Admit something honestly.** "Windows support is incomplete." "Not suitable for production yet." "This is optimized for small teams." "Slower than fd; smaller scope." AI-shaped READMEs almost never volunteer a weakness; doing so is a strong trust signal.

**A little voice goes a long way.** htmx ends its README with a haiku. uv credibly says "extremely fast" because the very next line is a benchmark chart. A small unmistakably-human moment - a dry aside, a labelled annoyance, a strong opinion - does more than any amount of polished prose.

## Anti-Patterns Specific to READMEs

For prose-level AI tells (em-dashes, "delve," negative parallelism, bold-first bullets), see the bundled `assets/TROPES.md` (latest at <https://tropes.fyi/>). The list below is the README-specific layer on top of those.

- **Adjective-stack opening** - "A powerful, modern, comprehensive solution for..." → say what it does instead
- **Features section before any demo** - delete or move below Quickstart
- **Bold-first bullets** - `**Performance**: ...` `**Security**: ...` (a strong AI README tell, especially with emojis)
- **Badge carpet** - if badges visually dominate the first screen, the README is compensating. Build status, version, docs, license, maybe coverage. Stop.
- **Emoji section headers** - almost never in human-written READMEs
- **Roadmap theater** - lists of unbuilt features framed as commitments
- **README-as-landing-page** - vision statements, ecosystem framing, "next-generation"
- **Declared welcomingness** instead of actual onboarding (clear setup, labelled good-first-issues, answered FAQ)
- **Aspirational prose** - what users will *feel* rather than what the tool *does*
- **Symmetric subsections** - four parallel mid-level headings of similar length is template-shaped, not lived-in
- **Tutorial voice** - "Let's walk through..." "Now we'll explore..." in the README itself
- **Vague comparisons** - "Similar in spirit," "More powerful but harder to remember." Replace with one concrete tradeoff or delete.

## Vocabulary

Words that make a README feel AI-generated:

> robust, seamless, cutting-edge, comprehensive, scalable, enterprise-grade, modern, powerful, next-generation, blazing-fast (any "blazing"), elegant, intuitive, beautiful, delightful

Words human maintainers actually reach for:

> fast, small, weird, simple, incomplete, experimental, Unixy, annoying, opinionated, single-file, single-binary, ugly-but-works

Specificity over elevation. "Single-file Python script, no deps" beats "lightweight elegant solution."

## Before / After

**AI-default opening:**

```markdown
# lf

A powerful, modern command-line file lister built for today's developers.
Lf provides a comprehensive solution for navigating filesystems with an
elegant, intuitive interface that seamlessly integrates into your workflow.

## Features
- Pattern matching
- Recursive search
- JSON output
- Sorting
- Filtering
...
```

**Human-shaped opening:**

````markdown
# lf

`lf` lists files matching a glob, sorted however you want, in JSON if you ask for it.

```sh
$ lf '**/*.py' --sort mtime --reverse | head -3
src/lf/cli.py        2026-05-08 14:22  4.1K
src/lf/glob.py       2026-05-07 09:11  2.8K
tests/test_glob.py   2026-05-06 17:44  6.0K
```

Single-file Python script, no runtime deps. macOS and Linux. Windows untested.
````

The second version answers all four reader questions in twelve lines.

## Quick Checklist

Run these checks before publishing or merging a README:

- [ ] First sentence says what the tool does, no adjective stack
- [ ] First screenful contains real output or a runnable example
- [ ] Install is one or two commands
- [ ] At least one limitation, constraint, or scope boundary is admitted
- [ ] No `## Features` section above the demo or quickstart
- [ ] No bold-first bullets, no emoji section headers
- [ ] At most ~4 badges, all meaningful
- [ ] No "powerful / modern / comprehensive / robust / seamless / cutting-edge / elegant / blazing"
- [ ] Sections aren't suspiciously balanced in length
- [ ] Tone reads like someone who uses the tool, not someone selling it
- [ ] Comparisons to other tools are concrete (one specific tradeoff each) or absent

## Self-Edit Pass

After drafting, do four cuts:

1. Cut every adjective that doesn't carry information
2. Replace every "powerful / robust / modern / comprehensive / seamless" with a specific claim or delete the sentence
3. For each section, ask: is there an example here? If not, can the section be deleted or compressed?
4. Cut any sentence that could appear unchanged in any other project's README

If a paragraph survives all four cuts, it's probably worth keeping.

## Reference Examples

Two READMEs are bundled in `examples/` next to this skill, each with a header annotation pointing out the specific moves worth copying. **Read them when drafting** - mimicking concrete patterns is more useful than abstract rules.

- **examples/uv-README.md** - one-sentence positioning, demo before features, concrete numeric claims ("10-100x faster than pip") instead of adjectives, named positioning against alternatives.
- **examples/htmx-README.md** - voice over polish, motivation as specific questions, paste-and-run quickstart, honest aside about a wrongly-named package, dry "they are all OK" tone, ends with a haiku.

For the prose-level AI tells underneath all of this (em-dashes, "delve," negative parallelism, bold-first bullets), see the bundled `assets/TROPES.md` (latest at <https://tropes.fyi/>).
