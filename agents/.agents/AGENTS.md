The user's name is Danny O'Brien <danny@spesh.com>. He knows Python and Unix
shell well, prefers simple Makefiles over most build environments.He is
unafraid of piping, or shell one-liners. He's either using MacOS or Linux. He
has a preference for homebrew on Macos and Debian in the Linux environment.

# Conversations and Tone

Avoid AI cliches in conversation and in written output.

## Remove mannered prose

Mannered prose substitutes metaphor and flourish for direct statement. Instead
of "a parameter worth varying," the mannered writer produces "a dial worth
turning." Instead of "this point still matters," they write "this point earns
its keep." The phrases exist to display the writer, not to convey the idea, and
readers can tell. That is why mannered prose irritates: it makes the reader
work harder so the writer can perform. It is also imprecise. Metaphors drag in
connotations the writer did not choose and cannot control. The fix is to say
what you mean. When a literal phrase is available, use it.

If you see mannered prose in previously generated text (skills, summaries,
README.mds), ask if you can improve on it.

# Generally, choose Python

Python is the default language for Danny; the other possibilities are rust, for
fast or system programming, or Guile, when working on Guix, Spritely or GNU
projects. If you're unsure, check before proceeding.

# Simple Scripts

When creating standalone executables in Python. Use "#!/usr/bin/env -S uv
run --script" and inline PEP 723 dependencies.

# Larger Codebases

## Python

When creating Python packages (or more complex Python programs), use `uv' in a
venv environment, with the uv build system. There should be a Python template
in @~/Public/src/dob/py-template/template/ -- adapt it as necessary.

## Guile

Use Guix coding standards and libraries for Guile. Use the following prefix for standalone scripts.

```sh
#!/usr/bin/guile \
-e main -s
!#
```

## Test-Driven Development for most things (see the Superpower skill on this)

The preferred development approach for anything but the simplest scripts is red/green
test-driven development: write a failing test, then write the code, then run
the test to ensure it passes.

Before claiming a feature/fix works, however, rebuild the actual binary or
artifact and run an end-to-end smoke test against it — NEVER rely on passing
unit tests alone.

# Committing Code

Offer to commit code at regular intervals. Ideally, a commit should be self
contained and change only one thing. The first line of the commit message
should very briefly state what the change is, and the body of the commit
message (if any) should say why it was changed.

When the destination of a commit seems to be a public repo, omit personal
information or local configuration details (ie. references to ~/Private/data ,
examples that use local inaccessible documents, etc).

git commits: keep commits focused on a single concern. Do not let unrelated
changes sneak into a commit. When asked to commit specific changes, exclude
everything else. If you delete the git worktree directory when you are in that
directory, bash will throw errors -- tell the user to quit and restart claude
in another directory.

The preferred license is MIT.

# Contacting other people / public-facing communications

Do not report issues, file pull requests, or send messages to individuals who
may be irritated by direct AI communications without clear, explicit permission
from Danny. This includes filing GitHub issues/PRs on other people's repos,
emailing or messaging maintainers/strangers, and posting to public trackers or
forums. If in doubt, ask. 

Preparing the artifact (draft issue, repro, patch) is fine and encouraged;
*sending* it needs a clear, explicit yes for that specific act -- an offhand
"do it" does not clear the bar.

A good place to put proposed upstream issues or PRs is in the life system's
inbox. See the life system skill for more on this. 

If something does go out (with permission), keep it short and human: lead in a
human voice, give the core evidence and a clean repro, and cut the exhaustive
AI-catalogue bits (long negative-results lists, variant matrices). Don't file
marginal secondary issues alongside the main one.

# Bash and Shell

Danny uses zsh, which is mostly bash-compatible.

In Danny's setup Unix commands 'rm', 'mv', `ls` and 'cp' are aliased to
interactive (-i or colorized) versions so you should escape them (e.g. '\rm')
if you use them non-interactively.

For complex shell constructs (loops with pipes, command substitution, `while
read`, etc.), wrap them in a heredoc passed to bash to avoid zsh parsing
issues:

```bash
bash << 'EOF'
for item in $(some_command | head -5); do
  echo "$item"
done
EOF
```

# Google Drive / Documents / Sheets / Slides / Workspace Access -- Reading and Writing.

Use the gog skill. 

# Secrets and pass

Use the pass-secrets skill. Never print a pass entry to the terminal, and do
not try to make it safe by filtering the output through a denylist of words
like "password" or "secret" -- entries are freeform notes and the interesting
line is as likely to read "root:" or "Root pass:". Pipe the value straight into
the command that consumes it, or extract one named field. `pass ls` (names
only) is always safe. If a credential does get printed, say so plainly and tell
Danny which ones need rotating.

# Remote commands using ssh

Use tmux (and the tmux superpower skill) to use long-lasted sessions (and
gather their results) rather than firing off a sequence of standalone ssh
commands.

# Private setup

More private configuration data is available at: ~/.agents/AGENTS-PRIVATE.md
@~/.agents/AGENTS-PRIVATE.md

When updating these instructions, consider what is private and personal -- if
it is, add it instead to the above file.

All of the above can be overruled by Danny.
