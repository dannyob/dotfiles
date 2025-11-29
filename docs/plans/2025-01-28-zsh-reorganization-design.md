# Zsh Configuration Reorganization

## Goal

Reorganize zsh dotfiles to eliminate duplication, separate platform-specific from machine-specific config, and modernize plugin management.

## File Structure

```
zsh/
├── .zshrc              # Core + platform detection + zsh_unplugged
├── .zprofile           # Login shell setup
├── .zshrc_macos        # macOS-specific (new)
├── .zshrc_linux        # Linux-specific with Debian fine-tuning (new)
├── .zshrc-pequod       # Machine-specific (slimmed)
├── .zshrc-megayacht    # Machine-specific (slimmed)
└── .local/bin/fasd     # Unchanged
```

Naming convention:
- `_` separator = platform files (auto-detected)
- `-` separator = hostname files (machine-specific)

## Loading Order

1. Core settings (SHORTHOST, DEBEMAIL, shell options, core aliases/functions)
2. GPG/SSH agent setup
3. PYTHONPYCACHEPREFIX (with darwin/linux detection)
4. Platform detection → source `.zshrc_macos` OR `.zshrc_linux`
5. Guix-as-package-manager snippet (if `guix` command exists on any platform)
6. Plugins via zsh_unplugged
7. Machine-specific → source `.zshrc-$SHORTHOST` if exists

## Platform Detection Logic

```zsh
if [[ "$OSTYPE" == darwin* ]]; then
    [[ -f ~/.zshrc_macos ]] && source ~/.zshrc_macos
else
    [[ -f ~/.zshrc_linux ]] && source ~/.zshrc_linux
fi
```

## What Goes Where

### .zshrc (core, shared)

- SHORTHOST detection
- DEBEMAIL/DEBFULLNAME/EMAIL
- Shell options (auto_cd, auto_pushd, share_history, hist_ignore_dups, etc.)
- Core aliases (rm -i, mv -i, cp -i, more=less, vi=$EDITOR, ps, cls)
- Core functions:
  - vimbin, tempe, », tidyhtml, html2markdown
  - fixgpg, dob-mkindex, dob-tz, dob-cid, dob-spelling
- GPG/SSH agent setup
- PYTHONPYCACHEPREFIX (already has darwin/linux detection)
- KEYTIMEOUT
- zsh_unplugged function + plugin loading
- Guix-as-package-manager snippet:
  - guixinstall, guixupgrade functions
  - GUIX_LOCPATH
  - fpath for guix completions
- Platform file sourcing
- Hostname file sourcing
- TRAMP fix
- Wasmer setup
- SUMVIDEO_DIR
- PATH additions

### .zshrc_macos

- `eval "$(/opt/homebrew/bin/brew shellenv)"`
- FPATH for brew completions
- `alias ls='gls -A -F --color=auto --hyperlink'`
- `alias clipin='pbcopy'` / `alias clipout='pbpaste'`
- Guile paths (homebrew locations)
- NVM setup
- Mac-specific functions:
  - dob-st (browser history)
  - dob-toplink (get URL from Brave)
  - dob-update-llm-plugins
  - dob-chrome-rdp
  - activ (venv activation)
- Tailscale alias

### .zshrc_linux

- `alias ls='ls -A -F --group-directories-first --sort=extension --color=auto --hyperlink'`
- `alias clipin='wl-copy'` / `alias clipout='wl-paste'`
- Wayland functions:
  - screencapture
  - screenrecord
  - wl-copymime
- Debian fine-tuning (if `/etc/debian_version` exists):
  - aptinstall function

### .zshrc-pequod (machine-specific)

- Rye env sourcing
- History file/size settings
- TeX path
- Qlot path
- Homebrew Ruby path
- Protobuf path
- Guix VM aliases (guix, guix-env, guix-app, guix-shell)

### .zshrc-megayacht (machine-specific)

- History file/size settings
- Any megayacht-unique config

## Plugin Management

Replace zgen with zsh_unplugged (~30 lines, no external dependencies).

Plugins cloned to `~/.config/zsh/plugins/`:
- zsh-users/zsh-syntax-highlighting
- zsh-users/zsh-history-substring-search
- zsh-users/zsh-completions

Fasd loaded directly via `eval "$(fasd --init auto)"`.

```zsh
##
# PLUGINS (zsh_unplugged)
##
ZPLUGINDIR=${ZPLUGINDIR:-${HOME}/.config/zsh/plugins}

function plugin-load {
  local repo plugdir initfile initfiles=()
  for repo in $@; do
    plugdir=$ZPLUGINDIR/${repo:t}
    initfile=$plugdir/${repo:t}.plugin.zsh
    if [[ ! -d $plugdir ]]; then
      echo "Cloning $repo..."
      git clone -q --depth 1 https://github.com/$repo $plugdir
    fi
    if [[ ! -e $initfile ]]; then
      initfiles=($plugdir/*.{plugin.zsh,zsh-theme,zsh,sh}(N))
      (( $#initfiles )) || continue
      ln -sf $initfiles[1] $initfile
    fi
    fpath+=$plugdir
    . $initfile
  done
}

repos=(
  zsh-users/zsh-syntax-highlighting
  zsh-users/zsh-history-substring-search
  zsh-users/zsh-completions
)
plugin-load $repos

# fasd
[[ -x ~/.local/bin/fasd ]] && eval "$(~/.local/bin/fasd --init auto)"

# history-substring-search keybindings
bindkey -M vicmd 'k' history-substring-search-up
bindkey -M vicmd 'j' history-substring-search-down
bindkey '^[[A' history-substring-search-up
bindkey '^[[B' history-substring-search-down
```

## Files to Delete

- `.zgen/` (entire directory tree)
- `.zshrc-workboat`

## Files to Create

- `.zshrc_macos`
- `.zshrc_linux`

## Files to Modify

- `.zshrc` (restructure)
- `.zprofile` (remove GUIX_LOCPATH, moves to guix snippet)
- `.zshrc-pequod` (slim down, remove duplicated plugin/fasd setup)
- `.zshrc-megayacht` (slim down, remove duplicated plugin/fasd setup)

## Verification

After implementation, test on:
1. macOS machine (pequod) - verify brew, gls, pbcopy work
2. Guix System machine (megayacht) - verify guix functions, wl-copy work
3. New shell launch - verify plugins clone correctly to ~/.config/zsh/plugins/
