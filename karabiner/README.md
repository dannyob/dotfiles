# karabiner — SEED config (copy, do NOT stow)

Karabiner-Elements rewrites `~/.config/karabiner/karabiner.json` on its own
(atomic save: write temp + rename), which **replaces a stow symlink with a real
file**. So this package is a *seed* you copy into place, not a stow package.

## Contents

- `karabiner.json` — the canonical profile. Portable across Macs:
  - complex modifications: `Mail to Hyper`, `Mock Compose Button`,
    `CapsLock to Hyper`, `Danny Apps`
  - a device override (vendor 1241 / product 1031, an **external** keyboard)
    that swaps ⌘↔⌥. Keyed to that keyboard's USB IDs, so it applies on any Mac
    when that keyboard is plugged in — not tied to a specific machine.
- `assets/complex_modifications/capslock.json` — the rule library Karabiner
  reads (never writes); safe to copy or symlink.

## Install on a new Mac

```sh
brew install --cask karabiner-elements
mkdir -p ~/.config/karabiner/assets/complex_modifications
cp ~/Public/dotfiles/karabiner/karabiner.json ~/.config/karabiner/
cp ~/Public/dotfiles/karabiner/assets/complex_modifications/capslock.json \
   ~/.config/karabiner/assets/complex_modifications/
```

Then launch Karabiner-Elements and approve its driver/Input-Monitoring
permissions in System Settings (Privacy & Security).

## Updating the seed

If you change rules on a machine, copy the live file back and commit:

```sh
cp ~/.config/karabiner/karabiner.json ~/Public/dotfiles/karabiner/karabiner.json
```

Note Karabiner appends a `devices` entry the first time it sees a machine's
built-in keyboard; that's harmless, but avoid committing machine-specific device
noise back into the seed unless it's the shared external-keyboard override.
