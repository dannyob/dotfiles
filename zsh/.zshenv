# .zshenv - sourced for ALL shells (interactive, non-interactive, login, non-login)
# This is the right place for PATH and environment variables that must always be set.

# Guix profile (on systems using Guix as package manager)
# This ensures guix-installed binaries are available for non-interactive SSH commands
if [ -f "$HOME/.guix-profile/etc/profile" ]; then
    . "$HOME/.guix-profile/etc/profile"
fi

# Guix current channel (for guix command itself)
if [ -f "$HOME/.config/guix/current/etc/profile" ]; then
    . "$HOME/.config/guix/current/etc/profile"
fi
