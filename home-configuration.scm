;; This "home-environment" file can be passed to 'guix home reconfigure'
;; to reproduce the content of your profile.  This is "symbolic": it only
;; specifies package names.  To reproduce the exact same profile, you also
;; need to capture the channels being used, as returned by "guix describe".
;; See the "Replicating Guix" section in the manual.

(use-modules (gnu home)
             (gnu packages)
             (gnu services)
             (guix gexp)
             (gnu home services fontutils)
             (gnu home services shells))

(home-environment
  ;; Below is the list of packages that will show up in your
  ;; Home profile, under ~/.guix-home/profile.
  (packages (specifications->packages (list "zsh"
                                            "nyxt"
                                            "mu"
                                            "bcc"
                                            "grimshot"
                                            "qtwayland"
                                            "flatpak"
                                            "obs"
                                            "neovim"
                                            "msmtp"
                                            "obs-wlrobs"
                                            "mblaze"
                                            "sbcl"
                                            "guile-goblins"
                                            "gdb"
                                            "guile-next"
                                            "google-chrome-unstable"
                                            "emacs-pgtk"
                                            "thunar"
                                            "mplayer"
                                            "xdg-desktop-portal-wlr"
                                            "hexchat"
                                            "magic-wormhole"
                                            "sway"
                                            "mako"
                                            "w3m"
                                            "slurp"
                                            "bemenu"
                                            "wgetpaste"
                                            "mutt"
                                            "fd"
                                            "grim"
                                            "shepherd"
                                            "guile-gnutls"
                                            "pandoc"
                                            "gnutls"
                                            "isync"
                                            "emacs-geiser-guile"
                                            "emacs-vterm"
                                            "sqlite"
                                            "libvterm"
                                            "haunt"
                                            "shellcheck"
                                            "guile"
                                            "readline"
                                            "htop"
                                            "rlwrap"
                                            "stow"
                                            "guile-readline"
                                            "emacs-compat"
                                            "emacs-debbugs"
                                            "guile-hoot"
                                            "syncthing"
                                            "pup"
                                            "fasd"
                                            "emacs-geiser"
                                            "font-openmoji"
                                            "font-iosevka-term"
                                            "font-iosevka-aile"
                                            "font-iosevka"
                                            "le-certs"
                                            "nss-certs"
                                            "glibc-locales"
                                            "font-google-noto")))

  ;; Below is the list of Home services.  To search for available
  ;; services, run 'guix home search KEYWORD' in a terminal.
  (services
   (list (service home-bash-service-type
                  (home-bash-configuration
                   (aliases '(("ls" . "ls --color=auto")))
                   (bashrc (list (local-file "bash/.bashrc" "bashrc")))
                   (bash-logout (list (local-file "bash/.bash_logout" "bash_logout")))))

         ;; Catch locally installed fonts, also git dotfiles fonts
         (simple-service 'additional-fonts-service
                home-fontconfig-service-type
                (list "~/.fonts/" "~/.local/share/fonts"))))) ;; FIXME make generate and refer to a gexp for fonts/.fonts
