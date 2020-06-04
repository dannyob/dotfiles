(use-modules (gnu packages))

(define profile-directory (string-append (getenv "HOME") "/.config/guix-profiles/"))

;; base
(define base-manifest
  (specifications->manifest
   '(
      "aspell"
      "aspell-dict-en"
      "binutils"
      "coreutils"
      "cryptsetup"
      "curl"
      "emacs"
      "emacs-guix"
      "fasd"
      "file"
      "git"
      "glibc-utf8-locales"
      "gnupg"
      "guile"
      "guile-colorized"
      "guile-json"
      "guile-readline"
      "guile-sqlite3"
      "imagemagick"
      "jq"
      "lsof"
      "magic-wormhole"
      "mailutils"
      "msmtp"
      "nmap"
      "neovim"
      "nss"
      "nss-certs"
      "openssh"
      "owncloud-client"
      "patchelf"
      "pinentry"
      "python"
      "rsync"
      "sed"
      "sqlite"
      "stow"
      "strace"
      "the-silver-searcher"
      "tidy"
      "tor"
      "unzip"
      "wget"
      "xdg-user-dirs"
      "xdg-utils"
      "zip"
      "zsh")))

;; laptop tools
(define laptop-manifest
   (specifications->manifest
      '(
         "bluez"
         "powertop")))

;; stuff I need for a decent desktop
(define desktop-manifest
   (specifications->manifest
      '(
         "pulseaudio"
         "pavucontrol"
         "kitty"
         "hexchat"
         "font-google-noto"
         "font-dejavu"
         "font-iosevka-term"
         "fontconfig"
         "feh"
         "evince"
         "xev"
         "mako"
         "docker"
         "docker-cli"
         "clementine"
         "libreoffice"
         "youtube-dl"
         "xhost"
         "dmenu"
         "sway"
         "wl-clipboard")))

(define manifest-assoc `(("desktop" . ,desktop-manifest)
                         ("laptop" . ,laptop-manifest)
                         ("base" . ,base-manifest)))

;; INIT -- create profiles
