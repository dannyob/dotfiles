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
             (gnu home services shells)
             (gnu home services)
             (dtao-guile home-service)
             (dwl-guile home-service)
             (dwl-guile patches))

(define dob-dtao-guile-time-block
  (list (dtao-block
          (render `(strftime "%A, %d %b %T" (localtime (current-time))))
          (interval  1))))

(define dob-dtao-guile-left-block
  (append
   (map
    (lambda (tag)
      (let ((str (string-append "^p(8)" (number->string tag) "^p(8)"))
            (index (- tag 1)))
        (dtao-block
         (interval 0)
         (events? #t)
         (click `(match button
                   (0 (dtao:view ,index))))
         (render `(cond
                   ((dtao:selected-tag? ,index)
                    ,(format #f "[~a]" str))
                   ((dtao:urgent-tag? ,index)
                    ,(format #f "*~a*"
                             str))
                   ((dtao:active-tag? ,index)
                    ,(format #f "#~a#"
                             str))
                   (else ,str))))))
    (iota 9 1))
   (list
    (dtao-block
     (events? #t)
     (click `(dtao:next-layout))
     (render `(string-append "^p(4)" (dtao:get-layout)))))))

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

                    ;; Kitty configuration!
                    (simple-service 'kitty-config
                                    home-xdg-configuration-files-service-type
                                    `(("kitty/kitty.conf" ,(local-file "kitty/.config/kitty/kitty.conf"))))

                    ;; Create and add the dtao-guile home service to your home configuration.
                    (service home-dtao-guile-service-type
                             (home-dtao-guile-configuration
                               ;; Optionally use a custom dtao-guile package.
                               (auto-start? #t)
                               ;; Create a custom configuration for dtao.
                               (config
                                 (dtao-config
                                   ;; A font string in fcft format.
                                   (font "modeseven:size=16")
                                   ;; Read `root', `border' and `text' colors from dwl-guile.
                                   ; (use-dwl-guile-colorscheme? #t)
                                   (background-color "11111AA")
                                   (border-color "333333FF")
                                   (foreground-color "FFFFFFFF")
                                   (padding-left 8)
                                   (padding-right 8)
                                   (padding-top 2)
                                   (padding-bottom 2)
                                   ;; Request an exclusive zone for the bar to prevent overlapping.
                                   (exclusive? #t)
                                   ;; Layer to render the bar in (LAYER-TOP, LAYER-BOTTOM, LAYER-OVERLAY, LAYER-BACKGROUND).
                                   (layer 'LAYER-BOTTOM)
                                   ;; Render the bar at the bottom of the screen.
                                   (bottom? #f)
                                   ;; Height of the bar in pixels. Set to #f for automatic height based on font size.
                                   (height #f)
                                   ;; Delimiter string of arbitrary length inserted between blocks.
                                   ; (delimiter #f)
                                   ;; Additional spacing on each side of the delimiter string.
                                   (block-spacing 0)
                                   (left-blocks dob-dtao-guile-left-block)
                                   (center-blocks '())
                                   (right-blocks dob-dtao-guile-time-block)
                                   ;; List of Guile module dependencies needed to run your blocks.
                                   (modules '())))))

         (service home-dwl-guile-service-type
                  ;; If you wish to configure the home service further, you can pass in
                  ;; a configuration to the service. All options listed below are optional.
                  (home-dwl-guile-configuration
                    ;; Use a custom dwl-guile package.
                    ;; (package my-custom-dwl)
                    ;; If you want to dynamically apply patches, you can create a new
                    ;; modified package definition (multiple patches can be applied).
                    ;; Note that some patches might have conflicts.
                    ;;
                    (package
                      (patch-dwl-guile-package dwl-guile
                                               #:patches (list %patch-xwayland)))

                    ;; Environment variables to set for Wayland compatibility with applications.
                    ;; By default, native Wayland rendering will be enabled for most applications.
                    ;; Native rendering of QT-applications is enabled using the @code{native-qt?}
                    ;; option. This is because it requires the qtwayland package.
                    ;;
                    ;; Set it to an empty list to skip setting environment variables:
                    ;; (environment-variables '())
                    ;;
                    ;; Or extend the default environment variables:
                    ;; (environment-variables
                    ;;  (append `(("var" . "value")) %dwl-guile-base-env-variables))

                    ;; A string containing a command to execute after starting dwl-guile.
                    ;; This is the equivalent of specifying a script to the '-s' flag of dwl.
                    ;; The gexp's will be executed in the same order as in the list.
                    ;;
                    ;; The preferred way of running commands/applications (that does not need
                    ;; to access the stdout of dwl) on startup is by using the
                    ;; dwl:startup-hook in your Guile config.
                    ;;
                    ;; By default, this option is not used.
                    (startup-command "kitty")

                    ;; If QT-applications should be rendered natively. Enabled by default.
                    ;; This will set QT_QPA_PLATFORM="wayland-egl" and install
                    ;; the "qtwayland" package to enable support for Wayland.
                    (native-qt? #t)

                    ;; If dwl-guile should auto-start on first login. Enabled by default.
                    (auto-start? #t)

                    ;; If the dwl-guile config should be automatically reloaded on change.
                    ;; This will allow you to see (most of) the effects of your config changes
                    ;; dynamically, without restarting dwl-guile.
                    (reload-config-on-change? #t)

                    ;; Create a custom configuration for dwl.
                    (config '((set-xkb-rules '((options . "compose:prsc,caps:hyper")))))))

                    ;; Catch locally installed fonts, also git dotfiles fonts
                    (simple-service 'additional-fonts-service
                                    home-fontconfig-service-type
                                    (list "~/.fonts/" "~/.local/share/fonts"))))) ;; FIXME make generate and refer to a gexp for fonts/.fonts
