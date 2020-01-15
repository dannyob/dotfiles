;; -*- mode: emacs-lisp; lexical-binding: t -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Layer configuration:
This function should only modify configuration layer settings."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs

   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'unused

   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t

   ;; If non-nil layers with lazy install support are lazy installed.
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()

   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(clojure
     python
     javascript
     yaml
     haskell
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press `SPC f e R' (Vim style) or
     ;; `M-m f e R' (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     helm
     ;; ivy
     deft
     html
     auto-completion
     better-defaults
     emacs-lisp
     latex
     bibtex
     git
     markdown
     scheme
     multiple-cursors
     parinfer
     fasd
     notmuch
     pandoc
     (shell :variables
            shell-default-position 'right)
     org
     purescript
     racket
     spacemacs-org
     spell-checking
     syntax-checking
     version-control
     theming
     dobextras
     dobprivate)


   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   ;; To use a local version of a package, use the `:location' property:
   ;; '(your-package :location "~/path/to/your-package/")
   ;; Also include the dependencies as they will not be resolved automatically.
   dotspacemacs-additional-packages '(
                                      (org-pretty-table :location (recipe :fetcher github :repo "Fuco1/org-pretty-table"))
                                      guix
                                      beeminder
                                      persistent-scratch)

   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()

   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '()

   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and deletes any unused
   ;; packages as well as their unused dependencies. `used-but-keep-unused'
   ;; installs only the used packages but won't delete unused ones. `all'
   ;; installs *all* packages supported by Spacemacs and never uninstalls them.
   ;; (default is `used-only')
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization:
This function is called at the very beginning of Spacemacs startup,
before layer configuration.
It should only modify the values of Spacemacs settings."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non-nil then enable support for the portable dumper. You'll need
   ;; to compile Emacs 27 from source following the instructions in file
   ;; EXPERIMENTAL.org at to root of the git repository.
   ;; (default nil)
   dotspacemacs-enable-emacs-pdumper nil

   ;; File path pointing to emacs 27.1 executable compiled with support
   ;; for the portable dumper (this is currently the branch pdumper).
   ;; (default "emacs-27.0.50")
   dotspacemacs-emacs-pdumper-executable-file "emacs-27.0.50"

   ;; Name of the Spacemacs dump file. This is the file will be created by the
   ;; portable dumper in the cache directory under dumps sub-directory.
   ;; To load it when starting Emacs add the parameter `--dump-file'
   ;; when invoking Emacs 27.1 executable on the command line, for instance:
   ;;   ./emacs --dump-file=~/.emacs.d/.cache/dumps/spacemacs.pdmp
   ;; (default spacemacs.pdmp)
   dotspacemacs-emacs-dumper-dump-file "spacemacs.pdmp"

   ;; If non-nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t

   ;; Maximum allowed time in seconds to contact an ELPA repository.
   ;; (default 5)
   dotspacemacs-elpa-timeout 5

   ;; Set `gc-cons-threshold' and `gc-cons-percentage' when startup finishes.
   ;; This is an advanced option and should not be changed unless you suspect
   ;; performance issues due to garbage collection operations.
   ;; (default '(100000000 0.1))
   dotspacemacs-gc-cons '(100000000 0.1)

   ;; If non-nil then Spacelpa repository is the primary source to install
   ;; a locked version of packages. If nil then Spacemacs will install the
   ;; latest version of packages from MELPA. (default nil)
   dotspacemacs-use-spacelpa nil

   ;; If non-nil then verify the signature for downloaded Spacelpa archives.
   ;; (default nil)
   dotspacemacs-verify-spacelpa-archives nil

   ;; If non-nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil

   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'. (default 'emacs-version)
   dotspacemacs-elpa-subdirectory 'emacs-version

   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'hybrid

   ;; If non-nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil

   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official

   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'.
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7)
                                (agenda . nil)
                                (todos . nil))

   ;; True if the home buffer should respond to resize events. (default t)
   dotspacemacs-startup-buffer-responsive t

   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'lisp-interaction-mode

   ;; Initial message in the scratch buffer, such as "Welcome to Spacemacs!"
   ;; (default nil)
   dotspacemacs-initial-scratch-message nil

   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press `SPC T n' to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(spacemacs-light
                         spacemacs-dark)

   ;; Set the theme for the Spaceline. Supported themes are `spacemacs',
   ;; `all-the-icons', `custom', `doom', `vim-powerline' and `vanilla'. The
   ;; first three are spaceline themes. `doom' is the doom-emacs mode-line.
   ;; `vanilla' is default Emacs mode-line. `custom' is a user defined themes,
   ;; refer to the DOCUMENTATION.org for more info on how to create your own
   ;; spaceline theme. Value can be a symbol or list with additional properties.
   ;; (default '(spacemacs :separator wave :separator-scale 1.5))
   dotspacemacs-mode-line-theme '(spacemacs :separator wave :separator-scale 1.5)

   ;; If non-nil the cursor color matches the state color in GUI Emacs.
   ;; (default t)
   dotspacemacs-colorize-cursor-according-to-state t

   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   dotspacemacs-default-font '("Iosevka Term"
                               :size 32
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)

   ;; The leader key (default "SPC")
   dotspacemacs-leader-key "SPC"

   ;; The key used for Emacs commands `M-x' (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"

   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"

   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"

   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","

   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m")
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"

   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs `C-i', `TAB' and `C-m', `RET'.
   ;; Setting it to a non-nil value, allows for separate commands under `C-i'
   ;; and TAB or `C-m' and `RET'.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab t

   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"

   ;; If non-nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil

   ;; If non-nil then the last auto saved layouts are resumed automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil

   ;; If non-nil, auto-generate layout name when creating new layouts. Only has
   ;; effect when using the "jump to layout by number" commands. (default nil)
   dotspacemacs-auto-generate-layout-names nil

   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1

   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache

   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5

   ;; If non-nil, the paste transient-state is enabled. While enabled, after you
   ;; paste something, pressing `C-j' and `C-k' several times cycles through the
   ;; elements in the `kill-ring'. (default nil)
   dotspacemacs-enable-paste-transient-state nil

   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4

   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom

   ;; Control where `switch-to-buffer' displays the buffer. If nil,
   ;; `switch-to-buffer' displays the buffer in the current window even if
   ;; another same-purpose window is available. If non-nil, `switch-to-buffer'
   ;; displays the buffer in a same-purpose window even if the buffer can be
   ;; displayed in the current window. (default nil)
   dotspacemacs-switch-to-buffer-prefers-purpose nil

   ;; If non-nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t

   ;; If non-nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil

   ;; If non-nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil

   ;; If non-nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil


   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90

   ;; If non-nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t

   ;; If non-nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t

   ;; If non-nil unicode symbols are displayed in the mode line.
   ;; If you use Emacs as a daemon and wants unicode characters only in GUI set
   ;; the value to quoted `display-graphic-p'. (default t)
   dotspacemacs-mode-line-unicode-symbols t

   ;; If non-nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t

   ;; Control line numbers activation.
   ;; If set to `t' or `relative' line numbers are turned on in all `prog-mode' and
   ;; `text-mode' derivatives. If set to `relative', line numbers are relative.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; (default nil)
   dotspacemacs-line-numbers nil

   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil

   ;; If non-nil `smartparens-strict-mode' will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil

   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc…
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil

   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all

   ;; If non-nil, start an Emacs server if one is not already running.
   ;; (default nil)
   dotspacemacs-enable-server 't

   ;; Set the emacs server socket location.
   ;; If nil, uses whatever the Emacs default is, otherwise a directory path
   ;; like \"~/.emacs.d/server\". It has no effect if
   ;; `dotspacemacs-enable-server' is nil.
   ;; (default nil)
   dotspacemacs-server-socket-dir nil

   ;; If non-nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil

   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `rg', `ag', `pt', `ack' and `grep'.
   ;; (default '("rg" "ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("rg" "ag" "pt" "ack" "grep")

   ;; Format specification for setting the frame title.
   ;; %a - the `abbreviated-file-name', or `buffer-name'
   ;; %t - `projectile-project-name'
   ;; %I - `invocation-name'
   ;; %S - `system-name'
   ;; %U - contents of $USER
   ;; %b - buffer name
   ;; %f - visited file name
   ;; %F - frame name
   ;; %s - process status
   ;; %p - percent of buffer above top of window, or Top, Bot or All
   ;; %P - percent of buffer above bottom of window, perhaps plus Top, or Bot or All
   ;; %m - mode name
   ;; %n - Narrow if appropriate
   ;; %z - mnemonics of buffer, terminal, and keyboard coding systems
   ;; %Z - like %z, but including the end-of-line format
   ;; (default "%I@%S")
   dotspacemacs-frame-title-format "%I@%S"

   ;; Format specification for setting the icon title format
   ;; (default nil - same as frame-title-format)
   dotspacemacs-icon-title-format nil

   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed' to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup nil

   ;; Either nil or a number of seconds. If non-nil zone out after the specified
   ;; number of seconds. (default nil)
   dotspacemacs-zone-out-when-idle nil

   ;; Run `spacemacs/prettify-org-buffer' when
   ;; visiting README.org files of Spacemacs.
   ;; (default nil)
   dotspacemacs-pretty-docs nil))

(defun dotspacemacs/user-env ()
  "Environment variables setup.
This function defines the environment variables for your Emacs session. By
default it calls `spacemacs/load-spacemacs-env' which loads the environment
variables declared in `~/.spacemacs.env' or `~/.spacemacs.d/.spacemacs.env'.
See the header of this file for more information."
  (spacemacs/load-spacemacs-env))

(defun dotspacemacs/user-init ()
  "Initialization for user code:
This function is called immediately after `dotspacemacs/init', before layer
configuration.
It is mostly for variables that should be set before packages are loaded.
If you are unsure, try setting them in `dotspacemacs/user-config' first."
  ;; Pretty settings from http://blog.lujun9972.win/emacs-document/blog/2018/10/22/ricing-up-org-mode/index.html

  (defmacro set-pair-faces (themes consts faces-alist)
    "Macro for pair setting of custom faces.
THEMES name the pair (theme-one theme-two). CONSTS sets the variables like
  ((sans-font \"Some Sans Font\") ...). FACES-ALIST has the actual faces
like:
  ((face1 theme-one-attr theme-two-atrr)
   (face2 theme-one-attr nil           )
   (face3 nil            theme-two-attr)
   ...)"
    (defmacro get-proper-faces ()
      `(let* (,@consts)
         (backquote ,faces-alist)))

    `(setq theming-modifications
           ',(mapcar (lambda (theme)
                       `(,theme ,@(cl-remove-if
                                   (lambda (x) (equal x "NA"))
                                   (mapcar (lambda (face)
                                             (let ((face-name (car face))
                                                   (face-attrs (nth (cl-position theme themes) (cdr face))))
                                               (if face-attrs
                                                   `(,face-name ,@face-attrs)
                                                 "NA"))) (get-proper-faces)))))
                     themes)))

  (set-pair-faces
   ;; Themes to cycle in
   (doom-molokai spacemacs-light)

   ;; Variables
   ((bg-white           "#fbf8ef")
    (bg-light           "#222425")
    (bg-dark            "#1c1e1f")
    (bg-darker          "#1c1c1c")
    (fg-white           "#ffffff")
    (shade-white        "#efeae9")
    (fg-light           "#655370")
    (dark-cyan          "#008b8b")
    (region-dark        "#2d2e2e")
    (region             "#39393d")
    (slate              "#8FA1B3")
    (keyword            "#f92672")
    (comment            "#525254")
    (builtin            "#fd971f")
    (purple             "#9c91e4")
    (doc                "#727280")
    (type               "#66d9ef")
    (string             "#b6e63e")
    (gray-dark          "#999")
    (gray               "#bbb")
    (sans-font          "Iosevka")
    (serif-font         "Merriweather")
    (et-font            "EtBembo")
    (twee-font          "Trattatello")
    (sans-mono-font     "Iosevka")
    (serif-mono-font    "Iosevka"))

   ;; Settings
   ((variable-pitch
     (:family ,sans-font)
     (:family ,et-font
              :background nil
              :foreground ,bg-dark
              :height 1.0))
    (header-line
     (:background nil :inherit nil)
     (:background nil :inherit nil))
    (eval-sexp-fu-flash
     (:background ,dark-cyan
                  :foreground ,fg-white)
     nil)

    (org-document-title
     (:inherit variable-pitch
               :height 1.3
               :weight normal
               :foreground ,gray)
     (:inherit nil
               :family ,twee-font
               :height 1.8
               :foreground ,bg-dark
               :underline nil))
    (org-document-info
     (:foreground ,gray
                  :slant italic)
     (:height 1.2
              :slant italic))
    (org-level-1
     (:inherit variable-pitch
               :height 1.3
               :weight bold
               :foreground ,keyword
               :background ,bg-dark)
     (:inherit nil
               :family ,twee-font
               :height 1.6
               :weight normal
               :slant normal
               :foreground ,bg-dark))
    (org-level-2
     (:inherit variable-pitch
               :weight bold
               :height 1.2
               :foreground ,gray
               :background ,bg-dark)
     (:inherit nil
               :family ,et-font
               :weight normal
               :height 1.3
               :slant italic
               :foreground ,bg-dark))
    (org-level-3
     (:inherit variable-pitch
               :weight bold
               :height 1.1
               :foreground ,slate
               :background ,bg-dark)
     (:inherit nil
               :family ,et-font
               :weight normal
               :slant italic
               :height 1.2
               :foreground ,bg-dark))
    (org-level-4
     (:inherit variable-pitch
               :weight bold
               :height 1.1
               :foreground ,slate
               :background ,bg-dark)
     (:inherit nil
               :family ,et-font
               :weight normal
               :slant italic
               :height 1.1
               :foreground ,bg-dark))
    (org-level-5
     (:inherit variable-pitch
               :weight bold
               :height 1.1
               :foreground ,slate
               :background ,bg-dark)
     nil)
    (org-level-6
     (:inherit variable-pitch
               :weight bold
               :height 1.1
               :foreground ,slate
               :background ,bg-dark)
     nil)
    (org-level-7
     (:inherit variable-pitch
               :weight bold
               :height 1.1
               :foreground ,slate
               :background ,bg-dark)
     nil)
    (org-level-8
     (:inherit variable-pitch
               :weight bold
               :height 1.1
               :foreground ,slate
               :background ,bg-dark)
     nil)
    (org-headline-done
     (:strike-through t)
     (:family ,et-font
              :strike-through t))
    (org-quote
     (:background ,bg-dark)
     (:family ,et-font))
    (org-block
     (:background ,bg-dark)
     (:background nil
                  :family ,sans-mono-font
                  :foreground ,bg-dark))
    (org-block-begin-line
     (:background ,bg-dark)
     (:background nil
                  :height 0.8
                  :family ,sans-mono-font
                  :foreground ,slate))
    (org-block-end-line
     (:background ,bg-dark)
     (:background nil
                  :height 0.8
                  :family ,sans-mono-font
                  :foreground ,slate))
    (org-document-info-keyword
     (:foreground ,comment)
     (:height 0.8
              :foreground ,gray))
    (org-link
     (:underline nil
                 :weight normal
                 :foreground ,slate)
     (:foreground ,bg-dark))
    (org-special-keyword
     (:height 0.9
              :foreground ,comment)
     (:family ,sans-mono-font
              :height 0.8))
    (org-todo
     (:foreground ,builtin
                  :background ,bg-dark)
     nil)
    (org-done
     (:inherit variable-pitch
               :foreground ,dark-cyan
               :background ,bg-dark)
     nil)
    (org-agenda-current-time
     (:foreground ,slate)
     nil)
    (org-hide
     nil
     (:foreground ,bg-white))
    (org-indent
     (:inherit org-hide)
     (:inherit (org-hide fixed-pitch)))
    (org-time-grid
     (:foreground ,comment)
     nil)
    (org-warning
     (:foreground ,builtin)
     nil)
    (org-date
     nil
     (:family ,sans-mono-font
              :height 0.8))
    (org-agenda-structure
     (:height 1.3
              :foreground ,doc
              :weight normal
              :inherit variable-pitch)
     nil)
    (org-agenda-date
     (:foreground ,doc
                  :inherit variable-pitch)
     (:inherit variable-pitch
               :height 1.1))
    (org-agenda-date-today
     (:height 1.5
              :foreground ,keyword
              :inherit variable-pitch)
     nil)
    (org-agenda-date-weekend
     (:inherit org-agenda-date)
     nil)
    (org-scheduled
     (:foreground ,gray)
     nil)
    (org-upcoming-deadline
     (:foreground ,keyword)
     nil)
    (org-scheduled-today
     (:foreground ,fg-white)
     nil)
    (org-scheduled-previously
     (:foreground ,slate)
     nil)
    (org-agenda-done
     (:inherit nil
               :strike-through t
               :foreground ,doc)
     (:strike-through t
                      :foreground ,doc))
    (org-ellipsis
     (:underline nil
                 :foreground ,comment)
     (:underline nil
                 :foreground ,comment))
    (org-tag
     (:foreground ,doc)
     (:foreground ,doc))
    (org-table
     (:background nil)
     (:family ,serif-mono-font
              :height 0.9
              :background ,bg-white))
    (org-code
     (:inherit font-lock-builtin-face)
     (:inherit nil
               :family ,serif-mono-font
               :foreground ,comment
               :height 0.9)))))


(defun dotspacemacs/user-load ()
  "Library to load while dumping.
This function is called only while dumping Spacemacs configuration. You can
`require' or `load' the libraries of your choice that will be included in the
dump.")


(defun dotspacemacs/user-config ()
  "Configuration for user code:
 This function is called at the very end of Spacemacs startup, after layer
 configuration.
 Put your configuration code here, except for variables that should be set
 before packages are loaded."
  (persistent-scratch-setup-default)

  (setq auto-save-interval 20)
  (setq auto-save-timeout 10)

  (defvar --backup-directory (concat user-emacs-directory "backups"))
  (if (not (file-exists-p --backup-directory))
      (make-directory --backup-directory t))
  (setq backup-directory-alist `(("." . ,--backup-directory)))
  (setq make-backup-files t               ; backup of a file the first time it is saved.
        backup-by-copying t               ; don't clobber symlinks
        version-control t                 ; version numbers for backup files
        delete-old-versions t             ; delete excess backup files silently
        delete-by-moving-to-trash t
        kept-old-versions 6               ; oldest versions to keep when a new numbered backup is made (default: 2)
        kept-new-versions 9               ; newest versions to keep when a new numbered backup is made (default: 2)
        auto-save-default t               ; auto-save every buffer that visits a file
        auto-save-timeout 20              ; number of seconds idle time before auto-save (default: 30)
        auto-save-interval 200            ; number of keystrokes between auto-saves (default: 300)
        vc-make-backup-files t)            ; Make backups of version controlled files


  (setq helm-ff-auto-update-initial-value 't)

  (global-fasd-mode 1)

  (setq vc-follow-symlinks 't)

  ;; Some beautification tips from http://blog.lujun9972.win/emacs-document/blog/2018/10/22/ricing-up-org-mode/

  (add-hook 'org-mode-hook (lambda () (progn
                                        (variable-pitch-mode)
                                        (visual-line-mode)
                                        (setq line-spacing 1)
                                        (setq header-line-format " ")
                                        (setq left-margin-width 2)
                                        (setq right-margin-width 2)
                                        (hl-line-mode -1)
                                        (set-window-buffer nil (current-buffer)))))

  (add-hook 'purescript-mode-hook 'turn-on-purescript-indentation)

  (setq deft-extensions '("org" "md" "markdown" "txt" "wiki"))
  (setq deft-directory "~/Private/wiki/")
  (setq deft-recursive t)
  (global-set-key (kbd "C-c d") 'deft)

  (global-set-key (kbd "<redo>") 'undo-tree-redo)
  (global-set-key (kbd "<XF86Cut>") 'clipboard-kill-region)
  (global-set-key (kbd "<XF86Copy>") 'clipboard-kill-ring-save)
  (global-set-key (kbd "<XF86Paste>") 'clipboard-yank)
  (global-set-key (kbd "<mouse-8>") 'previous-buffer)
  (global-set-key (kbd "<mouse-9>") 'next-buffer)

  (global-set-key (kbd "C-c i") 'org-clock-in)
  (global-set-key (kbd "C-c o") 'org-clock-out)
  (global-set-key (kbd "C-c g") 'org-clock-goto)

  (global-set-key (kbd "M-c") 'kill-ring-save)
  (global-set-key (kbd "M-v") 'yank)

  (setq spaceline-org-clock-p t)

  (defun dob-person-filename (person-name)
    (let* ((name-file  (downcase (replace-regexp-in-string "[^[:alnum:]]" "-" (string-trim person-name))))
           (filename (concat "~/Private/wiki/people/" name-file ".org")))
      filename))

  (defun dob-person-make-region (start end)
    "Create a file for a mentioned person"
    (interactive "r")
    (let* ((person-name (buffer-substring start end))
           (filename (dob-person-filename person-name)))
      (org-store-link start)
      (let ((link (caar org-stored-links)))
        (if (string-prefix-p "notmuch:id:" link)
            (with-temp-buffer
              (call-process-shell-command (format "notmuch search --output=files id:%s | xargs cat | email2vcard" (substring link 11)) nil t nil)
              (beginning-of-buffer)
              (setq filename (string-trim (thing-at-point 'line))))))
      (find-file-other-window filename)
      (org-insert-last-stored-link 1)))

  (defun dob-person-make (person-name)
    (interactive "MPerson:")
    (find-file-other-window (dob-person-filename person-name)))

  (defun dob-wiki-url (name)
    "Convert a wiki page into a (emacs-accessible) URL. If it's local, return a filename. If it's remote (i.e. we're not on lifeboat), return a remote TRAMP url."
    (let* ((prefix (if (equal (getenv "SHORTHOST") "lifeboat") "file:///home/danny/Private/wiki/" "/ssh:danny@l4:/home/danny/Private/wiki/"))
           (suffix ".org"))
      (concat prefix (downcase (replace-regexp-in-string "[^[:alnum:]]" "-" (string-trim name))) suffix)))

  (require 'org-attach)
  (setq org-link-abbrev-alist '(("att" . org-attach-expand-link)
                                ("people" . "file:///%(dob-person-filename)")
                                ("wiki" . "%(dob-wiki-url)")))

  (setq org-agenda-files (remove-if-not 'file-exists-p '("~/Private/org/" "~/todo.org")))
  (if (string-equal "yacht" (getenv "SHORTHOST"))
      (setq org-agenda-files '("~/Private/org/yacht.org" "~/Private/org/codetherapy-guix.org")))

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((gnuplot . t)
     (python . t)
     (scheme . t)))

  (require 'ol-git-link)
  (defun dob-double-click (p)
    "My general purpose double click"
    (interactive "d")
    (message (get-text-property p 'org-category))
    (cond ((string= "todo" (get-text-property p 'org-category))
           (org-todo "DONE"))))
  (global-set-key [double-mouse-1] 'dob-double-click)

  (setq geiser-active-implementations '(guile))
  (setq geiser-repl-save-debugging-history-p t)

  (global-set-key (kbd "C-u") 'universal-argument)
  (define-key evil-normal-state-map "g\C-g" 'count-words)
  (define-key evil-motion-state-map "C-u" 'universal-argument))

(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
  (custom-set-variables
   ;; custom-set-variables was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(custom-safe-themes
     (quote
      ("fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" default)))
   '(evil-want-Y-yank-to-eol nil)
   '(initial-major-mode (quote lisp-interaction-mode))
   '(mm-text-html-renderer (quote gnus-w3m))
   '(mml-secure-openpgp-encrypt-to-self t)
   '(notmuch-archive-tags (quote ("-inbox" "+archive")))
   '(org-agenda-custom-commands
     (quote
      (("n" "Agenda and all TODOs"
        ((agenda "" nil)
         (alltodo "" nil))
        nil)
       ("F" "EFF todos" tags-todo "EFF" nil))))
   '(org-agenda-include-diary t)
   '(org-babel-load-languages (quote ((gnuplot . t) (python . t) (scheme . t) (C . t) (clojure . t) (haskell . t))))
   '(org-capture-templates
     (quote
      (("c" "Commonplace" entry
        (file+headline "~/Private/org/daylog.org" "Commonplace")
        "* %T - %?" :unnarrowed t)
       ("d" "Distraction" entry
        (file+headline "~/Private/org/daylog.org" "Distractions")
        "* %T %?" :unnarrowed t)
       ("l" "Day[l]og" entry
        (file+headline "~/Private/org/daylog.org" "This Week")
        "* %T %?" :unnarrowed t)
       ("t" "Todo" entry
        (file+headline "~/Private/org/todo.org" "Inbox")
        "* TODO %?
  %i
  %a" :unnarrowed t)
       ("n" "Note" entry
        (file "~/Private/wiki/notes.org")
        "* %? %^g
Entered on %U
  %i
  %a")
       ("j" "Journal entry" entry
        (function org-journal-find-location)
        "* %(format-time-string org-journal-time-format)%^{Title}
%i%?"))) t)
   '(org-modules
     (quote
      (ol-bibtex org-habit ol-info org-mouse org-protocol org-panel org-checklist org-eval ol-git-link ol-man org-notify ol-notmuch org-bbdb org-bibtex org-docview org-eww org-gnus org-info org-irc org-mhe org-rmail org-w3m org-checklist org-protocol org-pretty-table)))
   '(package-selected-packages
     (quote
      (lsp-ui helm-lsp dap-mode lsp-treemacs company-lsp xterm-color vterm shell-pop multi-term eshell-z eshell-prompt-extras esh-help yapfify stickyfunc-enhance pytest pyenv-mode py-isort pippel pipenv pyvenv pip-requirements lsp-python-ms python live-py-mode importmagic epc ctable concurrent deferred helm-pydoc helm-gtags helm-cscope xcscope ggtags cython-mode counsel-gtags counsel swiper ivy company-anaconda blacken anaconda-mode pythonic pandoc-mode ox-pandoc yasnippet-snippets ws-butler writeroom-mode winum which-key web-mode web-beautify volatile-highlights vi-tilde-fringe uuidgen use-package unfill treemacs-projectile treemacs-evil toc-org tagedit symon string-inflection spaceline-all-the-icons smeargle slim-mode scss-mode sass-mode restart-emacs rainbow-delimiters pug-mode prettier-js popwin persp-mode persistent-scratch pcre2el password-generator paradox overseer orgit org-projectile org-present org-checklist org-pomodoro org-mime org-journal org-download org-bullets org-brain open-junk-file nameless mwim move-text mmm-mode markdown-toc magit-svn magit-gitflow macrostep lorem-ipsum link-hint indent-guide impatient-mode hungry-delete hl-todo highlight-parentheses highlight-numbers highlight-indentation helm-xref helm-themes helm-swoop helm-purpose helm-projectile helm-org-rifle helm-notmuch helm-mode-manager helm-make helm-gitignore helm-git-grep helm-flx helm-descbinds helm-css-scss helm-company helm-c-yasnippet helm-ag google-translate golden-ratio gnuplot gitignore-templates gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe git-gutter-fringe+ gh-md fuzzy font-lock+ flyspell-correct-helm flycheck-pos-tip flycheck-package flx-ido fill-column-indicator fasd fancy-battery eyebrowse expand-region evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-textobj-line evil-surround evil-org evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit evil-lisp-state evil-lion evil-indent-plus evil-iedit-state evil-goggles evil-exchange evil-escape evil-ediff evil-cleverparens evil-args evil-anzu eval-sexp-fu emmet-mode elisp-slime-nav editorconfig dumb-jump dotenv-mode doom-modeline diminish diff-hl define-word counsel-projectile company-web company-statistics column-enforce-mode clean-aindent-mode centered-cursor-mode browse-at-remote beeminder guix auto-yasnippet auto-highlight-symbol auto-dictionary auto-compile aggressive-indent ace-link ace-jump-helm-line ac-ispell)))
   '(split-width-threshold 140)
   '(undo-tree-auto-save-history t)
   '(undo-tree-history-directory-alist (quote ((".*" . "~/.emacs.d/.cache/undo/")))))
  (custom-set-faces
   ;; custom-set-faces was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(header-line ((t (:background nil :inherit nil))))
   '(org-agenda-date ((t (:inherit variable-pitch :height 1.1))))
   '(org-agenda-done ((t (:strike-through t :foreground "#727280"))))
   '(org-block ((t (:background nil :family "Iosevka" :foreground "#1c1e1f"))))
   '(org-block-begin-line ((t (:background nil :height 0.8 :family "Iosevka" :foreground "#8FA1B3"))))
   '(org-block-end-line ((t (:background nil :height 0.8 :family "Iosevka" :foreground "#8FA1B3"))))
   '(org-code ((t (:inherit nil :family "Iosevka" :foreground "#525254" :height 0.9))))
   '(org-date ((t (:family "Iosevka" :height 0.8))))
   '(org-document-info ((t (:height 1.2 :slant italic))))
   '(org-document-info-keyword ((t (:height 0.8 :foreground "#bbb"))))
   '(org-document-title ((t (:inherit nil :family "Trattatello" :height 1.8 :foreground "#1c1e1f" :underline nil))))
   '(org-ellipsis ((t (:underline nil :foreground "#525254"))))
   '(org-headline-done ((t (:family "EtBembo" :strike-through t))))
   '(org-hide ((t (:foreground "#fbf8ef"))))
   '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
   '(org-level-1 ((t (:inherit nil :family "Trattatello" :height 1.6 :weight normal :slant normal :foreground "#1c1e1f"))))
   '(org-level-2 ((t (:inherit nil :family "EtBembo" :weight normal :height 1.3 :slant italic :foreground "#1c1e1f"))))
   '(org-level-3 ((t (:inherit nil :family "EtBembo" :weight normal :slant italic :height 1.2 :foreground "#1c1e1f"))))
   '(org-level-4 ((t (:inherit nil :family "EtBembo" :weight normal :slant italic :height 1.1 :foreground "#1c1e1f"))))
   '(org-link ((t (:foreground "#1c1e1f"))))
   '(org-quote ((t (:family "EtBembo"))))
   '(org-special-keyword ((t (:family "Iosevka" :height 0.8))))
   '(org-table ((t (:family "Iosevka" :height 0.9 :background "#fbf8ef"))))
   '(org-tag ((t (:foreground "#727280"))))
   '(variable-pitch ((t (:family "EtBembo" :background nil :foreground "#1c1e1f" :height 1.0)))))
  )
