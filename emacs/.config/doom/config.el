;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
;;
(add-load-path! (concat doom-user-dir "/lisp/"))
(require 'dob-org-push)

(setq user-full-name "Danny O'Brien"
      user-mail-address "danny@spesh.com")

(setq display-line-numbers-type nil)

(let ((my-info (expand-file-name "~/.local/share/info")))
  (when (file-directory-p my-info)
    (if (boundp 'Info-additional-directory-list)
        (add-to-list 'Info-additional-directory-list my-info)
      (setq Info-additional-directory-list (list my-info)))))

(setq auth-sources '("~/.authinfo.gpg" "~/Private/dotfiles/authinfo"))

(advice-add #'doom-modeline-segment--modals :override #'ignore)

(setq find-file-visit-truename t) ;; Helps with confusing symlinks, especially with org-roam see https://www.orgroam.com/manual.html#Getting-Started

;;; Windows, Popups and So On
;;;
;; Focus new window after splitting
(setq evil-split-window-below t
      evil-vsplit-window-right t)

(after! vterm
  (set-popup-rule! "*doom:vterm-popup:main" :size 0.25 :vslot -4 :select t :quit nil :ttl 0 :side 'right))

;; Special stuff for WSL
;; (if
;;      (string-match "microsoft"
;;                    (with-temp-buffer (shell-command "uname -r" t)
;;  
;;                                      (delete-char -1)
;;                                      (buffer-string)))
;;      (setq browse-url-browser-function 'browse-url-generic
;;            browse-url-program-generic "PowerShell.exe"
;;            browse-url-generic-args '("-Command" "Start-Process")))
;; 

;; I like context menus
(when (fboundp 'context-menu-mode)
  (context-menu-mode))

;; And I like menu bars
(when (fboundp 'menu-bar-mode)
  (menu-bar-mode))

;; KEYBOARD
(map!
 (:prefix "C-c"
  :desc "Start mu4e" "j" '=mu4e
  :desc "Start mu4e" "h" '=mu4e))

; (defvar dob-hidpi 0.75)
(defvar dob-hidpi 1)
(setq doom-font (font-spec :family "Iosevka" :size (* 16 dob-hidpi)))
(setq doom-variable-pitch-font (font-spec :family "Cream"))
(setq doom-big-font (font-spec :family "Iosevka Aile" :size (* 24 dob-hidpi)))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; (setq doom-theme 'tango)
(setq doom-theme 'kaolin-ocean)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory (file-truename (expand-file-name "~/Private/org/")))
(setq org-list-allow-alphabetical nil)


;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.

(if (file-exists-p "~/.guix-profile/bin/emacsql-sqlite")
    (setq emacsql-sqlite-executable  "~/.guix-profile/bin/emacsql-sqlite"))

(let ((tempdir (concat (getenv "HOME") "/tmp/")))
  (if (file-directory-p tempdir)
      (setq temporary-file-directory tempdir)))

(setq backup-directory-alist
          `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
          `((".*" ,temporary-file-directory t)))

(setq auto-save-interval 20)
(setq auto-save-timeout 3)
(setq delete-auto-save-files nil)
(setq auto-save-default t)

;; Guix hack -- Guix's emacs startup stuffs XDG_DATA_DIR with values pointing to various stuff, but
;; does not include XDG_DATA_DIR's defaults, which are /usr/local/share/:/usr/share/

(setenv "XDG_DATA_DIR" (concat (getenv "XDG_DATA_DIR") ":/usr/local/share:/usr/share"))


;; Keyboard bindings
(global-set-key (kbd "<redo>") 'undo-tree-redo)
(global-set-key (kbd "<XF86Cut>") 'clipboard-kill-region)
(global-set-key (kbd "<XF86Copy>") 'clipboard-kill-ring-save)
(global-set-key (kbd "<XF86Paste>") 'clipboard-yank)
(global-set-key (kbd "<mouse-8>") 'previous-buffer)
(global-set-key (kbd "<mouse-9>") 'next-buffer)

(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c i") 'org-clock-in)
(global-set-key (kbd "C-c o") 'org-clock-out)
(global-set-key (kbd "C-c g") 'org-clock-goto)
(global-set-key (kbd "C-x C-b") 'ivy-switch-buffer) ;; I hate typing C-x C-b when I meant C-x b
(global-set-key (kbd "M-c") 'kill-ring-save)
(global-set-key (kbd "M-v") 'yank)

;; KEYBOARD
(map!
 :desc "Count words" :n "g C-g" 'count-words
 :desc "Previous buffer" :n "H" 'previous-buffer
 :desc "Next buffer" :n "L" 'next-buffer
 :desc "Next window config" :n "M-l" 'winner-redo
 :desc "Prev window config" :n "M-h" 'winner-undo)

(map! (:leader
       :desc "Open Scratch Buffer" "b s" 'doom/switch-to-scratch-buffer
       :desc "Open Messages Buffer" "b m" (lambda () (interactive) (switch-to-buffer (messages-buffer)))))

(defun dob-from ()
  (interactive)
  (save-excursion
    (let ((to-content
           (save-restriction (message-narrow-to-headers)
                             (message-fetch-field "to")))
          (cc-content
           (save-restriction (message-narrow-to-headers)
                             (message-fetch-field "cc"))))
      (message-goto-body)
      (if (re-search-forward "From:[[:space:]]+\\(.*\\)$" nil t)
          (let* ((matchdata (match-data))
                 (start (nth 2 matchdata))
                 (end (nth 3 matchdata))
                 (from-names (buffer-substring start end)))
            (message-goto-to)
            (message-delete-line)
            (insert (concat "To: " from-names "\n"))
            (message-goto-cc)
            (end-of-line)
            (unless (s-blank-str-p cc-content) (insert ", "))
            (insert to-content))))))

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
            (goto-char (point-min))
            (setq filename (string-trim (thing-at-point 'line))))))
    (find-file-other-window filename)
    (org-insert-last-stored-link 1)))

(defun dob-person-make (person-name)
  (interactive "MPerson:")
  (find-file-other-window (dob-person-filename person-name)))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((gnuplot . t)
   (python . t)
   (lisp . t)
   (deno . t)
   (scheme . t)))

(setf org-babel-lisp-eval-fn "sly-eval")
(add-to-list 'org-src-lang-modes '("deno" . typescript))
(setq geiser-active-implementations '(guile))

;; Guix helpers
;; From [[info:guix#The%20Perfect%20Setup][info:guix#The Perfect Setup]]
(with-eval-after-load 'yasnippet
  (add-to-list 'yas-snippet-dirs "~/Public/src/guix/etc/snippets"))

;; Mail Stuff

(setenv "EMAIL_QUEUE_QUIET" "t")


;; Wayland clipboard
;;

(setq wl-copy-process nil)
(defun wl-copy (text)
  (setq wl-copy-process (make-process :name "wl-copy"
                                      :buffer nil
                                      :command '("wl-copy" "-f" "-n")
                                      :connection-type 'pipe))
  (process-send-string wl-copy-process text)
  (process-send-eof wl-copy-process))
(defun wl-paste ()
  (if (and wl-copy-process (process-live-p wl-copy-process))
      nil ; should return nil if we're the current paste owner
    (shell-command-to-string "wl-paste -n | tr -d \r")))
(setq interprogram-cut-function 'wl-copy)
(setq interprogram-paste-function 'wl-paste)

;; Org-Roam
;;
(after! org-roam
  (setq org-roam-directory (file-truename (expand-file-name "~/Private/org/wiki/")))
  (map! :map org-mode-map
        "M-<left>" #'org-roam-dailies-goto-previous-note
        "M-<right>" #'org-roam-dailies-goto-next-note)

  (setq org-roam-capture-templates
        '(("d" "default" plain "%?" :if-new
           (file+head "${slug}.org" "#+title: ${title}\n")
           :unnarrowed t)))
  (setq org-roam-dailies-capture-templates
        `(("d" "default" entry
           "* %T %?"
           :target (file+head "%<%Y-%m-%d>.org"
                              ,(if (file-exists-p   "~/Private/org/wiki/templates/journal.org")
                                   (with-temp-buffer
                                     (insert-file-contents  "~/Private/org/wiki/templates/journal.org")
                                     (buffer-string))
                                 ""))
           :unnarrowed t))))

;; Mu4e!
;;
(after! mu4e
  (setq
   mail-envelope-from 'header
   mail-specify-envelope-from t
   message-kill-buffer-on-exit t
   message-sendmail-envelope-from 'header
   message-sendmail-extra-arguments '("--read-envelope-from")
   message-sendmail-f-is-evil t
   mml-secure-openpgp-encrypt-to-self t
   send-mail-function (quote sendmail-send-it)
   message-send-mail-function 'message-send-mail-with-sendmail
   sendmail-program "msmtpq"
   user-mail-address (cond ((cl-search "eff" (file-truename (getenv "MAILDIR"))) "danny@eff.org")
                           ((cl-search "codetherapy" (file-truename (getenv "MAILDIR"))) "danny@codetherapy.space")
                           (t "danny@spesh.com")))

  (setq mail-user-agent 'mu4e-user-agent)
  (setq
   mu4e-sent-folder   "/Sent Items"       ;; folder for sent messages
   mu4e-drafts-folder "/Drafts"     ;; unfinished messages
   mu4e-trash-folder  "/Deleted Items")      ;; trashed messages
  (setq mu4e-use-fancy-chars t)
  (setq mu4e-attachment-dir "~/tmp")
  (setq mu4e-change-filenames-when-moving t)
  (setq mu4e-get-mail-command "syncandspam")
  (setq mu4e-split-view nil)

  (setq org-mime-export-options '(:section-numbers nil
                                  :with-author nil
                                  :with-toc nil))

   ;; KEYBOARD
  (map!
   (:prefix "C-c"
    :desc "Jump to Inbox" "j" (defun dob-jump-to-inbox () (interactive) (mu4e~headers-search-execute "m:/INBOX AND NOT tag:spam-guess AND NOT tag:spam-corpus AND NOT tag:boring-guess AND NOT tag:boring-corpus AND NOT tag:notification-guess AND NOT tag:notification-corpus"  't))
    :desc "Jump to Current Headers" "h" (lambda () (interactive (let ((i (get-buffer "*mu4e-headers*"))) (if i (switch-to-buffer "*mu4e-headers*") (dob-jump-to-inbox)))))))

  (setq mu4e-action-tags-completion-list '("spam-corpus"  "ham-corpus"  "boring-corpus" "notification-corpus"))
  (add-to-list 'mu4e-headers-actions '("tTag message" . mu4e-action-retag-message))
  (add-to-list 'mu4e-view-actions '("tTag message" . mu4e-action-retag-message))

  ;; Funky Thread Folding!
  ;; From: https://gist.github.com/felipeochoa/614308ac9d2c671a5830eb7847985202
  ;;
 (defun mu4e~headers-msg-unread-p (msg)
   "Check if MSG is unread."
   (let ((flags (mu4e-message-field msg :flags)))
     (and (member 'unread flags) (not (member 'trashed flags)))))

 (defvar mu4e-headers-folding-slug-function
   (lambda (headers) (format " (%d)" (length headers)))
   "Function to call to generate the slug that will be appended to folded threads.
This function receives a single argument HEADERS, which is a list
of headers about to be folded.")

 (defun mu4e~headers-folded-slug (headers)
   "Generate a string to append to the message line indicating the fold status.
HEADERS is a list with the messages being folded (including the root header)."
   (funcall mu4e-headers-folding-slug-function headers))

 (defun mu4e~headers-fold-make-overlay (beg end headers)
   "Hides text between BEG and END using an overlay.
HEADERS is a list with the messages being folded (including the root header)."
   (let ((o (make-overlay beg end)))
     (overlay-put o 'mu4e-folded-thread t)
     (overlay-put o 'face '(background-color . "#FF0000"))
     ;; (overlay-put o 'display (concat (mu4e~headers-folded-slug headers) "\n"))
     (overlay-put o 'evaporate t)
     (overlay-put o 'invisible t)))

 (defun mu4e~headers-fold-find-overlay (loc)
   "Find and return the 'mu4e-folded-thread overlay at LOC, or return nil."
   (cl-dolist (o (overlays-in (1- loc) (1+ loc)))
     (when (overlay-get o 'mu4e-folded-thread)
       (cl-return o))))

 (defun mu4e-headers-fold-all ()
   "Fold all the threads in the current view."
   (interactive)
   (let ((thread-id "") msgs fold-start fold-end)
     (mu4e-headers-for-each
      (lambda (msg)
        (end-of-line)
        (push msg msgs)
        (let ((this-thread-id (mu4e~headers-get-thread-info msg 'thread-id)))
          (if (string= thread-id this-thread-id)
              (setq fold-end (point))
            (when (< 1 (length msgs))
              (mu4e~headers-fold-make-overlay fold-start fold-end (nreverse msgs)))
            (setq fold-start (point)
                  fold-end (point)
                  msgs nil
                  thread-id this-thread-id)))))
     (when (< 1 (length msgs))
       (mu4e~headers-fold-make-overlay fold-start fold-end (nreverse msgs)))))

 (defun mu4e-headers-toggle-thread-folding (&optional subthread)
   "Toggle the folding state for the thread at point.
If SUBTHREAD is non-nil, only fold the current subthread."
   ;; Folding is accomplished using an overlay that starts at the end
   ;; of the parent line and ends at the end of the last descendant
   ;; line. If there's no overlay, it means it isn't folded
   (interactive "P")
   (if-let ((o (mu4e~headers-fold-find-overlay (point-at-eol))))
       (delete-overlay o)
     (let* ((msg (mu4e-message-at-point))
            (thread-id (mu4e~headers-get-thread-info msg 'thread-id))
            (path-re (concat "^" (mu4e~headers-get-thread-info msg 'path)))
            msgs first-marked-point last-marked-point)
       (mu4e-headers-for-each
        (lambda (submsg)
          (when (and (string= thread-id (mu4e~headers-get-thread-info submsg 'thread-id))
                     (or (not subthread)
                         (string-match-p path-re (mu4e~headers-get-thread-info submsg 'path))))
            (push msg msgs)
            (setq last-marked-point (point-at-eol))
            (unless first-marked-point
              (setq first-marked-point last-marked-point)))))
       (when (< 1 (length msgs))
         (mu4e~headers-fold-make-overlay first-marked-point last-marked-point (nreverse msgs))))))

;; KEYBOARD
 (map!
    :map (mu4e-headers-mode-map)
    :n "za" 'mu4e-headers-toggle-thread-folding
    :n "zM" 'mu4e-headers-fold-all)

;; Apparently this helps with text breaking, etc?
;;
 (add-hook 'message-mode-hook 'auto-fill-mode)
 (setq mu4e-compose-format-flowed t)
 (setq message-cite-reply-position 'below)

; With just the above settings sent emails do not wrap correctly in mu4e:view. You may also want to set
 (setq-default fill-column 72)
 (setq fill-flowed-encode-column fill-column)

 ;; Colorize headers based on tags
 ;;

 (defvar dob-mu4e-tag-colors '(("boring-guess" . "gray") ("boring-corpus" . "gray") ("spam-corpus" . "gainsboro")  ("spam-guess". "gainsboro") ("notification-corpus" . "dark grey") ("notification-guess" . "dark gray")))

 (setq mu4e-bookmarks
       '(
         ;; (:name "Unread messages" :query "flag:unread AND NOT flag:trashed" :key 117)
         (:name "Today's messages" :query "date:today..now" :key 116)
         ;; (:name "Last 7 days" :query "date:7d..now" :hide-unread t :key 119)
         (:name "Inbox (No lists)" :query  "m:/INBOX AND NOT list:/.*/ AND NOT tag:spam-guess AND NOT tag:spam-corpus AND NOT tag:boring-guess AND NOT tag:boring-corpus" :key 122)
         (:name "Inbox (Clean)" :query "m:/INBOX AND NOT tag:spam-guess AND NOT tag:spam-corpus AND NOT tag:boring-guess AND NOT tag:boring-corpus" :key 118)
         (:name "Suspected Spam" :query "m:/INBOX AND (tag:spam-guess OR tag:spam-corpus)" :key 120)
         (:name "Suspected Boring And Notifications" :query "m:/INBOX AND (tag:boring-guess OR tag:boring-corpus OR tag:notification-guess tag:notification-corpus)" :key 121)))

 (setq mu4e-refile-folder
       (defun dob-refile-to-archive (msg)
         (cond
          ((cl-intersection (mu4e-message-field msg :tags) '("spam-guess" "spam-corpus") :test 'equal) "/Junk Email")
          ((string-match "reply.github.com" (plist-get (car (mu4e-message-field msg :to)) :email))
           "/github")
          ((mu4e-message-field msg :date)
           (let ((year-folder (concat "/archive" (format-time-string "%Y" (mu4e-message-field msg :date)))))
             (if (string= year-folder "/archive2021") "/Archives" year-folder)))
          (t  (concat "/archive" (format-time-string "%Y"))))))

  ;; I prefer to be able to switch between org-msg-mode and not
 (remove-hook 'mu4e-compose-pre-hook 'org-msg-mode)

 (defun dob-mu4e-my-tags (msg)
   (let ((tags (mu4e-message-field msg :tags)))
     (cl-remove-if-not
      (lambda ($x) (string-match-p (rx (| "-guess" "-corpus")) $x)) tags)))

 (defun dob-mu4e-tag-as (msg tag)
   (let* ((my-tags (dob-mu4e-my-tags msg))
          (del-my-tags (mapcar (lambda ($x) (concat "-" $x)) my-tags))
          (add-tag (list (concat "+" tag))))
     (mu4e-action-retag-message msg (string-join (append add-tag del-my-tags) ","))))

 (defun dob-mu4e-quickspam ()
   (interactive)
   (dob-mu4e-tag-as (mu4e-message-at-point) "spam-corpus")
   (mu4e-headers-mark-for-refile))

 (defun dob-mu4e-quickham ()
   (interactive)
   (dob-mu4e-tag-as (mu4e-message-at-point) "ham-corpus")
   (mu4e-headers-mark-for-refile))

 (defun dob-mu4e-quicknotification ()
   (interactive)
   (dob-mu4e-tag-as (mu4e-message-at-point) "notification-corpus")
   (mu4e-headers-mark-for-refile))

;; KEYBOARD
 (map!
  :map (mu4e-headers-mode-map)
  :n "S" 'dob-mu4e-quickspam
  :n "H" 'dob-mu4e-quickham
  :n "N" 'dob-mu4e-quicknotification
  :n "x" 'mu4e-headers-mark-for-something
  :n "e" (defun dob-mu4e-mark-execute () (interactive) "Execute marked items." (mu4e-mark-execute-all t))
  :n "M-SPC" 'mu4e-view-scroll-up-or-next
  :n "i" 'mu4e-select-other-view
  :n "o" 'org-msg-mode
  :n "T"  (defun dob-mu4e-refile-thread () (interactive) "Mark whole thread for refiling" (mu4e-headers-mark-thread-using-markpair '(refile)))
  :map (gnus-article-mode-map)
  :n "M-SPC" 'mu4e-view-scroll-up-or-next
  :n "i" 'mu4e-select-other-view))

;; Sly-Mode and Other Lispiness
(load (expand-file-name "~/.roswell/helper.el")) ;; Get on board the ros train

(after! sly
  (defun dob-switch-or-select-window (buf)
    (interactive)
    (message "switching")
    (let ((w (get-buffer-window buf)))
      (if w (select-window w) (switch-to-buffer buf))))
  (defun dob-repl-or-code ()
    (interactive)
    (if (equal major-mode 'sly-mrepl-mode)
        (sly-switch-to-most-recent 'lisp-mode)
      (sly-mrepl 'dob-switch-or-select-window)))

  ;; KEYBOARD
  ;; (define-key sly-mode-map (kbd "C-c C-z") 'dob-repl-or-code)
  (map! :after sly-mrepl :map sly-mode-map "C-c C-z" #'dob-repl-or-code))

(when (file-directory-p "/usr/share/doc/hyperspec") (setq
     common-lisp-hyperspec-root "file:///usr/share/doc/hyperspec/"))

;; Org-mode
;;
;;

(after! org
  (require 'ol-info)
  (require 'ol-eww)
  (require 'org-ql)
  (require 'org-attach)
  (require 'org-crypt)
  (require 'org-ai)

  (add-hook 'org-mode-hook #'org-ai-mode)
  (org-ai-install-yasnippets)

  (org-crypt-use-before-save-magic)
  (setq org-tags-exclude-from-inheritance (quote ("crypt" "PROJECT")))
  ;; GPG key to use for encryption
  ;; Either the Key ID or set to nil to use symmetric encryption.
  (setq org-crypt-key "E1E70D6E64BA8D1F74E78285E5001906A3FDE45E")
  (setq org-startup-indented t
        org-todo-keywords '((sequence "TODO" "WAITING" "|"  "DONE" "DELEGATED" "CANCELED"))
        org-bullets-bullet-list '(" ")
        org-ellipsis "  "
        org-pretty-entities t
        org-hide-emphasis-markers t
        org-agenda-span 3
        org-agenda-start-day "-2d"
        org-agenda-block-separator ""
        org-agenda-include-diary t
        org-archive-location "archives/%s_archive::"
        org-fontify-whole-heading-line t
        org-fontify-done-headline t
        org-fontify-quote-and-verse-blocks t))

(use-package! plz)

;; org-transclusion
(use-package! org-transclusion
  :after org
  :init
  (map!
   :map global-map "<f12>" #'org-transclusion-add
   :leader
   :prefix "n"
   :desc "Org Transclusion Mode" "t" #'org-transclusion-mode))

(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)

(setq dob/organization-task-id "a7e9c9b9-0517-4ac5-876a-a1036b63c3a2")

(defun dob/clock-in-organization-task-as-default ()
  (interactive)
  (save-restriction
    (widen)
    (org-with-point-at (org-id-find dob/organization-task-id 'marker)
      (org-clock-in '(16)))))

;; (setq org-agenda-custom-commands
;;       '(("n" "Agenda and all TODOs" ((agenda "") (alltodo "")))
;;         ("w" "Work Schedule" ((agenda "") (tags-todo "FILECOIN") (tags-todo "-FILECOIN" ((org-agenda-files (cl-remove-if (lambda (x) (string-match ".*MyHabits.*" x)) org-agenda-files))))))))
(setq org-agenda-custom-commands
      '(("n" "Agenda and all TODOs" ((agenda "") (alltodo "")))
        ("p" "Projects" ((tags "PROJECT")))
        ("w" "Work Schedule" ((agenda "") (tags-todo "FILECOIN") (tags-todo "-FILECOIN" ((org-agenda-files (--remove (s-matches? "MyHabits.org" it) (org-agenda-files)))))))))

(setq org-super-agenda-groups
        '(
          (:log t)
          (:name "Schedule"
           :time-grid t)
           
          (:name "Today"
           :scheduled today)
          (:name "Wekan"
                 :file-path "wekan")
                 
          (:name "Filecoin"
                 :tag "Filecoin")
          (:name "Daylog"
                 :file-path "daylog")))
          

(setq org-super-agenda-mode t)
(setq org-super-agenda-header-map nil)
(face-spec-set 'org-agenda-date '((t :height 1.5 :box t :inverse-video t)))
(face-spec-set 'org-agenda-structure '((t :height 1.1)))
(face-spec-set 'org-super-agenda-header'((t :height 1.5)))

(defvar dob-journal-ql  '(and (tags "JOURNAL") (not (ancestors (tags "JOURNAL")))))

(defun dob-add-journal-todo ()
  "Add a new todo at the end of the journal subtree"
  (interactive)
  (org-roam-dailies-goto-today)
  (let* ((org-roam-daily-directory (file-truename (expand-file-name org-roam-dailies-directory org-roam-directory)))
         (org-roam-today (concat org-roam-daily-directory (format-time-string "%Y-%m-%d.org")))
         (journal-loc (org-ql-select org-roam-today dob-journal-ql :action '(list (point) (current-buffer))))
         (jbuf (cadar journal-loc))
         (jloc (caar journal-loc)))
    (if-let (jwin (get-buffer-window jbuf))
        (select-window jwin)
      (switch-to-buffer jbuf))
    (goto-char jloc))
  (org-insert-todo-subheading nil)
  (dob-org-insert-time-now nil)
  (org-todo "")
  (insert " "))

(defun dob-goto-journal ()
  "Jump to where journal entry should be added"
  (let* ((org-roam-daily-directory (expand-file-name org-roam-dailies-directory org-roam-directory))
         (org-roam-today (concat org-roam-daily-directory (format-time-string "%Y-%m-%d.org")))
         (journal-loc (org-ql-select org-roam-today dob-journal-ql :action '(list (point) (current-buffer))))
         (jbuf (cadar journal-loc))
         (jloc (caar journal-loc)))
    (if-let (jwin (get-buffer-window jbuf))
        (select-window jwin)
      (switch-to-buffer jbuf))
    (goto-char jloc)))

;; (defun dob-add-journal-todo ()
;;   (interactive)
;;   (org-roam-dailies-capture-today))


(defun dob-daylog () (interactive)
       (setq org-attach-id-dir "~/Private/org/wiki/data/")
       (setq dob-org-file "~/Private/org/daylog.org")
       (setq org-link-abbrev-alist '(("people" . "file:///%(dob-person-filename)")
                                     ("wiki" . "%(dob-wiki-url)")))
       (setq org-agenda-files (cl-remove-if-not 'file-exists-p '("~/Private/org/wiki" "~/Private/org/wiki/daily" "~/Private/org/" "~/todo.org"))))

(defun dob-yacht () (interactive)
       (setq dob-org-file "~/Private/org/codetherapy/yacht.org")
       (setq org-agenda-files '("~/Private/org/codetherapy")))

(if (string-equal "yacht" (getenv "SHORTHOST"))
    (dob-yacht)
  (dob-daylog))

  ;; OMG Org Publishing AGAIN??
(setq org-export-with-broken-links 'mark)
(setq org-publish-project-alist
      '(("codetherapy-dev-blog"
         :base-directory "/home/danny/Private/org/codetherapy/"
         :publishing-directory "/ssh:danny@boat:/var/local/www/codetherapy.space/notes/"
         :publishing-function org-html-publish-to-html)
        ("dannyob-eth-blog"
         :base-directory "/home/danny/Private/org/wiki/daily/"
         :publishing-directory "/home/danny/tmp/diary/"
         :publishing-function org-html-publish-to-html)))

  ;; Another go at org-capture, too
  ;;
(setq org-default-notes-file dob-org-file)
(setq org-capture-templates
      '(("s"
         "Scheduled Todo"
         entry
         (function (lambda () (dob-goto-journal)))
         "* TODO %^{Scheduling Todo}\nSCHEDULED: %T\n:PROPERTIES:\n:Effort: %^{Effort|5m|10m|15m|20m|30m}\n:END:"
         :unnarrowed t
         :immediate-finish t
         :jump-to-captured t)))

(setq org-log-done 'time)

(defun dob-org-insert-time-now (arg)
  "Insert a timestamp with today's time and date."
  (interactive "P")
  (org-time-stamp '(16)))

(defun dob-auth-secret (host login)
  "Pull out a password from authinfo using HOST and LOGIN."
  (funcall
   (plist-get
    (car (auth-source-search :host host :max 1 :login login)) :secret)))

;; Circe and IRC!
(after! circe
  (setq circe-network-options
        `(("ZNC-Libera"
           :tls t
           :nick "malaclyps"
           :realname "Danny O'Brien"
           :pass ,(dob-auth-secret "boat.endofgreatness.com" "znc")
           :host "boat.endofgreatness.com"
           :port "36667"
           :channels ("#emacs-circe")
           ))))

  ;; KEYBOARD
(map!
 (:prefix "C-c"
          :desc "Add a new journal entry" "x" 'dob-add-journal-todo
          :desc "Add a new org-roam item" "y" 'org-roam-node-insert
          :desc "Insert a ORG timestamp" "t" 'dob-org-insert-time-now
          :desc "Org store link" "M-c" 'org-store-link
          :desc "Org insert link" "M-v" 'org-insert-link-global))


;; ChatGPT
;;
(defvar dob-current-conversation-id nil)
(defvar dob-current-prompt-id nil)

(defun dob-remove-nil-values (list)
  (let (result)
    (dolist (elem list result)
      (when (cdr elem)
        (setq result (cons elem result))))))


(defun dob-converse (prompt &optional conversation-id parent-prompt-id)
  "Feed a PROMPT to ChatGPT, keeping track of previous history via
CONVERSATION-ID and PARENT-PROMPT-ID.  Relies on
https://github.com/waylaidwanderer/node-chatgpt-api and chatgpt-api running
locally."
  (if conversation-id (setq dob-current-conversation-id conversation-id))
  (if parent-prompt-id (setq dob-current-prompt-id parent-prompt-id))
  (let* ((json (plz 'post "http://localhost:3000/conversation"
                 :headers '(("Content-Type" . "application/json"))
                 :body (json-encode (dob-remove-nil-values `(("message" . ,prompt)
                                      ("conversationId" . ,conversation-id)
                                      ("parentMessageid" . ,parent-prompt-id))))
                 :as #'json-read
                 :then 'sync))
         (response (alist-get 'response json))
         (conversation-id (alist-get 'conversationId json))
         (message-id (alist-get 'messageId json)))
    (setq dob-current-conversation-id conversation-id)
    (setq dob-current-prompt-id message-id)
    response))


;; Finally, I like a teeny modeline
(setq doom-modeline-height 1)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(mode-line ((t (:height 80))))
 '(mode-line-inactive ((t (:height 80)))))

;; NOW IT IS TIME FOR THE CUSTOMARY CUSTOMIZATION
;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("79278310dd6cacf2d2f491063c4ab8b129fee2a498e4c25912ddaa6c3c5b621e" default)))
