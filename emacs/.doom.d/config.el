;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Danny O'Brien"
      user-mail-address "danny@spesh..com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "Iosevka Term" :size 32))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'kaolin-valley-light)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Private/org/")

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

(setq emacsql-sqlite-executable  "~/.guix-profile/bin/emacsql-sqlite")
(after! persistent-scratch
  (persistent-scratch-setup-default))

(setq auto-save-interval 20)
(setq auto-save-timeout 10)

;; (defvar --backup-directory (concat users-emacs-directory "backups"))
;;(if (not (file-exists-p --backup-directory))
;;      (make-directory --backup-directory t))
;;(setq backup-directory-alist `(("." . ,--backup-directory)))
;; (setq make-backup-files t               ; backup of a file the first time it is saved.
;;       backup-by-copying t               ; don't clobber symlinks
;;       version-control t                 ; version numbers for backup files
;;       delete-old-versions t             ; delete excess backup files silently
;;       delete-by-moving-to-trash t
;;       kept-old-versions 6               ; oldest versions to keep when a new numbered backup is made (default: 2)
;;       kept-new-versions 9               ; newest versions to keep when a new numbered backup is made (default: 2)
;;       auto-save-default t               ; auto-save every buffer that visits a file
;;       auto-save-timeout 20              ; number of seconds idle time before auto-save (default: 30)
;;       auto-save-interval 200            ; number of keystrokes between auto-saves (default: 300)
;;       vc-make-backup-files t            ; Make backups of version controlled files

;; (use-package org-roam
;;   :hook
;;   (after-init . org-roam-mode)
;;   :custom
;;   (org-roam-directory "/home/danny/Private/wiki/")
;;   :bind (:map org-roam-mode-map
;;          (("C-c d l" . org-roam)
;;           ("C-c d f" . org-roam-find-file)
;;           ("C-c d b" . org-roam-switch-to-buffer)
;;           ("C-c d g" . org-roam-graph-show))
;;          :map org-mode-map
;;          (("C-c d i" . org-roam-insert))))

;; Guix hack -- Guix's emacs startup stuffs XDG_DATA_DIR with values pointing to various stuff, but
;; does not include XDG_DATA_DIR's defaults, which are /usr/local/share/:/usr/share/

(setenv "XDG_DATA_DIR" (concat (getenv "XDG_DATA_DIR") ":/usr/local/share:/usr/share"))
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

;; (setq spaceline-org-clock-p t)          ;

;; (defun dob-begin ()
;;   "Start up my day"
;;   (interactive)
;;   (switch-to-buffer "daylog.org")
;;   (set-window-dedicated-p nil t)
;;   (org-id-goto "ac128a00-0af4-43e5-942e-38a2f36afd28")
;;   (split-window-horizontally)
;;   (dob-notmuch-now))

;; (defun dob-person-filename (person-name)
;;   (let* ((name-file  (downcase (replace-regexp-in-string "[^[:alnum:]]" "-" (string-trim person-name))))
;;          (filename (concat "~/Private/wiki/people/" name-file ".org")))
;;     filename))

;; (defun dob-person-make-region (start end)
;;   "Create a file for a mentioned person"
;;   (interactive "r")
;;   (let* ((person-name (buffer-substring start end))
;;          (filename (dob-person-filename person-name)))
;;     (org-store-link start)
;;     (let ((link (caar org-stored-links)))
;;       (if (string-prefix-p "notmuch:id:" link)
;;           (with-temp-buffer
;;             (call-process-shell-command (format "notmuch search --output=files id:%s | xargs cat | email2vcard" (substring link 11)) nil t nil)
;;             (beginning-of-buffer)
;;             (setq filename (string-trim (thing-at-point 'line))))))
;;     (find-file-other-window filename)
;;     (org-insert-last-stored-link 1)))

;; (defun dob-person-make (person-name)
;;   (interactive "MPerson:")
;;   (find-file-other-window (dob-person-filename person-name)))

;; (defun dob-wiki-url (name)
;;   "Convert a wiki page into a (emacs-accessible) URL. If it's local, return a filename. If it's remote (i.e. we're not on lifeboat), return a remote TRAMP url."
;;   (let* ((prefix (if (equal (getenv "SHORTHOST") "lifeboat") "file:///home/danny/Private/wiki/" "/ssh:danny@l4:/home/danny/Private/wiki/"))
;;          (suffix ".org"))
;;     (concat prefix (downcase (replace-regexp-in-string "[^[:alnum:]]" "-" (string-trim name))) suffix)))

;; (require 'org-attach)

(setq org-attach-id-dir "~/Private/wiki/data/")
(setq org-link-abbrev-alist '(("att" . org-attach-expand-link)
                              ("people" . "file:///%(dob-person-filename)")
                              ("wiki" . "%(dob-wiki-url)")))

(setq org-agenda-files (remove-if-not 'file-exists-p '("~/Private/org/" "~/todo.org")))
(if (string-equal "yacht" (getenv "SHORTHOST"))
    (setq org-agenda-files '("~/Private/org/yacht.org" "~/Private/org/codetherapy-guix.org")))

;; (org-babel-do-load-languages
;;  'org-babel-load-languages
;;  '((gnuplot . t)
;;    (python . t)
;;    (scheme . t)))

;; (require 'ol-git-link)

;; (defun dob-double-click (p)
;;   "My general purpose double click"
;;   (interactive "d")
;;   (message (get-text-property p 'org-category))
;;   (cond ((string= "todo" (get-text-property p 'org-category))
;;          (org-todo "DONE"))))
;; (global-set-key [double-mouse-1] 'dob-double-click)

(setq geiser-active-implementations '(guile))
;; (setq geiser-repl-save-debugging-history-p t)

;; Guix helpers
;; From [[info:guix#The%20Perfect%20Setup][info:guix#The Perfect Setup]]
;(with-eval-after-load 'geiser-guile
;  (add-to-list 'geiser-guile-load-path "~/Public/src/guix"))
(with-eval-after-load 'yasnippet
  (add-to-list 'yas-snippet-dirs "~/Public/src/guix/etc/snippets"))


(global-set-key (kbd "C-u") 'universal-argument)
(define-key evil-normal-state-map "g\C-g" 'count-words)
(define-key evil-motion-state-map "C-u" 'universal-argument)

;; Mail Stuff
(setq
  mail-envelope-from 'header
  mail-specify-envelope-from t
  message-kill-buffer-on-exit t
  message-sendmail-envelope-from 'header
  message-sendmail-extra-arguments '("--read-envelope-from")
  message-sendmail-f-is-evil t
  mm-text-html-renderer (quote gnus-w3m)
  mml-secure-openpgp-encrypt-to-self t
  send-mail-function (quote sendmail-send-it)
  message-send-mail-function 'message-send-mail-with-sendmail
  sendmail-program "msmtpq"
  user-mail-address "danny@spesh.com")

(setenv "EMAIL_QUEUE_QUIET" "t")


;; Notmuch setup
;;
(defvar dob-notmuch-spam-tags '("-inbox" "+spam" "+missedspam"))

(defun dob-notmuch-today ()
  "Show just today's inbox"
  (interactive)
  (notmuch-search "tag:inbox AND ( date:today OR date:yesterday )"))

(defun dob-notmuch-now ()
  "Show inbox now"
  (interactive)
  (notmuch-search "tag:inbox"))

(defun dob-notmuch-spamify
    (&optional unspam beg end)
  "Mark as spam the currently selected thread or region.

     Archive each message in the currently selected thread by applying
     the tag changes in `dob-notmuch-spam-tags' to each (remove the
         \"inbox\" tag by default). If a prefix argument is given, the
     messages will be \"unspammed\" (i.e. the tag changes in
     `dob-notmuch-spam-tags' will be reversed).

     This function advances the next thread when finished."
  (interactive (cons current-prefix-arg (notmuch-interactive-region)))
  (when dob-notmuch-spam-tags
    (notmuch-search-tag
     (notmuch-tag-change-list dob-notmuch-spam-tags unspam) beg end))
  (when (eq beg end)
    (notmuch-search-next-thread)))

(defun dob-notmuch-show-spamify-message (&optional unspam)
    "Mark the current message as spam.

Spamify the current message by applying the tag changes in
`dob-spam-tags' to it. If a prefix argument is given, the
message will be \"unarchived\", i.e. the tag changes in
`dob-spam-tags' will be reversed."
    (interactive "P")
    (when dob-notmuch-spam-tags
      (apply 'notmuch-show-tag-message
             (notmuch-tag-change-list dob-notmuch-spam-tags unspam))))

(defun dob-notmuch-show-spamify-message-then-next-or-next-thread ()
  "Mark as spam the current message, then show the next open message in the current thread.

If at the last open message in the current thread, then show next
thread from search."
  (interactive)
  (dob-notmuch-show-spamify-message)
  (unless (notmuch-show-next-open-message)
    (notmuch-show-next-thread t)))

(defmacro dob-with-inbox (s) `(with-current-buffer "*notmuch-saved-search-inbox*" ,s))

(defun dob-limit-to-author ()
  "Limit the inbox to mail written by the authors in
the current thread. Useful for finding other conversations, and as a substitute
for sorting by author."
  (interactive)
  (notmuch-search-filter (format "from:\"%s\"" (notmuch-search-find-authors))))

(defun dob-notmuch-ml-tag (iur)
  "Tag for future machine-learning. Takes three numerical attributes between 0-5:
   * Importance -- how bad would it be if I didn't see this
   * Urgency    -- how quickly should I be shown this?
   * Relevance  -- how connected is this to my actual work?
"
  (interactive "sImportance/Urgency/Relevance:")
  (if (string-equal iur "spam") (setq iur "000"))
  (let ((importance (string-to-number (substring iur 0 1)))
        (urgency (string-to-number (substring iur 1 2)))
        (relevance (string-to-number (substring iur 2 3))))
   (notmuch-show-tag-message
    (concat "+importance=" (number-to-string importance))
    (concat "+urgency=" (number-to-string urgency))
    (concat "+relevance=" (number-to-string relevance)))
   (if (= 0 (or importance urgency relevance))
      (dob-notmuch-spamify))))

(use-package notmuch
  :bind (("C-c t" . dob-notmuch-today)
         ("C-c n" . dob-notmuch-now)
         :map notmuch-search-mode-map
         ("C-c g" . notmuch-poll-refresh-this-buffer)
         ("S"     . dob-notmuch-spamify)
         ("F"     . dob-limit-to-author)
         :map notmuch-show-mode-map
         ("T"     . dob-notmuch-ml-tag)
         ("S"     . dob-notmuch-show-spamify-message-then-next-or-next-thread)))

(evil-define-key 'normal notmuch-message-mode-map (kbd "ZZ") 'notmuch-draft-save)

;; Org-mode
;;
(setq org-journal-dir "~/Private/wiki/journal/")
(setq org-journal-file-type 'weekly)
(setq org-journal-file-format "%Y-%m-%d.org")
(defun org-journal-date-format-func (time)
  "Custom function to insert journal date header.

  When buffer is empty prepend a header in front the entry header."
  (concat (when (= (buffer-size) 0)
            (concat
             (pcase org-journal-file-type
               (`daily "#+TITLE: Daily Journal\n")
               (`weekly "#+TITLE: Weekly Journal\n")
               (`monthly "#+TITLE: Monthly Journal\n")
               (`yearly "#+TITLE: Yearly Journal\n"))))
          org-journal-date-prefix
          (format-time-string "%A, %F" time)))
(setq org-journal-date-format 'org-journal-date-format-func)
(add-to-list 'org-agenda-files org-journal-dir)

(defun org-journal-find-location ()
  ;; Open today's journal, but specify a non-nil prefix argument in order to
  ;; inhibit inserting the heading; org-capture will insert the heading.
  (org-journal-new-entry t)
  ;; Position point on the journal's top-level heading so that org-capture
  ;; will add the new entry as a child entry.
  (goto-char (point-min)))

(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/Private/org/todo.org" "Inbox")
         "* TODO %?\n  %i\n  %a")
        ("n" "Note" entry (file "~/Private/wiki/notes.org")
         "* %? %^g\nEntered on %U\n  %i\n  %a")
        ("j" "Journal entry" entry (function
                                    org-journal-find-location)
         "* %(format-time-string org-journal-time-format)%^{Title}\n%i%?")))

(if (string-equal "yacht" (getenv "SHORTHOST"))
    (setq org-capture-templates
          '(("t" "Todo" entry (file+headline "~/Private/org/yacht.org" "Inbox")
             "* TODO %?\n  %i\n  %a")
            ("n" "Note" entry (file "~/Private/wiki/notes.org")
             "* %? %^g\nEntered on %U\n  %i\n  %a"))))

(setq org-todo-keywords
      '((sequence "TODO" "WAITING" "|" "CANCELED" "DONE" "DELEGATED")))

(setq org-startup-indented t
      org-bullets-bullet-list '(" ")
      org-ellipsis "  "
      org-pretty-entities t
      org-hide-emphasis-markers t
      org-agenda-block-separator ""
      org-fontify-whole-heading-line t
      org-fontify-done-headline t
      org-fontify-quote-and-verse-blocks t)

(defun dob-org-insert-time-now (arg)
   "Insert a timestamp with today's time and date."
   (interactive "P")
   (org-time-stamp '(16)))