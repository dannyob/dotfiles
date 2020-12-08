;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
;;
(setq user-full-name "Danny O'Brien"
      user-mail-address "danny@spesh.com")

(setq dob-org-file "~/Private/org/daylog.org")

(setq org-list-allow-alphabetical nil)

;; If we have native compilation, let's get compiling!
(when (fboundp 'native-compile-async)
      (setq comp-async-jobs-number 2 ;; not using all cores
            comp-deferred-compilation t
            comp-deferred-compilation-black-list
            '("autoloads.el")))

;; I like tabs
(when (fboundp 'tab-bar-mode)
  (tab-bar-mode))

(map!
 (:prefix "C-c"
  :desc "Start mu4e" "j" '=mu4e
  :desc "Start mu4e" "h" '=mu4e))


;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(defvar dob-hidpi 1 "Scaling factor for HiDPI monitors")
(setq doom-font (font-spec :family "Iosevka" :size (* 16 dob-hidpi)))
(setq doom-variable-pitch-font (font-spec :family "Iosevka Aile"))
(setq doom-big-font (font-spec :family "Iosevka Aile" :size (* 24 dob-hidpi)))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'tango)

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
(setq auto-save-timeout 10)
(setq delete-auto-save-files nil)
(setq auto-save-default t)

;; Guix hack -- Guix's emacs startup stuffs XDG_DATA_DIR with values pointing to various stuff, but
;; does not include XDG_DATA_DIR's defaults, which are /usr/local/share/:/usr/share/

(setenv "XDG_DATA_DIR" (concat (getenv "XDG_DATA_DIR") ":/usr/local/share:/usr/share"))
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

(define-key evil-normal-state-map "g\C-g" 'count-words)
(define-key evil-normal-state-map "H" 'previous-buffer)
(define-key evil-normal-state-map "L" 'next-buffer)

(map!
 (:leader
  :desc "Open Scratch Buffer" "b s" 'doom/switch-to-scratch-buffer
  :desc "Open Messages Buffer" "b m" (lambda () (interactive) (switch-to-buffer (messages-buffer)))))

(defun dob-begin ()
  "Start up my day"
  (interactive)
  (require 'mu4e)
  (require 'org)
  (find-file dob-org-file)
  (set-window-dedicated-p nil t)
  (split-window-horizontally)
  (mu4e~start 'mu4e~main-view))

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
            (beginning-of-buffer)
            (setq filename (string-trim (thing-at-point 'line))))))
    (find-file-other-window filename)
    (org-insert-last-stored-link 1)))

(defun dob-person-make (person-name)
  (interactive "MPerson:")
  (find-file-other-window (dob-person-filename person-name)))


(defun dob-wiki-url (name)
  "Convert a wiki page into a (emacs-accessible) URL. If it's local, return a filename. If it's remote (i.e. we're not on lifeboat), return a remote TRAMP url."
  (let* ((prefix (if (equal (getenv "SHORTHOST") "lifeboat")
                     "file:///home/danny/Private/wiki/"
                   "/ssh:danny@l4:/home/danny/Private/wiki/"))
         (suffix ".org"))
    (concat prefix (downcase (replace-regexp-in-string "[^[:alnum:]]" "-" (string-trim name))) suffix)))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((gnuplot . t)
   (python . t)
   (scheme . t)))

(setq geiser-active-implementations '(guile))

;; Guix helpers
;; From [[info:guix#The%20Perfect%20Setup][info:guix#The Perfect Setup]]
(with-eval-after-load 'yasnippet
  (add-to-list 'yas-snippet-dirs "~/Public/src/guix/etc/snippets"))


;; Mail Stuff

(setenv "EMAIL_QUEUE_QUIET" "t")


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
   mu4e-sent-folder   "/sent"       ;; folder for sent messages
   mu4e-drafts-folder "/drafts"     ;; unfinished messages
   mu4e-trash-folder  "/trash")      ;; trashed messages
  (setq mu4e-use-fancy-chars t)
  (setq mu4e-attachment-dir "~/tmp")
  (setq mu4e-change-filenames-when-moving t)
  (setq mu4e-get-mail-command "syncandspam")
  (setq mu4e-split-view nil) ;; I like a mutt-like division of labour

  ;; Get ORG-mode composing working
  ;;
  ;;;; https://matt.hackinghistory.ca/2016/11/18/sending-html-mail-with-mu4e/
  ;; (defun htmlize-and-send ()
  ;;   "When in an org-mu4e-compose-org-mode message, htmlize and send it."
  ;;   (interactive)
  ;;   (when (member 'org~mu4e-mime-switch-headers-or-body post-command-hook)
  ;;     (org-mime-htmlize)
  ;;     (org-mu4e-compose-org-mode)
  ;;     (mu4e-compose-mode)
  ;;     (message-send-and-exit)))

  ;; ;; This overloads the amazing C-c C-c commands in org-mode with one more function
  ;; ;; namely the htmlize-and-send, above.
  ;; (add-hook 'org-ctrl-c-ctrl-c-hook 'htmlize-and-send t)

  ;; ;; Originally, I set the `mu4e-compose-mode-hook' here, but
  ;; ;; this new hook works much, much better for me.
  ;; (add-hook 'mu4e-compose-post-hook
  ;;           (defun do-compose-stuff ()
  ;;             "My settings for message composition."
  ;;             (org-mu4e-compose-org-mode)))

  (setq org-mime-export-options '(:section-numbers nil
                                  :with-author nil
                                  :with-toc nil))


  (map!
   (:prefix "C-c"
    :desc "Jump to Inbox" "j" (defun dob-jump-to-inbox () (interactive) (mu4e~headers-search-execute "m:/INBOX AND NOT tag:spam-guess AND NOT tag:spam-corpus AND NOT tag:boring-guess AND NOT tag:boring-corpus AND NOT tag:notification-guess AND NOT tag:notification-corpus"  't))
    :desc "Jump to Current Headers" "h" (lambda () (interactive (let ((i (get-buffer "*mu4e-headers*"))) (if i (switch-to-buffer "*mu4e-headers*") (dob-jump-to-inbox)))))))

  (setq mu4e-action-tags-completion-list '("spam-corpus"  "ham-corpus"  "boring-corpus" "notification-corpus"))
  (add-to-list 'mu4e-headers-actions '("tTag message" . mu4e-action-retag-message))
  (add-to-list 'mu4e-view-actions '("tTag message" . mu4e-action-retag-message))

  ;; gnus-inherited email viewer
  (setq mu4e-view-use-gnus 't)


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
    (overlay-put o 'face '(background-color . "#FF0000" ))
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

(map!
   :map (mu4e-headers-mode-map)
   :n "za" 'mu4e-headers-toggle-thread-folding
   :n "zM" 'mu4e-headers-fold-all)

;; Below is more evilly correct but doesn't work with fold module in doom-emacs
;; because fold module remaps evil-fold-action keys.
;; (setq evil-fold-list (cl-remove-if (lambda (e) (eq (caar e) 'mu4e-headers-mode)) evil-fold-list))
;; (add-to-list 'evil-fold-list `((mu4e-headers-mode) :open-all nil :close-all ,(lambda () (call-interactively 'mu4e-headers-fold-all)) :toggle ,(lambda () (call-interactively 'mu4e-headers-toggle-thread-folding)) :open nil :open-rec  nil :close nil))

  ;; Colorize headers based on tags
  ;;

  (defvar dob-mu4e-tag-colors '(("boring-guess" . "gray") ("boring-corpus" . "gray") ("spam-corpus" . "gainsboro")  ("spam-guess". "gainsboro") ("notification-corpus" . "dark grey") ("notification-guess" . "dark gray")))

  (defun dob-mu4e~headers-line-apply-tag-face (msg line)
    "Adjust LINE's face property based on the MSG's mailing-list value."
    (let* ((ml (mu4e-message-field msg :tags))
           (tagl (mapcar 'car dob-mu4e-tag-colors))
           (tag (cl-intersection ml tagl :test 'equal))
           (face (if tag
                     `(:foreground ,(assoc-default (cl-first tag) dob-mu4e-tag-colors))
                   'mu4e-header-face)))
      (when (fboundp 'add-face-text-property)
        (add-face-text-property 0 (length line) face t line))
      line))

  (add-to-list 'mu4e~headers-line-handler-functions
               'dob-mu4e~headers-line-apply-tag-face)

  (setq mu4e-bookmarks
        '((:name "Unread messages" :query "flag:unread AND NOT flag:trashed" :key 117)
          (:name "Today's messages" :query "date:today..now" :key 116)
          (:name "Last 7 days" :query "date:7d..now" :hide-unread t :key 119)
          (:name "Inbox (Clean)" :query "m:/INBOX AND NOT tag:spam-guess AND NOT tag:spam-corpus AND NOT tag:boring-guess AND NOT tag:boring-corpus" :key 118)
          (:name "Suspected Spam" :query "m:/INBOX AND (tag:spam-guess OR tag:spam-corpus)" :key 120)
          (:name "Suspected Boring And Notifications" :query "m:/INBOX AND (tag:boring-guess OR tag:boring-corpus OR tag:notification-guess tag:notification-corpus)" :key 121)))

  (setq mu4e-refile-folder
        (defun dob-refile-to-archive (msg)
          (cond
           ((cl-intersection (mu4e-message-field msg :tags) '("spam-guess" "spam-corpus") :test 'equal) "/missedspam")
           ((mu4e-message-field msg :date) (concat "/archive" (format-time-string "%Y" (mu4e-message-field msg :date))))
           (t  (concat "/archive" (format-time-string "%Y"))))))

  ;; I prefer to be able to switch between org-msg-mode and not
  (remove-hook 'mu4e-compose-pre-hook 'org-msg-mode)

  (map!
   :map (mu4e-headers-mode-map)
   :n "x" 'mu4e-headers-mark-for-something
   :n "e" (defun dob-mu4e-mark-execute () (interactive) "Execute marked items." (mu4e-mark-execute-all t))
   :n "M-SPC" 'mu4e-view-scroll-up-or-next
   :n "i" 'mu4e-select-other-view
   :n "o" 'org-msg-mode
   :n "T"  (defun dob-mu4e-refile-thread () (interactive) "Mark whole thread for refiling" (mu4e-headers-mark-thread-using-markpair '(refile)))
   :map (gnus-article-mode-map)
   :n "M-SPC" 'mu4e-view-scroll-up-or-next
   :n "i" 'mu4e-select-other-view))



;; Org-mode
;;

(after! org
  (require 'ol-info)
  (require 'ol-eww)
  (require 'org-ql)
  (require 'org-attach)
  (require 'org-crypt)
  (org-crypt-use-before-save-magic)
  (setq org-tags-exclude-from-inheritance (quote ("crypt")))
;; GPG key to use for encryption
;; Either the Key ID or set to nil to use symmetric encryption.
  (setq org-crypt-key "E1E70D6E64BA8D1F74E78285E5001906A3FDE45E")
  (setq org-startup-indented t
        org-todo-keywords '((sequence "TODO" "WAITING" "|" "CANCELED" "DONE" "DELEGATED"))
        org-bullets-bullet-list '(" ")
        org-ellipsis "  "
        org-pretty-entities t
        org-hide-emphasis-markers t
        org-agenda-span 3
        org-agenda-start-day "-1d"
        org-agenda-block-separator ""
        org-agenda-include-diary t
        org-fontify-whole-heading-line t
        org-fontify-done-headline t
        org-fontify-quote-and-verse-blocks t)

  (defun dob-add-journal-todo ()
    "Add a new todo at the end of the journal subtree"
    (interactive)
    (let* ((journal-loc (org-ql-select (org-agenda-files) '(and (tags "JOURNAL") (not (ancestors (tags "JOURNAL")))) :action '(cons (point) (current-buffer))))
           (jbuf (cdar journal-loc))
           (jloc (caar journal-loc)))
      (if-let (jwin (get-buffer-window jbuf))
          (select-window jwin)
          (switch-to-buffer jbuf))
      (goto-char jloc)
      (org-insert-todo-subheading nil)
      (dob-org-insert-time-now nil)
      (org-todo "")
      (insert " ")))

  (defun dob-daylog () (interactive)
         (setq org-attach-id-dir "~/Private/wiki/data/")
         (setq dob-org-file "~/Private/org/daylog.org")
         (setq org-link-abbrev-alist '(("att" . org-attach-expand-link)
                                       ("people" . "file:///%(dob-person-filename)")
                                       ("wiki" . "%(dob-wiki-url)")))
         (setq org-agenda-files (cl-remove-if-not 'file-exists-p '("~/Private/org/" "~/todo.org"))))

  (defun dob-yacht () (interactive)
         (setq dob-org-file "~/Private/org/codetherapy.org")
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
           :publishing-function org-html-publish-to-html)))

  (setq orgit-export-alist
        (append orgit-export-alist
                '(("git.savannah.gnu.org/git[:/]\\(.+\\)$" "https://git.savannah.gnu.org/cgit/%n" "https://git.savannah.gnu.org/cgit/%n/log/?h=%r" "https://git.savannah.gnu.org/cgit/%n/commit/?id=%r"))))

  (setq org-log-done 'time)
  (defun dob-org-insert-time-now (arg)
    "Insert a timestamp with today's time and date."
    (interactive "P")
    (org-time-stamp '(16)))

  (map!
   (:prefix "C-c"
    :desc "Add a new journal entry" "x" 'dob-add-journal-todo
    :desc "Org store link" "M-c" 'org-store-link
    :desc "Org insert link" "M-v" 'org-insert-link-global)))
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
