;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
;;
(add-load-path! (concat doom-user-dir "/lisp/"))
;; Fun set of utilities to push org to Notion and Slack
(require 'dob-org-push)
;; Great set of keys to make documentation browsing easier
(require 'cc-doc-mode-ux) ;; in ~/..config/doom/lisp/

;; Name etc
(setq user-full-name "Danny O'Brien"
      user-mail-address "danny@spesh.com")

(setq dob-org-file (expand-file-name "~/Private/org/wiki/onebig.org"))

(setq display-line-numbers-type nil)

(let ((my-info (expand-file-name "~/.local/share/info")))
  (when (file-directory-p my-info)
    (if (boundp 'Info-additional-directory-list)
        (add-to-list 'Info-additional-directory-list my-info)
      (setq Info-additional-directory-list (list my-info)))))

(setq auth-sources '("~/.authinfo.gpg"))

(advice-add #'doom-modeline-segment--modals :override #'ignore)

(setq find-file-visit-truename t) ;; Helps with confusing symlinks, especially with org-roam see https://www.orgroam.com/manual.html#Getting-Started

;;; Windows, Popups and So On
;;;
;; Focus new window after splitting
(setq evil-split-window-below t
      evil-vsplit-window-right t)

(after! vterm
  (set-popup-rule! "*doom:vterm-popup:main" :size 0.25 :vslot -4 :select t :quit nil :ttl 0 :side 'right))

;; I like context menus
(when (fboundp 'context-menu-mode)
  (context-menu-mode))

;; And I like menu bars
(when (fboundp 'menu-bar-mode)
  (menu-bar-mode))

(after! 'evil
 (setq evil-want-C-u-scroll nil))

(map! :n "C-u" #'universal-argument
      :i "C-u" #'universal-argument
      :v "C-u" #'universal-argument)

;; change `org-directory'. It must be set before org loads!
(setq org-directory (file-truename (expand-file-name "~/Private/org/")))
(setq org-list-allow-alphabetical nil)

(if (file-exists-p "~/.guix-profile/bin/emacsql-sqlite")
    (setq emacsql-sqlite-executable  "~/.guix-profile/bin/emacsql-sqlite"))

(let ((tempdir (concat (getenv "HOME") "/tmp/emacs/")))
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


(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c i") 'org-clock-in)
(global-set-key (kbd "C-c o") 'org-clock-out)
(global-set-key (kbd "C-c g") 'org-clock-goto)
(global-set-key (kbd "C-x C-b") 'consult-buffer) ;; I hate typing C-x C-b when I meant C-x b
(global-set-key (kbd "M-c") 'kill-ring-save)
(global-set-key (kbd "M-v") 'yank)

;; KEYBOARD
;;

;; I prefer these the other way to the default on Macs
(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'super))



(when ())
(map!
 :desc "Count words" :n "g C-g" 'count-words
 :desc "Previous buffer" :n "H" 'previous-buffer
 :desc "Next buffer" :n "L" 'next-buffer
 :desc "Next window config" :n "M-l" 'winner-redo
 :desc "Prev window config" :n "M-h" 'winner-undo)

(map! (:leader
       :desc "Open Scratch Buffer" "b s" 'doom/switch-to-scratch-buffer
       :desc "Open Messages Buffer" "b m" (lambda () (interactive) (switch-to-buffer (messages-buffer)))))

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


;; Clipboards
;;

(when  (eq system-type 'darwin)
  (defun copy-from-osx ()
    (shell-command-to-string "pbpaste"))

  (defun paste-to-osx (text &optional push)
    (let ((process-connection-type nil))
      (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
        (process-send-string proc text)
        (process-send-eof proc))))

  (setq interprogram-cut-function 'paste-to-osx)
  (setq interprogram-paste-function 'copy-from-osx))

(when (getenv "WAYLAND_DISPLAY")
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
  (setq interprogram-paste-function 'wl-paste))

;; Org-Roam
;;
(after! org-roam
  (setq org-roam-directory (file-truename (expand-file-name "~/Private/org/wiki/"))))
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

;; Sly-Mode and Other Lispiness
;; (load (expand-file-name "~/.roswell/helper.el")) ;; Get on board the ros train

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

  (org-crypt-use-before-save-magic)
  (setq org-tags-exclude-from-inheritance (quote ("crypt" "PROJECT")))
  ;; GPG key to use for encryption
  ;; Either the Key ID or set to nil to use symmetric encryption.
  (setq org-crypt-key "E1E70D6E64BA8D1F74E78285E5001906A3FDE45E")
  (setq org-startup-indented t
        org-todo-keywords '((sequence "TODO" "WAITING" "|"  "DONE" "DELEGATED" "CANCELED"))
        org-agenda-span 3
        org-agenda-start-day "-2d"
        org-agenda-block-separator ""
        org-archive-location "archives/%s_archive::")

 (run-with-timer 0 10 'write-current-clock-entry-to-file)

 (defun write-current-clock-entry-to-file (args)
   "Write the current Org clock entry directly to a file."
   (let ((file-path "~/.current-task")
         (content (if (org-clocking-p)
                      (org-clock-get-clock-string)
                    "")))
     ;; Write the content directly to the file
     (message args)
     (write-region content nil file-path)))

; Maybe Doom already does this GC hack?
 (add-function :after after-focus-change-function 'garbage-collect))
(advice-add 'org-clock-in :after #'write-current-clock-entry-to-file)
(advice-add 'org-clock-out :after #'write-current-clock-entry-to-file)
(advice-add 'org-clock-cancel :after #'write-current-clock-entry-to-file)

(use-package! plz

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
         ("w" "Work Schedule" ((agenda "") (tags-todo "FILECOIN") (tags-todo "-FILECOIN" ((org-agenda-files (--remove (s-matches? "MyHabits.org" it) (org-agenda-files))))))))))

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
                 :file-path "daylog"))


 (setq org-super-agenda-mode t)
 (setq org-super-agenda-header-map nil)
 (face-spec-set 'org-agenda-date '((t :height 1.5 :box t :inverse-video t)))
 (face-spec-set 'org-agenda-structure '((t :height 1.1)))
 (face-spec-set 'org-super-agenda-header'((t :height 1.5)))

 (defvar dob-journal-ql  '(and (tags "JOURNAL") (not (ancestors (tags "JOURNAL")))))

;; Old Journal code
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

 (defun dob-add-journal-todo ()
   "Add a new todo at the end of the journal subtree"
   (interactive)
   (org-roam-dailies-goto-today)
   (let* ((org-roam-daily-directory (file-truename (expand-file-name org-roam-dailies-directory org-roam-directory)))
          (org-roam-today "~/Private/org/wiki/onebig.org")
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

 (defun dob-daylog () (interactive)
        (setq org-attach-id-dir "~/Private/org/wiki/data/")
        (setq dob-org-file "~/Private/org/daylog.org")
        (setq org-agenda-files (cl-remove-if-not 'file-exists-p '("~/Private/org/wiki/onebig.org"))))

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
          :publishing-function org-html-publish-to-html))))
;;
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
         :publishing-function org-html-publish-to-html))

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
   (org-time-stamp '(16))))



;; Authorization!!

(defun dob-auth-secret (host login)
  "Pull out a password from authinfo using HOST and LOGIN."
  (funcall
   (plist-get
    (car (auth-source-search :host host :max 1 :login login)) :secret)))

;; GPT
;;
(defun dob-get-value-from-pw (search-text &optional separator encrypted-file)
  "Search for line containing SEARCH-TEXT in ENCRYPTED-FILE (defaults to ~/Private/pw.gpg)
and return text after SEPARATOR (defaults to ':')."
  (let ((sep (or separator ":"))
        (file (or encrypted-file "~/Private/pw.gpg")))
    (with-temp-buffer
      (call-process "gpg" nil t nil "--quiet" "--decrypt" (expand-file-name file))
      (goto-char (point-min))
      (if (search-forward search-text nil t)
          (progn
            (beginning-of-line)
            (if (re-search-forward (concat ".*" search-text ".*?" sep " *\\(.*\\)") (line-end-position) t)
                (string-trim (match-string 1))
              nil))
        nil))))

(gptel-make-anthropic "Claude"
  :stream t
  :key (lambda () (dob-get-value-from-pw "ANTHROPIC_API_KEY" "=")))

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
           :channels ("#emacs-circe")))))


  ;; KEYBOARD
(map!
 (:prefix "C-c"
          :desc "Add a new journal entry" "x" 'dob-add-journal-todo
          :desc "Add a new org-roam item" "y" 'org-roam-node-insert
          :desc "Insert a ORG timestamp" "t" 'dob-org-insert-time-now
          :desc "Org store link" "M-c" 'org-store-link
          :desc "Org insert link" "M-v" 'org-insert-link-global))

;; Great set of keys to make documentation browsing easier
;;
(require 'cc-doc-mode-ux) ;; in ~/..config/doom/lisp/



;; NOW IT IS TIME FOR THE CUSTOMARY CUSTOMIZATION
;;
