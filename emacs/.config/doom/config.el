; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
;;
(add-load-path! (concat doom-user-dir "/lisp/"))
;; Fun set of utilities to push org to Notion and Slack
(require 'dob-org-push)
;; Great set of keys to make documentation browsing easier
(require 'cc-doc-mode-ux)

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


;; Functions to track current task in ~/.current-task
(defun dob-write-current-task-to-file ()
  "Write current clocked task headline to ~/.current-task file."
  (when (org-clocking-p)
    (with-temp-file "~/.current-task"
      (insert (org-clock-get-clock-string)))))

(defun dob-clear-current-task-file ()
  "Clear the ~/.current-task file by writing an empty string."
  (with-temp-file "~/.current-task"
    (insert "")))

;; Add advice to org-clock functions
(advice-add 'org-clock-in :after (lambda (&rest _) (dob-write-current-task-to-file)))
(advice-add 'org-clock-out :after (lambda (&rest _) (dob-clear-current-task-file)))

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

(when (eq system-type 'darwin)
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
        org-agenda-files (list dob-org-file)
        org-archive-location "archives/%s_archive::")

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
          :publishing-function org-html-publish-to-html)))

   ;; Another go at org-capture, too
   ;;
 (setq org-default-notes-file dob-org-file)
 (setq org-capture-templates
       '(("t" "Todo Inbox" entry
          (file+headline "~/Private/org/wiki/onebig.org" "Todo Inbox")
          "*** TODO %?\n" :prepend nil)

         ("s" "Scheduled Todo" entry
          (file+headline "~/Private/org/wiki/onebig.org" "Todo Inbox")
          "* TODO %^{Scheduling Todo}\nSCHEDULED: %T\n:PROPERTIES:\n:Effort: %^{Effort|5m|10m|15m|20m|30m}\n:END:"
          :unnarrowed t
          :immediate-finish t
          :jump-to-captured t)))

 (setq org-log-done 'time))

(defun dob-org-insert-time-now (arg)
  "Insert a timestamp with today's time and date."
  (interactive "P")
  (org-time-stamp '(16)))

(defun dob-add-journal-todo (&optional todo-text)
  "Add a new todo at the end of the journal subtree"
  (interactive)
  (let* ((original-buffer (current-buffer))
         (original-window (selected-window))
         (org-roam-today "~/Private/org/wiki/onebig.org")
         (journal-loc (org-ql-select org-roam-today dob-journal-ql :action '(list (point) (current-buffer))))
         (jbuf (cadar journal-loc))
         (jloc (caar journal-loc)))
    (if-let (jwin (get-buffer-window jbuf))
        (select-window jwin)
      (switch-to-buffer jbuf))
    (goto-char jloc)
   (org-insert-todo-subheading nil)
   (dob-org-insert-time-now nil)
   (org-todo "")
   (insert " ")
   (when todo-text
     (insert todo-text)
     (select-window original-window)
     (switch-to-buffer original-buffer))))

(defun ai-add-journal (todo-text)
  (let ((original-buffer (current-buffer))
        (original-window (selected-window)))
    (dob-add-journal-todo (concat "✨ " todo-text))
    (message "hello")
    (select-window original-window)
    (switch-to-buffer original-buffer)))


(defun dob-auth-secret (host login)
  "Pull out a password from authinfo using HOST and LOGIN. Returns nil if not found."
  (let ((auth-info (car (auth-source-search :max 1 :host host :login login))))
    (when auth-info
      (funcall (plist-get auth-info :secret)))))
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

(after! gptel
  (gptel-make-anthropic "Claude"
                              :stream t
                              :key (lambda () (dob-get-value-from-pw "ANTHROPIC_API_KEY" "="))))

(after! spell-fu
  ;; TODO workround for https://github.com/doomemacs/doomemacs/issues/6246
  (unless (file-exists-p ispell-personal-dictionary)
    (make-directory (file-name-directory ispell-personal-dictionary) t)
    (with-temp-file ispell-personal-dictionary
      (insert (format "personal_ws-1.1 %s 0\n" ispell-dictionary)))))

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

;; Custom function to find OneBig.org buffer
(defun ai-find-onebig ()
  "Find or create the OneBig.org buffer, and return the buffer. Useful for doing
  in-emacs modifications of OneBig"
  (interactive)
  (let* ((onebig-path (expand-file-name "~/Private/nextcloud/org/wiki/OneBig.org"))
         (buffer (find-buffer-visiting onebig-path)))
    (if buffer
        buffer
      (find-file-noselect onebig-path))))

(defun buffer-whole-string (buffer)
  "Return a whole buffer as a string. Useful for dumping a buffer into an AI conversation."
  (with-current-buffer buffer
    (save-restriction
      (widen)
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun ai-get-onebig ()
  "Shortcut to dump a OneBig into an AI prompt/conversation."
  (buffer-whole-string (ai-find-onebig)))

;; Function to quickly mark items as done in an action plan buffer
(defun ai-mark-item-done ()
  "Mark the current line with [X] if it has [ ]."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (when (search-forward "[ ]" (line-end-position) t)
      (replace-match "[X]"))))

;; Function to count completed vs total action items
(defun ai-action-plan-progress ()
  "Count and display completed tasks vs total tasks in buffer."
  (interactive)
  (let ((completed 0)
        (total 0))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "\\[[ X]\\]" nil t)
        (setq total (1+ total))
        (when (string= (match-string 0) "[X]")
          (setq completed (1+ completed)))))
    (message "%d of %d items completed (%.1f%%)"
             completed total
             (if (> total 0) (* 100.0 (/ completed (float total))) 0.0))))

;; Bind these to convenient keys in markdown-mode
(map! :map markdown-mode-map
      :n "C-c d" #'ai-mark-item-done
      :i "C-c d" #'ai-mark-item-done
      :n "C-c p" #'ai-action-plan-progress
      :i "C-c p" #'ai-action-plan-progress)

;; Function to quickly add TODOs to the OneBig.org inbox
(defun ai-add-todo (headline &optional scheduled description)
  "Add a new TODO item to the Todo Inbox in OneBig.org.
HEADLINE is the title of the todo item.
SCHEDULED is an optional date string like \"<2025-03-15 Sat>\".
DESCRIPTION is an optional string with bullet points (use - for each line)."
  (interactive "sTodo headline: \nsScheduled (optional, format <YYYY-MM-DD Day>): \nsDescription (optional): ")
  (let ((original-buffer (current-buffer))
        (original-window (selected-window)))
    (with-current-buffer (ai-find-onebig)
      (org-find-exact-headline-in-buffer "Todo Inbox")
      (goto-char (point-at-eol))
      (insert "\n** TODO " headline)
      (when (and scheduled (not (string-empty-p scheduled)))
        (insert "\nSCHEDULED: " scheduled))
      (when (and description (not (string-empty-p description)))
        (insert "\n" description))
      (save-buffer))
    (select-window original-window)
    (switch-to-buffer original-buffer)
    (message "Todo added to inbox: %s" headline)))

;; AI assistant helper function
(defun ai-load-context ()
  "Load onebig.org and return a summary of current priorities.
   Use this as the first function when starting a conversation with Claude."
  (let ((onebig-content (ai-get-onebig))
        (today (format-time-string "%Y-%m-%d"))
        (summary ""))
    (setq summary (concat "OneBig.org loaded successfully. Today is " today ".

"
                          "Current priorities:
"
                          "1. Check TODO items scheduled for today
"
                          "2. Review recent journal entries
"
                          "3. Process inbox items

"
                          "Use (ai-get-onebig) for the full content if needed."))
    summary))

;; AI-assisted capture functions for Danny's workflow

(defun ai-quick-capture (text)
  "Quickly add TEXT to the Capture section in OneBig.org with timestamp."
  (interactive "sCapture: ")
  (with-current-buffer (ai-find-onebig)
    (goto-char (point-min))
    (when (re-search-forward "^\\* Capture" nil t)
      (org-end-of-subtree)
      (insert (format "\n** <%s> %s\n"
                      (format-time-string "%Y-%m-%d %a %H:%M")
                      text))
      (save-buffer)
      (message "Captured: %s" text))))

(defun ai-capture-meeting (person topic notes)
  "Capture meeting notes with PERSON about TOPIC."
  (interactive "sMet with: \nsAbout: \nsNotes: ")
  (ai-quick-capture
   (format "MEETING: %s - Topic: %s\n   %s" person topic notes)))

(defun ai-capture-aspiration (goal)
  "Capture an aspiration or reminder."
  (interactive "sRemember to: ")
  (ai-quick-capture (format "ASPIRATION: %s" goal)))

(defun ai-capture-fact (fact)
  "Capture a fact or piece of information."
  (interactive "sFact/Info: ")
  (ai-quick-capture (format "FACT: %s" fact)))

(defun ai-process-captures ()
  "Review and process items from Capture section interactively."
  (interactive)
  (with-current-buffer (ai-find-onebig)
    (goto-char (point-min))
    (when (re-search-forward "^\\* Capture" nil t)
      (org-narrow-to-subtree)
      (message "Processing captures. Use C-c C-w to refile items."))))

;; Add keybindings for quick access
(global-set-key (kbd "C-c c q") 'ai-quick-capture)
(global-set-key (kbd "C-c c m") 'ai-capture-meeting)
(global-set-key (kbd "C-c c a") 'ai-capture-aspiration)
(global-set-key (kbd "C-c c f") 'ai-capture-fact)
(global-set-key (kbd "C-c c p") 'ai-process-captures)
