;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
;;
(setq user-full-name "Danny O'Brien"
      user-mail-address "danny@spesh.com")

;; If we have native compilation, let's get compiling!
(when (fboundp 'native-compile-async)
      (setq comp-async-jobs-number 2 ;; not using all cores
            comp-deferred-compilation t
            comp-deferred-compilation-black-list
            '("autoloads.el")))

;; I like tabs
(when (fboundp 'tab-bar-mode)
  (tab-bar-mode))


;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "Iosevka" :size 32))
(setq doom-variable-pitch-font (font-spec :family "Iosevka Aile"))
(setq doom-big-font (font-spec :family "Iosevka Aile" :size 48))

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
   (switch-to-buffer "daylog.org")
   (set-window-dedicated-p nil t)
   (org-id-goto "ac128a00-0af4-43e5-942e-38a2f36afd28")
   (split-window-horizontally)
   (dob-notmuch-now))

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
    user-mail-address "danny@codetherapy.space")

  (setq mail-user-agent 'mu4e-user-agent)
  (setq
   mu4e-sent-folder   "/sent"       ;; folder for sent messages
   mu4e-drafts-folder "/drafts"     ;; unfinished messages
   mu4e-trash-folder  "/trash"      ;; trashed messages
   mu4e-refile-folder "/archive2020")   ;; saved messages
  (setq mu4e-use-fancy-chars t)
  (setq mu4e-attachment-dir "~/tmp")
  (setq mu4e-change-filenames-when-moving t)
  (setq mu4e-get-mail-command "mbsync -a")
  (setq mu4e-split-view nil) ;; I like a mutt-like division of labour

  ;; Get ORG-mode composing working
  ;;
  ;;;; https://matt.hackinghistory.ca/2016/11/18/sending-html-mail-with-mu4e/
  (defun htmlize-and-send ()
    "When in an org-mu4e-compose-org-mode message, htmlize and send it."
    (interactive)
    (when (member 'org~mu4e-mime-switch-headers-or-body post-command-hook)
      (org-mime-htmlize)
      (org-mu4e-compose-org-mode)
      (mu4e-compose-mode)
      (message-send-and-exit)))

  ;; This overloads the amazing C-c C-c commands in org-mode with one more function
  ;; namely the htmlize-and-send, above.
  (add-hook 'org-ctrl-c-ctrl-c-hook 'htmlize-and-send t)

  ;; Originally, I set the `mu4e-compose-mode-hook' here, but
  ;; this new hook works much, much better for me.
  (add-hook 'mu4e-compose-post-hook
            (defun do-compose-stuff ()
              "My settings for message composition."
              (org-mu4e-compose-org-mode)))

  (setq org-mime-export-options '(:section-numbers nil
                                  :with-author nil
                                  :with-toc nil))
  (map!
   :map (mu4e-headers-mode-map mu4e-view-mode-map)
   :n "x" 'mu4e-headers-mark-for-something
   :n "e" (lambda () (interactive) (mu4e-mark-execute-all t))
   :n "M-SPC" 'mu4e-view-scroll-up-or-next
   :n "i" 'mu4e-select-other-view))


;; Org-mode
;;

(after! org
  (require 'ol-info)
  (require 'ol-eww)
  (require 'org-ql)
  (require 'org-attach)

  (setq org-startup-indented t
        org-todo-keywords '((sequence "TODO" "WAITING" "|" "CANCELED" "DONE" "DELEGATED"))
        org-bullets-bullet-list '(" ")
        org-ellipsis "  "
        org-pretty-entities t
        org-hide-emphasis-markers t
        org-agenda-block-separator ""
        org-fontify-whole-heading-line t
        org-fontify-done-headline t
        org-fontify-quote-and-verse-blocks t)

  (defun dob-add-journal-todo ()
    "Add a new todo at the end of the journal subtree"
    (interactive)
    (let ((journal-loc (org-ql-select (org-agenda-files) '(and (tags "JOURNAL") (not (ancestors (tags "JOURNAL")))) :action '(cons (point) (current-buffer)))))
      (switch-to-buffer (cdar journal-loc))
      (goto-char (caar journal-loc))
      (org-insert-todo-subheading nil)
      (dob-org-insert-time-now nil)
      (org-todo "")
      (insert " ")))

  (defun dob-daylog () (interactive)
    (setq org-attach-id-dir "~/Private/wiki/data/")
    (setq org-link-abbrev-alist '(("att" . org-attach-expand-link)
                                  ("people" . "file:///%(dob-person-filename)")
                                  ("wiki" . "%(dob-wiki-url)")))
    (setq org-agenda-files (cl-remove-if-not 'file-exists-p '("~/Private/org/" "~/todo.org"))))

  (defun dob-yacht () (interactive)
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
    :desc "Add a new journal entry" "x" 'dob-add-journal-todo)))

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
