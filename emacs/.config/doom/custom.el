(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-minimum-prefix-length 4)
 '(custom-safe-themes
   '("f058c82b57bc27ff4288f7ff702fcc4d298608c4de5933224aaceb770c0c9e19" "7964b513f8a2bb14803e717e0ac0123f100fb92160dcf4a467f530868ebaae3e" "10e5d4cc0f67ed5cafac0f4252093d2119ee8b8cb449e7053273453c1a1eb7cc" "342f853c3d097d60a01a8e17559d2cc4e6ccd4c8e8c4d32cdfb5d53fdd50ca27" "e8567ee21a39c68dbf20e40d29a0f6c1c05681935a41e206f142ab83126153ca" "2ce76d65a813fae8cfee5c207f46f2a256bac69dacbb096051a7a8651aa252b0" "0c6a36393d5782839b88e4bf932f20155cb4321242ce75dc587b4f564cb63d90" "2f1518e906a8b60fac943d02ad415f1d8b3933a5a7f75e307e6e9a26ef5bf570" "79278310dd6cacf2d2f491063c4ab8b129fee2a498e4c25912ddaa6c3c5b621e" default))
 '(magit-todos-insert-after '(bottom) nil nil "Changed by setter of obsolete option `magit-todos-insert-at'")
 '(package-selected-packages '(aggressive-indent vterm debbugs))
 '(safe-local-variable-values
   '((org-agenda-files . ".")
     (eval progn
      (require 'lisp-mode)
      (defun emacs27-lisp-fill-paragraph
          (&optional justify)
        (interactive "P")
        (or
         (fill-comment-paragraph justify)
         (let
             ((paragraph-start
               (concat paragraph-start "\\|\\s-*\\([(;\"]\\|\\s-:\\|`(\\|#'(\\)"))
              (paragraph-separate
               (concat paragraph-separate "\\|\\s-*\".*[,\\.]$"))
              (fill-column
               (if
                   (and
                    (integerp emacs-lisp-docstring-fill-column)
                    (derived-mode-p 'emacs-lisp-mode))
                   emacs-lisp-docstring-fill-column fill-column)))
           (fill-paragraph justify))
         t))
      (setq-local fill-paragraph-function #'emacs27-lisp-fill-paragraph))
     (eval let
      ((root-dir-unexpanded
        (locate-dominating-file default-directory ".dir-locals.el")))
      (when root-dir-unexpanded
        (let*
            ((root-dir
              (file-local-name
               (expand-file-name root-dir-unexpanded)))
             (root-dir*
              (directory-file-name root-dir)))
          (unless
              (boundp 'geiser-guile-load-path)
            (defvar geiser-guile-load-path 'nil))
          (make-local-variable 'geiser-guile-load-path)
          (require 'cl-lib)
          (cl-pushnew root-dir* geiser-guile-load-path :test #'string-equal))))
     (eval with-eval-after-load 'yasnippet
      (let
          ((guix-yasnippets
            (expand-file-name "etc/snippets/yas"
                              (locate-dominating-file default-directory ".dir-locals.el"))))
        (unless
            (member guix-yasnippets yas-snippet-dirs)
          (add-to-list 'yas-snippet-dirs guix-yasnippets)
          (yas-reload-all))))
     (eval add-to-list 'completion-ignored-extensions ".go")
     (eval add-hook 'after-save-hook
      (lambda nil
        (when
            (and
             (equal
              (file-name-nondirectory
               (buffer-file-name))
              "narrative-arc.org")
             (not
              (buffer-modified-p)))
          (org-export-to-file 'html "~/Public/dannyob.eth/now/narrative-arc.html")))
      nil 'local)
     (elisp-lint-indent-specs
      (describe . 1)
      (it . 1)
      (thread-first . 0)
      (cl-flet . 1)
      (cl-flet* . 1)
      (org-element-map . defun)
      (org-roam-dolist-with-progress . 2)
      (org-roam-with-temp-buffer . 1)
      (org-with-point-at . 1)
      (magit-insert-section . defun)
      (magit-section-case . 0)
      (org-roam-with-file . 2))
     (eval setq scheme-font-lock-keywords-2
      (cons
       '("(\\(define-json-mapping\\)"
         (1 font-lock-keyword-face))
       scheme-font-lock-keywords-2))
     (org-src-preserve-indentation)
     (eval require 'ox-texinfo+ nil t)
     (eval require 'ol-info)
     (elisp-lint-indent-specs
      (describe . 1)
      (it . 1)
      (org-element-map . defun)
      (org-roam-dolist-with-progress . 2)
      (org-roam-with-temp-buffer . 1)
      (org-with-point-at . 1)
      (magit-insert-section . defun)
      (magit-section-case . 0)
      (org-roam-with-file . 2))
     (elisp-lint-ignored-validators "byte-compile" "package-lint")
     (eval cl-flet
      ((enhance-imenu-lisp
        (&rest keywords)
        (dolist
            (keyword keywords)
          (add-to-list 'lisp-imenu-generic-expression
                       (list
                        (purecopy
                         (concat
                          (capitalize keyword)
                          (if
                              (string=
                               (substring-no-properties keyword -1)
                               "s")
                              "es" "s")))
                        (purecopy
                         (concat "^\\s-*("
                                 (regexp-opt
                                  (list
                                   (concat "define-" keyword))
                                  t)
                                 "\\s-+\\(" lisp-mode-symbol-regexp "\\)"))
                        2)))))
      (enhance-imenu-lisp "bookmarklet-command" "class" "command" "ffi-method" "function" "mode" "parenscript" "user-class"))
     (eval modify-syntax-entry 43 "'")
     (eval modify-syntax-entry 36 "'")
     (eval modify-syntax-entry 126 "'")
     (eval let
      ((root-dir-unexpanded
        (locate-dominating-file default-directory ".dir-locals.el")))
      (when root-dir-unexpanded
        (let*
            ((root-dir
              (expand-file-name root-dir-unexpanded))
             (root-dir*
              (directory-file-name root-dir)))
          (unless
              (boundp 'geiser-guile-load-path)
            (defvar geiser-guile-load-path 'nil))
          (make-local-variable 'geiser-guile-load-path)
          (require 'cl-lib)
          (cl-pushnew root-dir* geiser-guile-load-path :test #'string-equal))))
     (eval setq-local guix-directory
      (locate-dominating-file default-directory ".dir-locals.el"))
     (eval cl-flet
      ((enhance-imenu-lisp
        (&rest keywords)
        (dolist
            (keyword keywords)
          (add-to-list 'lisp-imenu-generic-expression
                       (list
                        (purecopy
                         (concat
                          (capitalize keyword)
                          (if
                              (string=
                               (substring-no-properties keyword -1)
                               "s")
                              "es" "s")))
                        (purecopy
                         (concat "^\\s-*("
                                 (regexp-opt
                                  (list
                                   (concat "define-" keyword))
                                  t)
                                 "\\s-+\\(" lisp-mode-symbol-regexp "\\)"))
                        2)))))
      (enhance-imenu-lisp "bookmarklet-command" "class" "command" "function" "mode" "parenscript" "user-class"))
     (no-ytbyte-compile . t)))
 '(send-mail-function 'mailclient-send-it)
 '(tracking-shorten-modes nil)
 '(warning-suppress-types
   '((org-element-cache)
     ((sly warning))
     ((sly warning))
     (nrepl-connected-hook))))
(put 'projectile-ag 'disabled nil)
(put 'projectile-grep 'disabled nil)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(mode-line ((t (:height 80))))
 '(mode-line-inactive ((t (:height 80)))))
