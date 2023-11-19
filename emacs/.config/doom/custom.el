(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("e8567ee21a39c68dbf20e40d29a0f6c1c05681935a41e206f142ab83126153ca" "2ce76d65a813fae8cfee5c207f46f2a256bac69dacbb096051a7a8651aa252b0" "0c6a36393d5782839b88e4bf932f20155cb4321242ce75dc587b4f564cb63d90" "2f1518e906a8b60fac943d02ad415f1d8b3933a5a7f75e307e6e9a26ef5bf570" "79278310dd6cacf2d2f491063c4ab8b129fee2a498e4c25912ddaa6c3c5b621e" default))
 '(package-selected-packages '(aggressive-indent vterm debbugs))
 '(safe-local-variable-values
   '((org-src-preserve-indentation)
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
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(mode-line ((t (:height 80))))
 '(mode-line-inactive ((t (:height 80)))))
(put 'projectile-ag 'disabled nil)
(put 'projectile-grep 'disabled nil)