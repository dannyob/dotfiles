;;; packages.el --- dobextras layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Danny O'Brien <danny@eff.org>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `dobextras-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `dobextras/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `dobextras/pre-init-PACKAGE' and/or
;;   `dobextras/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst dobextras-packages
  '((org-habit-plus :location local)
    (dob-org :location local)
    (dob-notmuch :location local)
    (notmuch)))

(defun dobextras/init-org-habit-plus ()
 (use-package org-habit-plus))

(defun dobextras/init-dob-org ()
 (use-package dob-org))

(defun dobextras/init-dob-notmuch ()
  (use-package dob-notmuch))
    ;; :bind (("C-c n" . dob-notmuch-today)
    ;;        :map notmuch-search-mode-map
    ;;        ("C-c g" . notmuch-poll-refresh-this-buffer)
    ;;        ("S"     . dob-notmuch-spamify)
    ;;        :map notmuch-show-mode-map
    ;;        ("S"     . dob-notmuch-show-spamify-message-then-next-or-next-thread))))

(defun dobextras/post-init-notmuch ())

;;; packages.el ends here
