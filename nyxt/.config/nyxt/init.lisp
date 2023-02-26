(load "~/.local/share/quicklisp/setup.lisp")
(in-package :nyxt-user)

(defun eval-in-emacs (&rest s-exps)
  "Evaluate S-exps with `emacsclient'."
  (let ((s-exps-string (cl-ppcre:regex-replace-all
                        ;; Discard the package prefix.
                        "next-user::?"
                        (write-to-string
                         `(progn ,@s-exps) :case :downcase)
                        "")))
    (log:debug "Sending to Emacs: ~s" s-exps-string)
    (ignore-errors (uiop:run-program
                    (list "emacsclient" "--eval" s-exps-string)))))

(defun foo () (log:debug "Bleh"))

(defvar *my-search-engines*
  (list
   '("g" "https://www.google.com/?q=~a")
   '("b" "https://search.brave.com/search?q=~a")))

(define-configuration buffer
  ((search-engines (append %slot-default%
                           (mapcar (lambda (engine) (apply 'make-search-engine engine))
                                   *my-search-engines*)))))

(define-command org-capture (&optional (buffer (current-buffer)))
  "Org-capture current page."
  (eval-in-emacs
   `(org-link-set-parameters
     "next"
     :store (lambda ()
              (org-store-link-props
               :type "next"
               :link ,(url buffer)
               :description ,(title buffer))))
   `(org-capture)))
;(define-key *my-keymap* "C-M-o" 'org-capture)

(define-command youtube-dl-current-page (&optional (buffer (current-buffer)))
  "Download a video in the currently open buffer."
  (eval-in-emacs
   (if (search "youtu" (url buffer))
       `(progn (youtube-dl ,(url buffer)) (youtube-dl-list))
       `(ambrevar/youtube-dl-url ,(url buffer)))))
;(define-key *my-keymap* "C-M-c d" 'youtube-dl-current-page)

(define-command play-video-in-current-page (&optional (buffer (current-buffer)))
  "Play video in the currently open buffer."
  (uiop:run-program (list "mpv" (url buffer))))
;(define-key *my-keymap* "C-M-c v" 'play-video-in-current-page)

(defvar +youtube-dl-command+ "youtube-dl"
  "/home/danny/.local/bin/yt-dlp")

(define-configuration base-mode
  ((keymap-scheme
    (define-scheme (:name-prefix "my-base" :import %slot-default%)
      scheme:vi-normal
      (list "g b" (make-command switch-buffer* ()
                    (switch-buffer :current-is-last-p t)))))))

(define-configuration buffer
  ((override-map
    (let ((map (make-keymap "override-map")))
                   (define-key map
                     "C-w" 'delete-current-buffer)))))

(define-configuration browser
  ((session-restore-prompt :always-restore)))

(load-after-system :slynk (nyxt-init-file "slynk.lisp"))
