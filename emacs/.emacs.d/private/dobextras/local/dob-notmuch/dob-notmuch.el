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
    (notmuch-search-next-thread))
  (defun dob-notmuch-show-spamify-message (&optional unspam)
    "Mark the current message as spam.

Spamify the current message by applying the tag changes in
`dob-spam-tags' to it. If a prefix argument is given, the
message will be \"unarchived\", i.e. the tag changes in
`dob-spam-tags' will be reversed."
    (interactive "P")
    (when dob-notmuch-spam-tags
      (apply 'notmuch-show-tag-message
             (notmuch-tag-change-list dob-notmuch-spam-tags unspam)))))

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

;; (defun dob--debug (x)
;;   (print (plist-get x :body)))

;; (defun dob-notmuch-extract-people ()
;;   "Use polyglot to extract the names of people from an email, and feed that as choices to dob-make-person"
;;   (interactive)
;;   (notmuch-show-with-message-as-text 'dob--debug))
    
;;          (call-shell-region (buffer-end 0) (buffer-end 1) "polyglot ner" nil "foo")))))


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

(provide 'dob-notmuch)
