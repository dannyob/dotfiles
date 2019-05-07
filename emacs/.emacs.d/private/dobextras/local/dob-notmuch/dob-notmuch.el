;; (require 'notmuch)
(defvar dob-notmuch-spam-tags '("-inbox" "+spam" "+missedspam"))

(defun dob-notmuch-today ()
  "Show just today's inbox"
  (interactive)
  (notmuch-search "tag:inbox AND ( date:today OR date:yesterday )"))

(defun dob-notmuch-spamify
    (&optional unspam beg end)
  "Mark as spam the currently selected thread or region.

     Archive each message in the currently selected thread by applying
     the tag changes in `dob-notmuch-spam-tags' to each (remove the
         \"inbox\" tag by default). If a prefix argument is given, the
     messages will be \"unspammed\" (i.e. the tag changes in
     `dob-notmuch-spam-tags' will be reversed).

     This function advances the next thread when finished."
  (interactive (cons current-prefix-arg (notmuch-search-interactive-region)))
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

(provide 'dob-notmuch)
