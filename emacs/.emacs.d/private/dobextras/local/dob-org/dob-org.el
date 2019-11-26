(setq org-journal-dir "~/Private/wiki/journal/")
(setq org-journal-file-type 'weekly)
(setq org-journal-file-format "%Y-%m-%d.org")
(defun org-journal-date-format-func (time)
  "Custom function to insert journal date header.

  When buffer is empty prepend a header in front the entry header."
  (concat (when (= (buffer-size) 0)
            (concat
             (pcase org-journal-file-type
               (`daily "#+TITLE: Daily Journal\n")
               (`weekly "#+TITLE: Weekly Journal\n")
               (`monthly "#+TITLE: Monthly Journal\n")
               (`yearly "#+TITLE: Yearly Journal\n"))))
          org-journal-date-prefix
          (format-time-string "%A, %F" time)))
(setq org-journal-date-format 'org-journal-date-format-func)
(add-to-list 'org-agenda-files org-journal-dir)

(defun org-journal-find-location ()
  ;; Open today's journal, but specify a non-nil prefix argument in order to
  ;; inhibit inserting the heading; org-capture will insert the heading.
  (org-journal-new-entry t)
  ;; Position point on the journal's top-level heading so that org-capture
  ;; will add the new entry as a child entry.
  (goto-char (point-min)))

(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/Private/org/todo.org" "Inbox")
         "* TODO %?\n  %i\n  %a")
        ("n" "Note" entry (file "~/Private/wiki/notes.org")
         "* %? %^g\nEntered on %U\n  %i\n  %a")
        ("j" "Journal entry" entry (function
                                    org-journal-find-location)
         "* %(format-time-string org-journal-time-format)%^{Title}\n%i%?")))

(if (string-equal "yacht" (getenv "SHORTHOST"))
    (setq org-capture-templates
          '(("t" "Todo" entry (file+headline "~/Private/org/yacht.org" "Inbox")
             "* TODO %?\n  %i\n  %a")
            ("n" "Note" entry (file "~/Private/wiki/notes.org")
             "* %? %^g\nEntered on %U\n  %i\n  %a"))))
  
(setq org-todo-keywords
      '((sequence "TODO" "WAITING" "|" "CANCELED" "DONE" "DELEGATED")))

(setq org-startup-indented t
      org-bullets-bullet-list '(" ")
      org-ellipsis "  "
      org-pretty-entities t
      org-hide-emphasis-markers t
      org-agenda-block-separator ""
      org-fontify-whole-heading-line t
      org-fontify-done-headline t
      org-fontify-quote-and-verse-blocks t)

(provide 'dob-org)
