(setq org-journal-dir "~/Private/wiki/journal/")
(setq org-journal-file-format "%Y-%m-%d.org")
(setq org-ellipsis " ▼")
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/Private/org/todo.org" "Inbox")
         "* TODO %?\n  %i\n  %a")
        ("n" "Note" entry (file "~/Private/wiki/notes.org")
         "* %? %^g\nEntered on %U\n  %i\n  %a")))
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
