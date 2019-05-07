(setq org-journal-dir "~/Private/wiki/journal/")
(setq org-journal-file-format "%Y-%m-%d.org")
(setq org-ellipsis " ▼")
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/Private/org/todo.org" "Inbox")
         "* TODO %?\n  %i\n  %a")
        ("n" "Note" entry (file "~/Private/wiki/notes.org")
         "* %? %^g\nEntered on %U\n  %i\n  %a")))
(setq org-todo-keywords
      '((sequence "TODO" "WAITING" "|" "CANCELED" "DONE" "DELEGATED")))

(provide 'dob-org)
