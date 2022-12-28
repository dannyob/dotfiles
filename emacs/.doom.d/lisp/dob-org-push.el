;;; dob-org-push.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Danny O'Brien
;;
;; Author: Danny O'Brien <danny@spesh.com>
;; Created: December 22, 2022
;; Version: 0.0.1
;; Keywords: lisp local
;; Homepage: https://github.com/danny/dotfiles/emacs/.doom.d/dob-org-push.el
;; Package-Requires: ((emacs "25.1") (org "9.6") (plz "0.2.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; A set of functions to push an org node to an external source, based on a
;; URI-like value in a "PUSH" property of the node. Current push values include:
;;
;;   https://[IGNORED FOR NOW].slack.com/[CHANNEL ID]#[MESSAGE TIMESSTAMP]
;;   https://www.notion.so/[NOTION-PAGE-URL]#[NAME OF SUBPAGE]
;;
;; Slack requires a token stored as `password' in the `authinfo' file, with the
;; `machine' field  `slack.com' and the `login' field set to `token`'.
;;
;; Notion requires a token stored as `password' in the `authinfo' file, with the
;; `machine' field  `notion.so' and the `login' field set to `token`'. You can
;; obtain its value by inspecting your browser cookies on a logged-in
;; (non-guest) session on Notion.so. (See https://github.com/jamalex/notion-py
;; and https://github.com/Cobertos/md2notion for more details.)
;;
;;;;; Code:

(require 'auth-source)
(require 'url-parse)
(require 'org)
(require 'org-ql)
(require 'plz)
(require 'doom)

(defun dop-do-push (push-uri)
  "Push current tree to destinarion defined by PUSH-URI."
  (let* ((parsed-sync-url (url-generic-parse-url push-uri))
         (sync-type (url-host parsed-sync-url))
         (element-metadata (org-element-headline-parser)))
    (org-narrow-to-element)
    (let ((new-uri (cond ((string-match "slack.com" sync-type) (dop-to-slack parsed-sync-url))
                         ((string-match "notion.so" sync-type) (dop-to-notion parsed-sync-url element-metadata)))))
      (widen)
      (message new-uri)
      new-uri)))

(defun dop-subtree ()
  "Iterate through each value of PUSH property in subtree."
  (apply #'org-entry-put-multivalued-property (point) "PUSH"
                                      (-map #'dop-do-push (org-entry-get-multivalued-property (point) "PUSH"))))

(defun dop-buffer ()
  "Push all nodes in current buffer that have PUSH property set."
  (interactive)
  (org-ql-select (current-buffer) '(property "PUSH")
    :action #'dop-subtree))

;; Slack implementation.
(defconst dop-slack-filter-filename
  (concat (or doom-user-dir (file-name-directory load-file-name))
          "lisp/dob-org-push-pandoc-slack-filter.lua")
  "Pandoc filter to convert Markdown to Slack mrkdwn.")

(defun dop--slack-secret-token ()
  "Obtain our slack token from authinfo."
  (funcall (plist-get (car (auth-source-search :host "slack.com" :user "token")) :secret)))

(defun dop-to-slack (url)
  "Push current tree to Slack.
Channel and optionally message timestamp is given by URL.
Returns new URL."
      (let ((slack-buf (generate-new-buffer "*slackbuf*")))
        (call-process-region (point-min) (point-max)
                             "pandoc" nil slack-buf nil "-f" "org" "-t" dop-slack-filter-filename)
        (let ((new-url (if (url-target url)
                           (dop-slack-update-message url slack-buf)
                         (dop-slack-post-message url slack-buf))))
          (kill-buffer slack-buf)
          new-url)))

(defun dop-slack-exec-with-url (url command payload)
  "Execute Slack API COMMAND with PAYLOAD and channel, ts from URL."
  (let* ((slack-channel (s-chop-suffix "/" (s-chop-prefix "/" (car (url-path-and-query url)))))
         (ts (url-target url))
         (response (if ts
                       (dop-slack-exec command (append `(("channel" . ,slack-channel)
                                                                  ("ts" . ,ts))
                                                                payload))
                     (dop-slack-exec command (append `(("channel" . ,slack-channel))
                                                     payload)))))
    response))

(defun dop-slack-exec (command payload)
  "Execute Slack API COMMAND with PAYLOAD."
  (let* ((addr (concat "https://slack.com/api/" command))
         (token (concat "Bearer " (dop--slack-secret-token)))
         (response (plz 'post addr
                     :headers `(("Content-Type" . "application/json; charset=utf-8")
                                ("Authorization" . ,token))
                     :body (json-encode payload)
                     :as #'json-read)))
    response))

(defun dop-slack-join-channel (url)
  "Joins a Slack channel, defined by URL.
Necessary before a bot can post to the channel (unless
you're using webhooks, which we are not.)"
  (dop-slack-exec-with-url url "conversations.join" '()))

(defun dop-slack-update-message (url slack-buf)
  "Change an existing message, pointed to by URL, to text in SLACK-BUF.
Requires that the URL include a message id in its fragment (or target) field."
  (dop-slack-join-channel url)
  (with-current-buffer slack-buf
    (dop-slack-exec-with-url url "chat.update" `(("text" . ,(buffer-string))))
    (url-recreate-url url)))

(defun dop-slack-post-message (url slack-buf)
  "Create a message in channel defined by URL, containing contents of SLACK-BUF."
  (dop-slack-join-channel url)
  (with-current-buffer slack-buf
    (let ((response (dop-slack-exec-with-url url "chat.postMessage" `(("text" . ,(buffer-string))))))
      (concat (url-recreate-url url) "#" (alist-get 'ts response)))))

(defun dop-slack-delete-message(url)
  "Delete Slack message pointed to by URL.
Returns an URL with channel but no ID."
  (dop-slack-exec-with-url url "chat.delete" '())
  (setf (url-target url) nil)
  (url-recreate-url url))

;; Notion implementation
(defun dop--notion-secret-token ()
  "Obtain our slack token from authinfo."
  (funcall (plist-get (car (auth-source-search :host "notion.so" :user "token")) :secret)))

(defun dop-to-notion (url metadata)
  "Push current tree to Notion. page is given by URL, name by METADATA's title."
      (let ((notion-buf (generate-new-buffer "*notion-buf*")))
        (call-process-region (point-min) (point-max)
                             "pandoc" nil notion-buf nil "-f" "org" "-t" "gfm-raw_html")
        (let ((new-uri (dop-notion-post-buffer url notion-buf metadata)))
      (kill-buffer notion-buf)
      new-uri)))

(defun dop--title-to-slug (metadata)
  "Extract a slug from the title of the parsed headline stored as METADATA."
  (replace-regexp-in-string "-+" "-"
                            (replace-regexp-in-string "[^A-Za-z0-9-]" "-"
                                                      (plist-get (cadr metadata) :raw-value))))

(defun dop-notion-post-buffer (url notion-buffer metadata)
  "Post NOTION-BUFFER contents to Notion page specified by URL."
  (with-current-buffer notion-buffer
    (let ((fname (concat temporary-file-directory (dop--title-to-slug metadata)))
          (new-url (cl-copy-seq url)))
      (write-region (point-min) (point-max) fname)
      (call-process "python" nil '(:file "/tmp/dump.log") nil "-m" "md2notion" "--clear-previous"
                    (dop--notion-secret-token) (url-recreate-url new-url) fname)))
  (url-recreate-url url))

(defun dop-notion-delete-url (url)
  "Delete Notion entry at URL."
  (message "Undefined"))

(provide 'dob-org-push)
;; (setf (alist-get "dob-org-push" package-lint--allowed-prefix-mappings) "dop")
;; Local Variables:
;; read-symbol-shorthands: (("dop-" . "dob-org-push-"))
;; End:
;;; dob-org-push.el ends here
