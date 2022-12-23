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
;;   slack://[IGNORED FOR NOW]/[CHANNEL ID]/[MESSAGE TIMESSTAMP]
;;
;; If you add a "PUSH-DELETE" property, the slack backend will delete the
;; message, and reset the message timestamp.
;;
;;; Code:

(require 'auth-source)
(require 'url-parse)
(require 'org)
(require 'org-ql)
(require 'plz)

(defun dop--slack-secret-token ()
  "Obtain our slack token from authinfo."
  (funcall (plist-get (car (auth-source-search :host "slack.com" :user "token")) :secret)))

(defun dop-subtree ()
  "Push current tree based on PUSH property URI."
  (let* ((sync-dest (cdr (assoc "PUSH" (org-entry-properties))))
         (parsed-sync-url (url-generic-parse-url sync-dest))
         (sync-type (url-type parsed-sync-url)))
    (cond ((string-equal "slack" sync-type) (dop-to-slack parsed-sync-url)))))

(defun dop-to-slack (url)
  "Push current tree to Slack.
Channel and optionally message timestamp is given by URL."
  (if (assoc "PUSH-DELETE" (org-entry-properties))
      (dop-slack-delete-message url)
      (org-narrow-to-element)
      (let ((slack-buf (generate-new-buffer "*slackbuf*")))
        (call-process-region (point-min) (point-max)
                             "pandoc" nil slack-buf nil "-f" "org" "-t" "/home/danny/tmp/slack.lua")
        (widen)
        (org-set-property "PUSH" (if (url-target url)
                                     (dop-slack-update-message url slack-buf)
                                   (dop-slack-post-message url slack-buf)))
      (kill-buffer slack-buf))))

(defun dop-slack-delete-message (url)
  "Delete Slack message pointed to by URL.
Returns an URL with channel but no ID."
  (let* ((slack-channel (s-chop-suffix "/" (s-chop-prefix "/" (car (url-path-and-query url)))))
         (addr "https://slack.com/api/chat.delete")
         (token (concat "Bearer " (dop--slack-secret-token)))
         (ts (url-target url))
         (payload `(("channel" . ,slack-channel)
                    ("ts" . ,ts)))
         (response (plz 'post addr
                     :headers `(("Content-Type" . "application/json; charset=utf-8")
                                ("Authorization" . ,token))
                     :body (json-encode payload)
                     :as #'json-read)))
    (message (json-encode response))
    (setf (url-target url) nil)
    (url-recreate-url url)))

(defun dop-slack-exec-with-url (url command payload)
  "Execute Slack API COMMAND with PAYLOAD and channel, ts from URL."
  (let* ((slack-channel (s-chop-suffix "/" (s-chop-prefix "/" (car (url-path-and-query url)))))
         (ts (url-target url))
         (response (if ts
                       (dop-slack-exec command (append `(("channel" . ,slack-channel)
                                                                  ("ts" . ,ts))
                                                                payload))
                     (dop-slack-exec command (append `(("channel" . ,slack-channel)))))))
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
  (let* ((slack-channel (s-chop-suffix "/" (s-chop-prefix "/" (car (url-path-and-query url))))  )
         (addr "https://slack.com/api/conversations.join")
         (token (concat "Bearer " (dop--slack-secret-token)))
         (payload `(("channel" . ,slack-channel)))
         (response (plz 'post addr
                     :headers `(("Content-Type" . "application/json; charset=utf-8")
                                ("Authorization" . ,token))
                     :body (json-encode payload)
                     :as #'json-read)))
    (message (json-encode response))
    response))

(defun dop-slack-update-message (url slack-buf)
  "Change an existing message, pointed to by URL, to text in SLACK-BUF.
Requires that the URL include a message id in its fragment (or target) field."
  (dop-slack-join-channel url)
  (with-current-buffer slack-buf
    (let* ((slack-channel (s-chop-suffix "/" (s-chop-prefix "/" (car (url-path-and-query url))))  )
           (addr "https://slack.com/api/chat.update")
           (token (concat "Bearer " (dop--slack-secret-token)))
           (payload `(("channel" . ,slack-channel)
                      ("ts" . ,(url-target url))
                      ("text" . ,(buffer-string))))
           (response (plz 'post addr
                       :headers `(("Content-Type" . "application/json; charset=utf-8")
                                  ("Authorization" . ,token))
                       :body (json-encode payload)
                       :as #'json-read)))
      (message (json-encode response))
      (url-recreate-url url))))

(defun dop-slack-post-message (url slack-buf)
  "Create a message in channel defined by URL, containing contents of SLACK-BUF."
  (dop-slack-join-channel url)
  (with-current-buffer slack-buf
    (let* ((slack-channel (s-chop-suffix "/" (s-chop-prefix "/" (car (url-path-and-query url))))  )
           (addr "https://slack.com/api/chat.postMessage")
           (token (concat "Bearer " (dop--slack-secret-token)))
           (payload `(("channel" . ,slack-channel)
                      ("text" . ,(buffer-string))))
           (response (plz 'post addr
                       :headers `(("Content-Type" . "application/json; charset=utf-8")
                                  ("Authorization" . ,token))
                       :body (json-encode payload)
                       :as #'json-read)))
      (message (json-encode response))
      (concat (url-recreate-url url) "#" (alist-get 'ts response)))))

(defun dop-buffer ()
  "Push all nodes in current buffer that have PUSH property set."
  (interactive)
  (org-ql-select (current-buffer) '(property "PUSH")
    :action #'dop-subtree))


(provide 'dob-org-push)
;; (setf (alist-get "dob-org-push" package-lint--allowed-prefix-mappings) "dop")
;; Local Variables:
;; read-symbol-shorthands: (("dop-" . "dob-org-push-"))
;; End:
;;; dob-org-push.el ends here
