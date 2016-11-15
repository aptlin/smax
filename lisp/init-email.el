(require 'mu4e)
(setq message-kill-buffer-on-exit t)
;; default
(setq mu4e-maildir "~/.mail/delly/")

(setq mu4e-drafts-folder "/drafts")
(setq mu4e-sent-folder   "/sent")
(setq mu4e-trash-folder  "/trash")

;; don't save message to Sent Messages, Gmail/IMAP takes care of this
;;(setq mu4e-sent-messages-behavior 'delete)

;; (See the documentation for `mu4e-sent-messages-behavior' if you have
;; additional non-Gmail addresses and want assign them different
;; behavior.)

;; setup some handy shortcuts
;; you can quickly switch to your Inbox -- press ``ji''
;; then, when you want archive some messages, move them to
;; the 'All Mail' folder by pressing ``ma''.

(setq mu4e-maildir-shortcuts
      '( ("/INBOX"               . ?i)
         ("/sent"   . ?s)
         ("/trash"       . ?t)
         ("/archive"    . ?a)))

;; allow for updating mail using 'U' in the main view:
(setq mu4e-get-mail-command "offlineimap")

;; something about ourselves
(setq
 user-mail-address "sasha.delly@gmail.com"
 user-full-name  "Alexander Illarionov"
 mu4e-compose-signature
 (concat
  "Sasha Illarionov\n"))
;; set up smtp manager
(setq message-send-mail-function 'message-send-mail-with-sendmail)
;;;use msmtp instead of sendmail
(setq sendmail-program "/usr/bin/msmtp")
;; http://pragmaticemacs.com/emacs/master-your-inbox-with-mu4e-and-org-mode/

;;store org-mode links to messages
(require 'org-mu4e)
;;store link to message if in header view, not to header query
(setq org-mu4e-link-query-in-headers-mode nil)
(setq mu4e-html2text-command "w3m -T text/html")
(provide 'init-email)
