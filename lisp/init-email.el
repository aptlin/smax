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

;; accounts
(setq
 user-mail-address "sasha.delly@gmail.com"
 user-full-name  "Alexander Illarionov"
 mu4e-compose-signature
 (concat
  "sasha\n"))

(defvar my-mu4e-account-alist
  '(("delly"
     (setq mu4e-drafts-folder "/drafts")
     (setq mu4e-sent-folder   "/sent")
     (user-mail-address "sasha.delly@gmail.com"))
    ("utoro"
     (setq mu4e-drafts-folder "/drafts")
     (setq mu4e-sent-folder   "/sent")
     (user-mail-address "sasha.illarionov@mail.utoronto.ca"))))
(add-hook 'mu4e-compose-pre-hook 'my-mu4e-set-account)

;; set up smtp manager
(setq message-send-mail-function 'message-send-mail-with-sendmail)
;;;use msmtp instead of sendmail
(setq sendmail-program "/usr/bin/msmtp")
;; tell msmtp to choose the SMTP server according to the from field in
;; the outgoing email
(setq message-sendmail-extra-arguments '("--read-envelope-from"))
(setq message-sendmail-f-is-evil 't)
;;https://www.reddit.com/r/emacs/comments/3r8dr3/mu4e_send_mail_with_custom_smtp_and_archive_in/
(defun my-mu4e-set-account ()
  "Set the account for composing a message."
  (let* ((accounts (mapcar #'car my-mu4e-account-alist))
         (account
          (or (and mu4e-compose-parent-message
                   (let ((maildir (mu4e-message-field mu4e-compose-parent-message :maildir)))
                     (string-match "/\\(.*?\\)/" maildir)
                     (car (member (match-string 1 maildir) accounts))))
              (completing-read (format "Compose with account: (%s) "
                                       (mapconcat #'identity accounts "/"))
                               accounts
                               nil t nil nil (car accounts))))
         (account-vars (cdr (assoc account my-mu4e-account-alist))))
    (if account-vars
        (mapc #'(lambda (var)
                  (set (car var) (cadr var)))
              account-vars)
      (error "No email account found"))))

;; http://pragmaticemacs.com/emacs/master-your-inbox-with-mu4e-and-org-mode/

;;store org-mode links to messages
(require 'org-mu4e)
;;store link to message if in header view, not to header query
(setq org-mu4e-link-query-in-headers-mode nil)
(setq mu4e-html2text-command "w3m -T text/html")

;; show counts
(require-package 'mu4e-maildirs-extension)
(mu4e-maildirs-extension)
(provide 'init-email)
