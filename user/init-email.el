;; Thank you, Nan.
;; https://nine27.com/2016-10-03/better-email-with-mu4e/

;;(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")
(require 'mu4e)
(use-package mu4e
  :ensure nil
  :init
  (progn
    ;; setting up variables
    (setq mu4e-view-show-addresses t)
    (setq mu4e-headers-full-search t)
    (defun mu4e-message-maildir-matches (msg rx)
      (when rx
	(if (listp rx)
	    ;; If rx is a list, try each one for a match
	    (or (mu4e-message-maildir-matches msg (car rx))
		(mu4e-message-maildir-matches msg (cdr rx)))
	  ;; Not a list, check rx
	  (string-match rx (mu4e-message-field msg :maildir)))))

    ;; Choose account label to feed msmtp -a option based on From header
    ;; in Message buffer; This function must be added to
    ;; message-send-mail-hook for on-the-fly change of From address before
    ;; sending message since message-send-mail-hook is processed right
    ;; before sending message.
    (defun choose-msmtp-account ()
      (if (message-mail-p)
	  (save-excursion
	    (let*
		((from (save-restriction
			 (message-narrow-to-headers)
			 (message-fetch-field "from")))
		 (account
		  (cond
		   ((string-match "sasha.illarionov@mail.utoronto.ca" from) "ut")
		   ((string-match "sasha.delly@gmail.com" from) "main"))))
	      (setq message-sendmail-extra-arguments (list '"-a" account))))))

    (setq mail-user-agent 'mu4e-user-agent)
    (setq mu4e-mu-binary "/usr/bin/mu")
    (setq mu4e-maildir "~/.mail")
    (setq mu4e-get-mail-command "offlineimap -o")
    (setq mu4e-update-interval 3600)
    (setq mu4e-view-show-images t)
    (setq mu4e-html2text-command "w3m -dump -T text/html")
    ;; This enables unicode chars to be used for things like flags in the message index screens.
    ;; I've disabled it because the font I am using doesn't support this very well. With this
    ;; disabled, regular ascii characters are used instead.
                                        ;(setq mu4e-use-fancy-chars t)
    ;; This enabled the thread like viewing of email similar to gmail's UI.
    ;;(setq mu4e-headers-include-related nil)
    (setq mu4e-attachment-dir "~/TMP/MAIL")
    ;; This prevents saving the email to the Sent folder since gmail will do this for us on their end.
    (setq message-kill-buffer-on-exit t)
    ;; Enable inline images.
    (setq mu4e-view-show-images t)
    ;; Use imagemagick, if available.
    (when (fboundp 'imagemagick-register-types)
      (imagemagick-register-types))

    ;; Sometimes html email is just not readable in a text based client, this lets me open the
    ;; email in my browser.
    (add-to-list 'mu4e-view-actions '("View in browser" . mu4e-action-view-in-browser) t)

    ;; Spell checking ftw.
    (add-hook 'mu4e-compose-mode-hook 'flyspell-mode)
    (add-hook 'mu4e-compose-mode-hook (lambda ()
					(set-fill-column 40)))

    ;; This sets up my two different context for my personal and university emails.
    (setq mu4e-contexts
	  `( ,(make-mu4e-context
	       :name "delly"
	       :enter-func (lambda () (mu4e-message "Switch to the delly context"))
	       :match-func (lambda (msg)
			     (when msg
			       (mu4e-message-maildir-matches msg "^/delly")))
	       :leave-func (lambda () (mu4e-clear-caches))
	       :vars '((user-mail-address     . "sasha.delly@gmail.com")
		       (user-full-name        . "Alexander Illarionov")
		       (mu4e-sent-folder      . "/delly/sent")
		       (mu4e-drafts-folder    . "/delly/drafts")
		       (mu4e-trash-folder     . "/delly/trash")
		       (mu4e-refile-folder    . "/delly/archive")))
	     ,(make-mu4e-context
	       :name "ut"
	       :enter-func (lambda () (mu4e-message "Switch to the do context"))
	       :match-func (lambda (msg)
			     (when msg
			       (mu4e-message-maildir-matches msg "^/utoro")))
	       :leave-func (lambda () (mu4e-clear-caches))
	       :vars '((user-mail-address     . "sasha.illarionov@mail.utoronto.ca")
		       (user-full-name        . "Alexander Illarionov")
		       (mu4e-sent-folder      . "/delly/sent")
		       (mu4e-drafts-folder    . "/delly/drafts")
		       (mu4e-trash-folder     . "/delly/trash")
		       (mu4e-refile-folder    . "/delly/archive")))))

    ;; Configure sending mail.
    (setq message-send-mail-function 'message-send-mail-with-sendmail
	  sendmail-program "/usr/bin/msmtp"
	  user-full-name "Alexander Illarionov")

    ;; Use the correct account context when sending mail based on the from header.
    (setq message-sendmail-envelope-from 'header)
    (add-hook 'message-send-mail-hook 'choose-msmtp-account)

    ;; Bookmarks for common searches that I use.
    (setq mu4e-bookmarks '(("\\\\delly/INBOX" "Inbox" ?i)
			   ("flag:unread" "Unread messages" ?u)
			   ("date:today..now" "Today's messages" ?t)
			   ("date:7d..now" "Last 7 days" ?w)
			   ("mime:image/*" "Messages with images" ?p)))

    (setq mu4e-maildir-shortcuts
	  '( ("/delly/INBOX"               . ?i)
	     ("/delly/sent"   . ?s)
	     ("/delly/drafts"   . ?d)
	     ("/delly/trash"       . ?t)
	     ("/delly/archive"    . ?a)))

    ;; use dired
    ;; http://www.djcbsoftware.nl/code/mu/mu4e/Attaching-files-with-dired.html

    ;; make the `gnus-dired-mail-buffers' function also work on
    ;; message-mode derived modes, such as mu4e-compose-mode
    (defun gnus-dired-mail-buffers ()
      "Return a list of active message buffers."
      (let (buffers)
	(save-current-buffer
	  (dolist (buffer (buffer-list t))
	    (set-buffer buffer)
	    (when (and (derived-mode-p 'message-mode)
		       (null message-sent-message-via))
	      (push (buffer-name buffer) buffers))))
	(nreverse buffers)))

    (setq gnus-dired-mail-mode 'mu4e-user-agent)
    (add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)

    ;;  key bindings
    (global-unset-key (kbd "C-x m"))
    (global-set-key (kbd "C-x m") 'mu4e)))



(provide 'init-email)
