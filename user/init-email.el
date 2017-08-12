;; Thank you, Nan.
;; https://nine27.com/2016-10-03/better-email-with-mu4e/
;; * Helpers
(defun my-shell-command-to-string (&rest cmd)
  (replace-regexp-in-string "\r?\n$" ""
                            (shell-command-to-string (mapconcat 'identity cmd " "))))
;; * Mu4e
(when (f-exists? "/run/current-system/sw/share/emacs/site-lisp")
  (add-to-list 'load-path "/run/current-system/sw/share/emacs/site-lisp/"))
(if (executable-find "mu")
    (progn
      (setq mu4e-mu-binary (my-shell-command-to-string "which mu"))
      (use-package mu4e
        :load-path "/usr/share/emacs/site-lisp/mu4e"
        :ensure nil
        :init
        (require 'mu4e)
        (progn
          ;; setting up variables
          (setq mu4e-view-show-addresses t)
          (setq mu4e-headers-full-search t)
          ;; each paragraph is a single long line; at sending, emacs will add the
          ;; special line continuation characters.
          (setq mu4e-compose-format-flowed t)
          ;;  ISO(ish) format date-time stamps in the header list
          (setq mu4e-headers-date-format "%Y-%m-%d %H:%M")

          (defun mu4e-message-maildir-matches (msg rx)
            (when rx
              (if (listp rx)
                  ;; If rx is a list, try each one for a match
                  p		(or (mu4e-message-maildir-matches msg (car rx))
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
          (setq mu4e-maildir "~/.mail")
          (setq mu4e-get-mail-command "offlineimap -o")
          (setq mu4e-update-interval 180)
          (setq mu4e-view-show-images t)
          ;; use imagemagick, if available
          (when (fboundp 'imagemagick-register-types)
            (imagemagick-register-types))
          (setq mu4e-html2text-command #'mu4e-shr2text)
          ;; (setq mu4e-html2text-command "html2text -style pretty -width 72")
          ;; This enables unicode chars to be used for things like flags in the message index screens.
          ;; I've disabled it because the font I am using doesn't support this very well. With this
          ;; disabled, regular ascii characters are used instead.
                                        ;(setq mu4e-use-fancy-chars t)
          ;; This enabled the thread like viewing of email similar to gmail's UI.
          ;;(setq mu4e-headers-include-related nil)
          (setq mu4e-attachment-dir "~/tmp/mail")
          ;; This prevents saving the email to the Sent folder since gmail will do this for us on their end.
          (setq message-kill-buffer-on-exit t)
          ;; Enable inline images.
          (setq mu4e-view-show-images t)
          ;; Use imagemagick, if available.
          (when (fboundp 'imagemagick-register-types)
            (imagemagick-register-types))

          ;; Sometimes html email is just not readable in a text based client, this lets me open the
          ;; email in my browser.
          ;; Spell checking ftw.
          (add-hook 'mu4e-compose-mode-hook (lambda ()
                                              (set-fill-column 40)))

          ;; This sets up my two different context for my personal and university emails.
          (setq mu4e-contexts
                `( ,(make-mu4e-context
                     :name "sdll"
                     :enter-func (lambda () (mu4e-message "Switch to sdll"))
                     :match-func  (lambda (msg)
                                    (when msg
                                      (mu4e-message-contact-field-matches msg
                                                                          :to "sasha.delly@gmail.com")))
                     :leave-func (lambda () (mu4e-clear-caches))
                     :vars '((user-mail-address     . "sasha.delly@gmail.com")
                             (user-full-name        . "Alexander Illarionov")
                             (mu4e-sent-folder      . "/sdll/sent")
                             (mu4e-drafts-folder    . "/sdll/drafts")
                             (mu4e-trash-folder     . "/sdll/trash")
                             (mu4e-refile-folder    . "/sdll/archive")))
                   ,(make-mu4e-context
                     :name "ut"
                     :enter-func (lambda () (mu4e-message "Switch to the ut context"))
                     :match-func (lambda (msg)
                                   (when msg
                                     (mu4e-message-contact-field-matches msg
                                                                         :to "sasha.illarionov@mail.utoronto.ca")))
                     :leave-func (lambda () (mu4e-clear-caches))
                     :vars '((user-mail-address     . "sasha.illarionov@mail.utoronto.ca")
                             (user-full-name        . "Alexander Illarionov")
                             (mu4e-sent-folder      . "/sdll/sent")
                             (mu4e-drafts-folder    . "/sdll/drafts")
                             (mu4e-trash-folder     . "/sdll/trash")
                             (mu4e-refile-folder    . "/sdll/archive")))))

          ;; start with the first (default) context;
          (setq mu4e-context-policy 'pick-first)

          ;; compose with the current context if no context matches;
          (setq mu4e-compose-context-policy nil)

          ;; Configure sending mail.
          (setq message-send-mail-function 'message-send-mail-with-sendmail
                sendmail-program (my-shell-command-to-string "which msmtp")
                user-full-name "Alexander Illarionov")

          ;; Use the correct account context when sending mail based on the from header.
          (setq message-sendmail-envelope-from 'header)
          (add-hook 'message-send-mail-hook 'choose-msmtp-account)

          ;; Bookmarks for common searches that I use.
          (setq mu4e-bookmarks '(("\\\\sdll/INBOX" "Inbox" ?i)
                                 ("flag:unread" "Unread messages" ?u)
                                 ("date:today..now" "Today's messages" ?t)
                                 ("date:7d..now" "Last 7 days" ?w)
                                 ("mime:image/*" "Messages with images" ?p)))

          (setq mu4e-maildir-shortcuts
                '( ("/sdll/INBOX"               . ?i)
                   ("/sdll/sent"   . ?s)
                   ("/sdll/drafts"   . ?d)
                   ("/sdll/trash"       . ?t)
                   ("/sdll/archive"    . ?a)))

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
          (global-unset-key (kbd "<f2> m"))
          (global-set-key (kbd "<f2> m") 'mu4e))))
  (message "Cannot load mu4e: mu is required to proceed!"))
;; * Make search more responsive
(when ( require 'mu4e nil 'noerror) (use-package helm-mu
                                      :ensure t
                                      :config (progn
                                                (bind-key "S" #'helm-mu mu4e-main-mode-map))))

;; * Make naming homogeneous
(when ( require 'mu4e nil 'noerror)
  (setq malb/mu4e-name-replacements '(("" . "")))
  (defun malb/canonicalise-contact-name (name)
    (let ((case-fold-search nil))
      (setq name (or name ""))
      (if (string-match-p "^[^ ]+@[^ ]+\.[^ ]" name)
          ""
        (progn
          ;; drop email address
          (setq name (replace-regexp-in-string "^\\(.*\\) [^ ]+@[^ ]+\.[^ ]" "\\1" name))
          ;; strip quotes
          (setq name (replace-regexp-in-string "^\"\\(.*\\)\"" "\\1" name))
          ;; deal with YELL’d last names
          (setq name (replace-regexp-in-string "^\\(\\<[[:upper:]]+\\>\\) \\(.*\\)" "\\2 \\1" name))
          ;; Foo, Bar becomes Bar Foo
          (setq name (replace-regexp-in-string "^\\(.*\\), \\([^ ]+\\).*" "\\2 \\1" name))
          ;; look up names and replace from static table, TODO look this up by email
          (setq name (or (cdr (assoc name malb/mu4e-name-replacements)) name))
          ))))

  (defun malb/mu4e-contact-rewrite-function (contact)
    (let* ((name (or (plist-get contact :name) ""))
           (mail (plist-get contact :mail))
           (case-fold-search nil))
      (plist-put contact :name (malb/canonicalise-contact-name name))
      contact))

  (setq mu4e-contact-rewrite-function #'malb/mu4e-contact-rewrite-function))


;; * Add Notifications
(when ( require 'mu4e nil 'noerror)
  (use-package mu4e-alert
    :init
    :config
    (mu4e-alert-set-default-style 'libnotify)
    (add-hook 'after-init-hook #'mu4e-alert-enable-notifications)))
;; * Automate Replies
(when ( require 'mu4e nil 'noerror)
  (defun malb/yas-get-names-from-fields (fields)
    (let (names
          ret
          name
          point-end-of-line
          (search-regexp (mapconcat (lambda (arg)
                                      (concat "^" arg ": "))
                                    fields "\\|"))
          (case-fold-search nil))
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward search-regexp nil t)
          (save-excursion
            (setq point-end-of-line (re-search-forward "$")))
          (setq name (buffer-substring-no-properties (point) point-end-of-line))
          (setq name (split-string name "[^ ]+@[^ ]+," t " ")) ;; split on email@address,
          (setq names (append names name)))
        (dolist (name names)
          (setq name (malb/canonicalise-contact-name name))
          (if (string-match "\\([^ ,]+\\)" name)
              (progn
                (setq name (match-string 1 name))
                (setq name (capitalize name))
                (if ret
                    (setq ret (concat ret ", " name))
                  (setq ret name)))))
        (if ret ret ""))))

  (defun malb/yas-get-names-from-to-fields ()
    (interactive)
    (malb/yas-get-names-from-fields '("To"))))

;; * Make writing more polished
(add-hook 'message-mode-hook #'typo-mode)
(add-hook 'message-mode-hook #'footnote-mode)
(provide 'init-email)
