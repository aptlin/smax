(require-package 'gnus)
(setq mail-sources '((maildir :path "~/TMP/RSS" :subdirs ("cur" "new"))))
(setq gnus-select-method
      '(nnmbox "RSSFeeds"
               (directory "/home/aleph/TMP/RSS")
               (directory-files
                nnheader-directory-files-safe)
               (get-new-mail nil)))
(setq gnus-secondary-select-methods nil)
(setq gnus-message-archive-group "nnmaildir+mymailbox:outbox")

;; modified from Jordi Inglada

(defun ji-feed2imap ()
  (interactive)
  (start-process
   "feed2imap"
   "*feed2imap*"
   "/usr/bin/feed2imap" "-v"))


(defun get-feeds-gnus ()
  (interactive)
  (progn
    (ji-feed2imap)
    (gnus)))

(global-set-key (kbd "<f9> g") 'get-feeds-gnus)




(provide 'init-gnus)
