;;From
;;http://beatofthegeek.com/2014/02/my-setup-for-using-emacs-as-web-browser.html

;;change default browser for 'browse-url' to w3m
(require-package 'w3m)
(setq browse-url-browser-function 'w3m-goto-url-new-session)
(setq w3m-default-display-inline-images t)
(defun wikipedia-search (search-term)
  "Search for SEARCH-TERM on wikipedia"
  (interactive
   (let ((term (if mark-active
                   (buffer-substring (region-beginning) (region-end))
                 (word-at-point))))
     (list
      (read-string
       (format "Wikipedia (%s):" term) nil nil term)))
   )
  (browse-url
   (concat
    "http://en.m.wikipedia.org/w/index.php?search="
    search-term
    ))
  )

;;when I want to enter the web address all by hand
(defun w3m-open-site (site)
  "Opens site in new w3m session with 'http://' appended"
  (interactive
   (list (read-string "Enter website address(default: w3m-home):" nil nil w3m-home-page nil )))
  (w3m-goto-url-new-session
   (concat "http://" site)))

(global-set-key (kbd "C-`") 'w3m)
(global-set-key (kbd "C-~") 'w3m-open-site)
(provide 'init-browser)
