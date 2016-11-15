;;From
;;http://beatofthegeek.com/2014/02/my-setup-for-using-emacs-as-web-browser.html

;;change default browser for 'browse-url' to w3m
(require-package 'w3m)
(setq browse-url-browser-function 'w3m-goto-url-new-session)
;; show images
(setq w3m-default-display-inline-images t)
;; search wiki
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

;; open a website
(defun w3m-open-site (site)
  "Opens site in new w3m session with 'http://' appended"
  (interactive
   (list (read-string "Enter website address(default: w3m-home):" nil nil w3m-home-page nil )))
  (w3m-goto-url-new-session
   (concat "http://" site)))

(setq w3m-default-save-directory "~/TMP/WWW/")

(setq w3m-home-page "https://sdll.github.io/agenda")

;; Change tabs easily
(when (require 'w3m)
  (define-key w3m-mode-map (kbd "M-<right>") 'w3m-next-buffer)
  (define-key w3m-mode-map (kbd "M-<left>") 'w3m-previous-buffer)
  )
;; enable-cookies-in-w3m
(setq w3m-use-cookies t)

;; w3m-antenna
(autoload 'w3m-antenna "w3m-antenna" "Report changes of WEB sites." t)

;; Remove-trailing-white-space-in-w3m-buffers
(add-hook 'w3m-display-hook
          #'(lambda (url)
              (let ((buffer-read-only nil))
                (delete-trailing-whitespace))))

;;;;

;; keybindings
(global-set-key (kbd "C-`") 'w3m)
(global-set-key (kbd "C-~") 'w3m-open-site)


(provide 'init-browser)
