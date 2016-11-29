;;From
;;http://beatofthegeek.com/2014/02/my-setup-for-using-emacs-as-web-browser.html

;;change default browser for 'browse-url' to w3m
(require-package 'w3m)
(require 'mime-w3m)
(setq browse-url-browser-function 'w3m-goto-url-new-session)

;; images
(setq w3m-default-display-inline-images nil)

;; set encoding
;;https://www.emacswiki.org/emacs/emacs-w3m#searchlang

(setq w3m-coding-system 'utf-8
      w3m-file-coding-system 'utf-8
      w3m-file-name-coding-system 'utf-8
      w3m-input-coding-system 'utf-8
      w3m-output-coding-system 'utf-8
      w3m-terminal-coding-system 'utf-8)

;; search google
;; thank you, Howard
;; https://raw.githubusercontent.com/howardabrams/dot-files/master/emacs-browser.org

(require-package 'ace-link)
(ace-link-setup-default)
(define-key w3m-mode-map (kbd "o") 'ace-link-eww)
;; clean up the w3m buffers:
(add-hook 'w3m-display-functions 'w3m-hide-stuff)
(add-hook 'w3m-mode 'ace-link-mode)

(defun w3m-skip-in-google ()
  "For a Google Search, skip to the first result."
  (beginning-of-buffer)
  (search-forward-regexp "[0-9, ]+ results")
  (forward-line 2)
  (recenter-top-bottom 0))

(defun w3m-skip-in-stackoverflow ()
  (beginning-of-buffer)
  (search-forward-regexp "^   ")
  (forward-line -2)
  (recenter-top-bottom 0))

(defun w3m-skip-in-clojuredocs()
  "When viewing the Clojuredocs, we can skip to the meat of the
     function description by looking for the label, ‘Available since’,
     and finding the function name just before that."
  (beginning-of-buffer)
  (search-forward-regexp "Available since")
  (forward-line -4)
  (recenter-top-bottom 0))

(defun w3m-hide-stuff (url)
  "Call screen cleaning functions for the W3M based on the URL."
  (interactive)
  (cond ((string-match "google\.com/search" url) (w3m-skip-in-google))
        ((string-match "clojuredocs.org" url) (w3m-skip-in-clojuredocs))
        ((string-match "stackoverflow.com" url) (w3m-skip-in-stackoverflow))
        ))


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
(eval-after-load "w3m"
                 '(setcdr (assoc "application/pdf" w3m-content-type-alist)
                   `("\\.pdf\\'" (,(or
                                    (executable-find "zathura")
                                    (executable-find "evince")
                                    ) file) nil)))
;; keybindings
(global-set-key (kbd "C-`") 'w3m)
(global-set-key (kbd "C-~") 'w3m-open-site)

;; orgmode integration
;; https://www.emacswiki.org/emacs/emacs-w3m#searchlang

(defun w3m-get-buffer-with-org-style ()
  "Get current buffer content with `org-mode' style.
This function will encode `link-title' and `link-location' with `org-make-link-string'.
And move buffer content to lastest of kill ring.
So you can yank in `org-mode' buffer to get `org-mode' style content."
  (interactive)
  (let (transform-start
        transform-end
        return-content
        link-location
        link-title
        temp-position
        out-bound)
    (if mark-active
        (progn
          (setq transform-start (region-beginning))
          (setq transform-end (region-end))
          (deactivate-mark))
      (setq transform-start (point-min))
      (setq transform-end (point-max)))
    (message "Start transform link to `org-mode' style, please wait...")
    (save-excursion
      (goto-char transform-start)
      (while (and (not out-bound)       ;not out of transform bound
                  (not (w3m-no-next-link-p))) ;not have next link in current buffer
        ;; store current point before jump next anchor
        (setq temp-position (point))
        ;; move to next anchor when current point is not at anchor
        (or (w3m-anchor (point)) (w3m-get-next-link-start))
        (if (<= (point) transform-end) ;if current point is not out of transform bound
            (progn
              ;; get content between two links.
              (if (> (point) temp-position)
                  (setq return-content (concat return-content (buffer-substring temp-position (point)))))
              ;; get link location at current point.
              (setq link-location (w3m-anchor (point)))
              ;; get link title at current point.
              (setq link-title (buffer-substring (point) (w3m-get-anchor-end)))
              ;; concat `org-mode' style url to `return-content'.
              (setq return-content (concat return-content (org-make-link-string link-location link-title))))
          (goto-char temp-position) ;reset point before jump next anchor
          (setq out-bound t)        ;for break out `while' loop
          ))
      ;; concat rest context of current buffer
      (if (< (point) transform-end)
          (setq return-content (concat return-content (buffer-substring (point) transform-end))))
      (kill-new return-content)
      (message "Transform link completed. You can get it from lastest kill ring."))))

(defun w3m-get-anchor-start ()
  "Move and return `point' for thst start of the current anchor."
  (interactive)
  (goto-char (or (previous-single-property-change (point) 'w3m-anchor-sequence) ;get start position of anchor
                 (point)))                                                      ;or current point
  (point))

(defun w3m-get-anchor-end ()
  "Move and return `point' after the end of current anchor."
  (interactive)
  (goto-char (or (next-single-property-change (point) 'w3m-anchor-sequence) ;get end position of anchor
                 (point)))                                                  ;or current point
  (point))

(defun w3m-get-next-link-start ()
  "Move and return `point' for that start of the current link."
  (interactive)
  (catch 'reach
    (while (next-single-property-change (point) 'w3m-anchor-sequence) ;jump to next anchor
      (goto-char (next-single-property-change (point) 'w3m-anchor-sequence))
      (when (w3m-anchor (point))        ;return point when current is valid link
        (throw 'reach nil))))
  (point))

(defun w3m-get-prev-link-start ()
  "Move and return `point' for that end of the current link."
  (interactive)
  (catch 'reach
    (while (previous-single-property-change (point) 'w3m-anchor-sequence) ;jump to previous anchor
      (goto-char (previous-single-property-change (point) 'w3m-anchor-sequence))
      (when (w3m-anchor (point))        ;return point when current is valid link
        (throw 'reach nil))))
  (point))

(defun w3m-no-next-link-p ()
  "Return t if no next link after cursor.
Otherwise, return nil."
  (save-excursion
    (equal (point) (w3m-get-next-link-start))))

(defun w3m-no-prev-link-p ()
  "Return t if no previous link after cursor.
Otherwise, return nil."
  (save-excursion
    (equal (point) (w3m-get-prev-link-start))))
(define-key w3m-minor-mode-map (kbd "C-x M-w") 'w3m-get-buffer-with-org-style)


;; make browse-url-url-at-point use w3m links if they exist
(defadvice browse-url-url-at-point (after w3m-anchor-at-point activate)
  "Browse the url at point. If w3m-anchor finds a url, use it."
  (setq ad-return-value
        (or
         (w3m-anchor)
         (ad-return-value))))

;;

(setq w3m-confirm-leaving-secure-page nil)

(provide 'init-browser)
