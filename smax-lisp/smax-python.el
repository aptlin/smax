;;; smax-python.el ---

;;; Commentary:
;;
;;; Code:

;; * Python
;; ** Behaviour
(setq python-indent-offset 4)
;; This eliminates an annoying message about the interpreter not using
;; readline. That doesn't seem to matter at all.
(setq warning-suppress-types '((python)
                               (emacs)))
;; ** Search

(add-to-list 'mk-search-prefix '(inferior-python-mode . "python"))
(add-to-list 'mk-search-prefix '(python-mode          . "python"))

(defun mk-python-docs (symbol)
  "Find documentation for given symbol SYMBOL online."
  (interactive (list (mk-grab-input "Python Docs: ")))
  (browse-url
   (concat "https://docs.python.org/3/search.html?q="
           (url-hexify-string symbol)
           "&check_keywords=yes&area=default")))


;; ** Outlines in python code
(defconst lpy-font-lock-keywords
  '(("^# \\(\\* .*\\)$" 1 'org-level-1 prepend)
    ("^# \\(\\*\\* .*\\)$" 1 'org-level-2 prepend)
    ("^# \\(\\*\\*\\* .*\\)$" 1 'org-level-3 prepend)
    ("^# \\(\\*\\*\\*\\* .*\\)$" 1 'org-level-4 prepend)
    ("^# \\(\\*\\*\\*\\*\\* .*\\)$" 1 'org-level-5 prepend)
    (lpy-outline-comment-highlight 1 'default prepend)
    ("`\\([^\n']+\\)'" 1 font-lock-constant-face prepend)))

(font-lock-add-keywords 'python-mode lpy-font-lock-keywords)

(defun lpy-outline-comment-highlight (limit)
  (while (re-search-forward "^# \\(?:[^*]\\|$\\)" limit t)
    (let* ((pt (point))
	   (success (save-excursion
		      (and (re-search-backward "^# \\*" nil t)
			   (null (re-search-forward "^[^#]" pt t))))))
      (when success
	(set-match-data (list (line-beginning-position) (line-end-position)
			      (point) (line-end-position)))
	(end-of-line)
	t))))

(add-hook 'python-mode-hook
          (lambda ()
            (setq outline-regexp "# \\*+"
                  orgstruct-heading-prefix-regexp "# ?\\*+\\|\\`")
            (orgstruct-mode)
            (org-global-cycle 3)))
;; ** Autocompletion
(use-package company-anaconda
  :ensure t)



;; ** Keybindings
(define-key inferior-python-mode-map (kbd "C-c r")   'ipython-reset)
(define-key python-mode-map (kbd "C-c C-c") 'python-shell-send-defun)
(define-key python-mode-map (kbd "C-c C-l") 'python-shell-send-buffer)
(define-key python-mode-map (kbd "C-c h")   'mk-python-docs)

;; * End
(provide 'smax-python)

;;; smax-python.el ends here
