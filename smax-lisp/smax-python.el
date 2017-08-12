;;; smax-python.el ---

;;; Commentary:
;;
;;; Code:

;; * Python
;; **    Behaviour
(setq python-indent-offset 4)
;; This eliminates an annoying message about the interpreter not using
;; readline. That doesn't seem to matter at all.
(setq warning-suppress-types '((python)
                               (emacs)))
;; **    Outlines in python code
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
;; **    Elpy

(use-package elpy
  :init
  :bind (("M-." . elpy-doc)
         ("M-*" . elpy-goto-definition))
  :config
  (add-hook 'python-mode-hook 'elpy-enable)
  )

;; **    Keybindings
(define-key inferior-python-mode-map (kbd "C-c r")   'ipython-reset)
(define-key python-mode-map (kbd "C-c C-c") 'python-shell-send-defun)
(define-key python-mode-map (kbd "C-c C-l") 'python-shell-send-buffer)
(define-key python-mode-map (kbd "C-c h")   'mk-python-docs)

;; **    Highlight Indentation
(use-package highlight-indentation
  :ensure t
  :diminish highlight-indentation-mode
  :config (progn (add-hook 'python-mode-hook #'highlight-indentation-mode)))
;; * End
(provide 'smax-python)

;;; smax-python.el ends here

