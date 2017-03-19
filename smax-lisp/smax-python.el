;;; smax-python.el ---

;;; Commentary:
;;
;;; Code:

;; * Python
;; ** Behaviour
(setq-default
 python-fill-docstring-style 'django
 python-indent-offset        4)
;; This eliminates an annoying message about the interpreter not using
;; readline. That doesn't seem to matter at all.
(setq warning-suppress-types '((python)
			       (emacs)))
;; ** Interpreter
(when (executable-find "ipython")
  (setq
   python-shell-interpreter          "ipython"
   python-shell-prompt-output-regexp "Out\\[[0-9 +]\\]: "
   python-shell-prompt-regexp        "In \\[[0-9]+\\]: "))
(defun python-shell-ensure-proc (&rest _rest)
  "Make sure that python process is running for current buffer."
  (unless (python-shell-get-process)
    (let ((win (get-buffer-window)))
      (run-python nil nil t)
      (select-window win))))
(defun ipython-reset ()
  "Reset iPython shell."
  (interactive)
  (comint-send-string (python-shell-get-process)
                      "%reset\ny\n"))
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



;; ** Keybindings
(τ python inferior-python "C-c r"   #'ipython-reset)
(τ python python          "C-c C-c" #'python-shell-send-defun)
(τ python python          "C-c C-l" #'python-shell-send-buffer)
(τ python python          "C-c h"   #'mk-python-docs)
;; ** Advice
(advice-add 'python-shell-send-buffer :before #'python-shell-ensure-proc)
(advice-add 'python-shell-send-defun  :before #'python-shell-ensure-proc)
(advice-add 'run-python               :after  (η #'python-shell-switch-to-shell))

;; * End
(provide 'smax-python)

;;; smax-python.el ends here
