;;* Editing
;; ** Helpers
(defun prettify-paragraph ()
  (interactive)
  (align-current)
  (fill-paragraph)
  )
(global-set-key (kbd "<S-return>") 'prettify-paragraph)

(defun rename-this-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file!" name))
    (progn
      (when (file-exists-p filename)
        (rename-file filename new-name 1))
      (set-visited-file-name new-name)
      (rename-buffer new-name))))

;; ** Packages
(subword-mode 1) 
(use-package expand-region
  :ensure t
  :init
  :bind (
	 ("C-@" . er/expand-region)))

(use-package multiple-cursors
  :ensure t
  :init)
;; multiple-cursors
(global-set-key (kbd "C-c m p") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c m n") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c m t") 'mc/mark-all-like-this)
;; extend string editing
(defun swap-text (str1 str2 beg end)
  "Changes all STR1 to STR2 and all STR2 to STR1 in beg/end region."
  (interactive "sString A: \nsString B: \nr")
  (if mark-active
      (setq deactivate-mark t)
    (setq beg (point-min) end (point-max))) 
  (goto-char beg)
  (while (re-search-forward
          (concat "\\(?:\\b\\(" (regexp-quote str1) "\\)\\|\\("
                  (regexp-quote str2) "\\)\\b\\)") end t)
    (if (match-string 1)
	(replace-match str2 t t)
      (replace-match str1 t t))))
;; From active region to multiple cursors:
(global-set-key (kbd "C-c m r") 'set-rectangular-region-anchor)
(global-set-key (kbd "C-c m c") 'mc/edit-lines)
(global-set-key (kbd "C-c m e") 'mc/edit-ends-of-lines)
(global-set-key (kbd "C-c m a") 'mc/edit-beginnings-of-lines)

;;* Git
(use-package git-timemachine
  :ensure t
  :init)
;;* Search
(global-set-key (kbd "C-<") 'grep-find)
(global-set-key (kbd "C->") 'helm-locate)
(global-set-key (kbd "C-!") 'save-buffers-kill-emacs)

;;* UI
(use-package which-key
  :ensure t
  :init
  :config
  (add-hook 'after-init-hook
	    (lambda ()
	      (which-key-mode)
	      (which-key-setup-side-window-right-bottom)
	      (diminish 'guide-key-mode)
	      ))
  )
;;* Fixes
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . sh-mode))
;;** Helpers
(defun add-auto-mode (mode &rest patterns)
  "Add entries to `auto-mode-alist' to use `MODE' for all given file `PATTERNS'."
  (dolist (pattern patterns)
    (add-to-list 'auto-mode-alist (cons pattern mode))))
(provide 'init-utils)
