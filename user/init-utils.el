;;* Editing
;; ** Helpers
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

(use-package ivy-historian
  :ensure t
  :init
  :config
  (add-hook 'after-init-hook
	    (lambda ()
	      (ivy-historian-mode)
	      (diminish 'historian-mode)
	      (diminish 'ivy-historian-mode)))
  )

;;* Git
(use-package git-timemachine
  :ensure t
  :init)

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
;;* Code
(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode t)
  (diminish 'flycheck))
;;* Fixes
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . sh-mode))
;;** Helpers
(defun add-auto-mode (mode &rest patterns)
  "Add entries to `auto-mode-alist' to use `MODE' for all given file `PATTERNS'."
  (dolist (pattern patterns)
    (add-to-list 'auto-mode-alist (cons pattern mode))))
(provide 'init-utils)




