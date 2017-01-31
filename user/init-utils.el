;; Editing
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
;; From active region to multiple cursors:
(global-set-key (kbd "C-c m r") 'set-rectangular-region-anchor)
(global-set-key (kbd "C-c m c") 'mc/edit-lines)
(global-set-key (kbd "C-c m e") 'mc/edit-ends-of-lines)
(global-set-key (kbd "C-c m a") 'mc/edit-beginnings-of-lines)

;; Search
(global-set-key (kbd "C-<") 'grep-find)
(global-set-key (kbd "C->") 'helm-locate)
(global-set-key (kbd "C-!") 'save-buffers-kill-emacs)

;; Helpers
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

(provide 'init-utils)
