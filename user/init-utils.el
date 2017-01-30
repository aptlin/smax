(global-set-key (kbd "C-<") 'grep-find)
(global-set-key (kbd "C->") 'helm-locate)
(global-set-key (kbd "C-!") 'save-buffers-kill-emacs)
(use-package expand-region
  :ensure t
  :init
  :bind (
	 ("C-@" . er/expand-region)))
(provide 'init-utils)
