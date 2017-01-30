(global-set-key (kbd "C-<") 'grep-find)
(global-set-key (kbd "C->") 'helm-locate)
(global-set-key (kbd "C-!") 'save-buffers-kill-emacs)
(use-package expand-region
  :ensure t
  :init
  :bind (
	 ("C-@" . er/expand-region)))
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
