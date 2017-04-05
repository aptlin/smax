(use-package ggtags
  :ensure t
  :config 
  (add-hook 'c-mode-common-hook
	    (lambda ()
	      (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
		(ggtags-mode 1)))))
(use-package irony
  :ensure t
  :init
  :config

  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)

  (defun my-irony-mode-hook ()
    (define-key irony-mode-map
      [remap completion-at-point] 'counsel-irony)
    (define-key irony-mode-map
      [remap complete-symbol] 'counsel-irony))
  (add-hook 'irony-mode-hook 'my-irony-mode-hook)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
  )
(provide 'smax-c)
