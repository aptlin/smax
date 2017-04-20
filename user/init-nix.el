(use-package nix-mode
  :init
  :config
  (setq auto-mode-alist (append '(("\\.nix$" . nix-mode))
				auto-mode-alist))
  )
(provide 'init-nix)
