(setq-default yas-snippet-dirs '("~/.emacs.d/user/snippets"))
(use-package yasnippet
  :ensure t
  :init
  :config
  (progn
    (yas-global-mode 1)
    (yas-load-directory (expand-file-name "snippets" user-dir))
    (setq yas-indent-line 'auto)
    (setq yas-also-auto-indent-first-line t)
    )
  )
(provide 'init-yas)
