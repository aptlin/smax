(setq-default yas-snippet-dirs (list (expand-file-name "snippets" user-dir)))
(use-package yasnippet
  :init
  :config
  (progn
    (yas-global-mode 1)
    (yas-load-directory (expand-file-name "snippets" user-dir))
    (setq yas-indent-line 'auto)
    (setq yas-also-auto-indent-first-line t)
    ))
(provide 'init-yas)
