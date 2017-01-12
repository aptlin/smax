;; keybindings
(global-set-key (kbd "C-\"") '(lambda ()  (interactive) (ansi-term "/bin/zsh")))
(add-hook 'term-mode-hook (lambda()
                            (yas-minor-mode -1)))
(provide 'init-eshell)
