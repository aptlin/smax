;; * Haskell

;; ** Haskell Mode

(use-package haskell-mode
  :ensure t
  :init
  :config
  (add-hook 'haskell-mode-hook                 'turn-on-haskell-indentation)
  (eval-after-load 'haskell-mode
    '(define-key haskell-mode-map (kbd "<f2> <f3>") 'haskell-navigate-imports))
  (eval-after-load 'haskell-mode
    '(define-key haskell-mode-map (kbd "C-c C-o") 'haskell-compile))
  (eval-after-load 'haskell-cabal
    '(define-key haskell-cabal-mode-map (kbd "C-c C-o") 'haskell-compile))
  (add-hook 'haskell-mode-hook 'haskell-auto-insert-module-template)
  )

;; ** Refactoring
;; *** Hindent
(use-package hindent
  :init
  :config
  (add-hook 'haskell-mode-hook 'hindent-mode)
  )
;; *** Flycheck
(use-package flycheck-haskell
  :defer t
  :config
  (add-hook 'flycheck-mode-hook 'flycheck-haskell-setup))


;; ** Visual
(use-package rainbow-delimiters
  :init
  :config
  (add-hook 'haskell-mode-hook 'rainbow-delimiters-mode))

(provide 'smax-haskell)
