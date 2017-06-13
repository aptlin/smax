;; * Haskell
;; ** Ebal
(use-package ebal
  :defer t
  :config
  (setq
   ebal-operation-mode                   'stack
   haskell-ask-also-kill-buffers         nil ; don't ask
   haskell-process-load-or-reload-prompt t   ; please ask
   haskell-process-show-debug-tips       nil ; don't show anything
   haskell-process-type                  'stack-ghci
   haskell-process-args-stack-ghci       '("--ghci-options=-ferror-spans"))
  (define-key haskell-mode-map (kbd  "C-c e") 'ebal-execute)
  )

;; ** Haskell Mode
;;; Code:
(use-package haskell-mode
  :ensure t
  :init
  :config
  (add-hook 'haskell-mode-hook                 'interactive-haskell-mode)
  (add-hook 'haskell-mode-hook                 'turn-on-haskell-indentation)
  (eval-after-load 'haskell-mode
    '(define-key haskell-mode-map [f2] 'haskell-navigate-imports))
  
  (eval-after-load 'haskell-mode
    '(define-key haskell-mode-map (kbd "C-c C-o") 'haskell-compile))
  (eval-after-load 'haskell-cabal
    '(define-key haskell-cabal-mode-map (kbd "C-c C-o") 'haskell-compile))
  (add-hook 'haskell-mode-hook 'haskell-auto-insert-module-template)

  (with-eval-after-load 'smartparens
    (add-to-list 'sp-no-reindent-after-kill-modes 'haskell-cabal-mode)
    (add-to-list 'sp-no-reindent-after-kill-modes 'haskell-mode))
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
