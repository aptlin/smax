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

  ;; (custom-set-variables '(haskell-tags-on-save t))
  ;; stylish-haskell formatting on
  (setq-default haskell-stylish-on-save nil)
  
  (eval-after-load 'haskell-mode
    '(define-key haskell-mode-map (kbd "C-c C-o") 'haskell-compile))
  (eval-after-load 'haskell-cabal
    '(define-key haskell-cabal-mode-map (kbd "C-c C-o") 'haskell-compile))
  (add-hook 'haskell-mode-hook 'haskell-auto-insert-module-template)

  (with-eval-after-load 'smartparens
    (add-to-list 'sp-no-reindent-after-kill-modes 'haskell-cabal-mode)
    (add-to-list 'sp-no-reindent-after-kill-modes 'haskell-mode))

  (defun mk-haskell-set-min-versions (lib-list)
    "Help Flycheck handle Cabal MIN_VERSION_ definitions.

     LIB-LIST should of the following form:

     (LIB-NAME V0 V1 V2)

     Where LIB-NAME is a string, name of library and V0, V1, V2 are
     version components."
    
    (dolist (item lib-list)
      (cl-destructuring-bind (lib a b c) item
        (let ((definition
                (format
                 "((a<%d)||(a==%d&&b<%d)||(a==%d&&b==%d&&c<=%d))"
                 a a b a b c)))
          (add-to-list
           'flycheck-ghc-args
           (format "-DMIN_VERSION_%s(a,b,c)=%s"
                   lib definition))
          (add-to-list
           'flycheck-hlint-args
           (format "--cpp-define=MIN_VERSION_%s(a,b,c)=%s"
                   lib definition))))))

  (mk-haskell-set-min-versions
   '(("Cabal"      1 22 0)
     ("QuickCheck" 2 8 2)
     ("base"       4 8 0)
     ("bytestring" 0 10 6)
     ("containers" 0 5 7)
     ("directory"  1 2 2)
     ("process"    1 2 1)
     ("retry"      0 6 0)
     ("time"       1 5 0)))
  (add-to-list 'mk-search-prefix '(haskell-cabal-mode       . "haskell"))
  (add-to-list 'mk-search-prefix '(haskell-interactive-mode . "haskell"))
  (add-to-list 'mk-search-prefix '(haskell-mode             . "haskell"))
  )

;; ** Extensions
;; *** Hasky Extensions
(use-package hasky-extensions
  :defer t
  :config
  (define-key haskell-mode-map (kbd  "C-c y") 'hasky-extensions)
  (add-hook 'hasky-extensions-prettifying-hook 'whitespace-cleanup)
  (add-hook 'hasky-extensions-prettifying-hook 'mk-single-empty-line)
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
