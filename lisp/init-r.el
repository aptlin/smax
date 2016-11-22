(require-package 'ess)

;; http://stats.blogoverflow.com/2011/08/using-emacs-to-work-with-r/
(add-to-list 'auto-mode-alist '("\\.r\\.R\\'"  . R-mode))
(setq comint-input-ring-size 1000)
(setq ess-indent-level 4)
(setq ess-arg-function-offset 4)
(setq ess-else-offset 4)
(add-hook 'inferior-ess-mode-hook
          '(lambda nil
             (define-key inferior-ess-mode-map [\C-up]
               'comint-previous-matching-input-from-input)
             (define-key inferior-ess-mode-map [\C-down]
               'comint-next-matching-input-from-input)
             (define-key inferior-ess-mode-map [\C-x \t]
               'comint-dynamic-complete-filename)
             )
          )
(setq ess-ask-about-transfile t)
(provide 'init-r)
