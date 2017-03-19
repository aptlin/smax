

;;* UI

;;* Code
(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode t)
  (diminish 'flycheck))
;;* Fixes
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . sh-mode))

(provide 'init-utils)




