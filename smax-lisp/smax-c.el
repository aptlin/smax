;; * Orgstruct
(require 'cc-mode)
(defconst lel-font-lock-keywords
  '(("^// ?\\(\\* .*\\)$" 1 'org-level-1 prepend)
    ("^// ?\\(\\*\\* .*\\)$" 1 'org-level-2 prepend)
    ("^// ?\\(\\*\\*\\* .*\\)$" 1 'org-level-3 prepend)
    ("^// ?\\(\\*\\*\\*\\* .*\\)$" 1 'org-level-4 prepend)
    ("^// ?\\(\\*\\*\\*\\*\\* .*\\)$" 1 'org-level-5 prepend)
    ;; (lel-outline-comment-highlight 1 'default prepend)
    ("`\\([^\n']+\\)'" 1 font-lock-constant-face prepend)))

(font-lock-add-keywords 'c-mode lel-font-lock-keywords)
(font-lock-add-keywords 'c++-mode lel-font-lock-keywords)

(defun c-outline-setup ()
  "Setup outline and orgstruct mode for emacs-lisp code.
This enables you to use tab to open and close outlines."
  (setq-local outline-regexp "// ?\\*+\\|\\`")
  (setq-local orgstruct-heading-prefix-regexp "// ?\\*+\\|\\`")
  (outline-minor-mode)
  (orgstruct-mode)
  (outline-show-branches))

(add-hook 'c-mode-common-hook
          #'c-outline-setup)


;; * Tagging
(use-package counsel-gtags
  :init
  (add-hook 'c-mode-common-hook 'counsel-gtags-mode)


  (with-eval-after-load 'counsel-gtags
    (define-key counsel-gtags-mode-map (kbd "M-.") 'counsel-gtags-find-definition)
    (define-key counsel-gtags-mode-map (kbd "M-r") 'counsel-gtags-find-reference)
    (define-key counsel-gtags-mode-map (kbd "M-s") 'counsel-gtags-find-symbol)
    (define-key counsel-gtags-mode-map (kbd "M-,") 'counsel-gtags-go-backward))
  (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
  )
(use-package function-args
  :init
  (fa-config-default)
  (set-default 'semantic-case-fold t)
  )

(use-package company
  :init
  (require 'company)
  (add-hook 'after-init-hook 'global-company-mode)

  (setq company-backends (delete 'company-semantic company-backends))
  (define-key c-mode-map  [(tab)] 'company-complete)
  (define-key c++-mode-map  [(tab)] 'company-complete)

  (add-to-list 'company-backends 'company-c-headers))
(use-package company-c-headers
  :init
  (add-to-list 'company-c-headers-path-system "/run/current-system/sw/lib")
  )
(use-package semantic
  :init
  
  (require 'semantic)

  (global-semanticdb-minor-mode 1)
  (global-semantic-idle-scheduler-mode 1)
  (semantic-mode 1)
  (semantic-add-system-include "/run/current-system/sw/lib")
  )
;; (use-package srefactor
;;   :init
;;   (require 'srefactor)
;;   (require 'srefactor-lisp)

;;   (define-key c-mode-map (kbd "M-RET") 'srefactor-refactor-at-point)
;;   (define-key c++-mode-map (kbd "M-RET") 'srefactor-refactor-at-point))
;; (use-package ggtags
;;   :ensure t
;;   :config
;;   (add-hook 'c-mode-common-hook
;;             (lambda ()
;;               (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
;;                 (ggtags-mode 1))))
;;   )
;; (use-package irony
;;   :init
;;   (add-hook 'c++-mode-hook 'irony-mode)
;;   (add-hook 'c-mode-hook 'irony-mode)

;;   (defun my-irony-mode-hook ()
;;     (define-key irony-mode-map
;;       [remap completion-at-point] 'counsel-irony)
;;     (define-key irony-mode-map
;;       [remap complete-symbol] 'counsel-irony))
;;   (add-hook 'irony-mode-hook 'my-irony-mode-hook)
;;   (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
;;   )

(provide 'smax-c)
