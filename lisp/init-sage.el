(require-package 'sage-shell-mode)
(require-package 'ob-sagemath)
(require-package 'auto-complete-sage)
(setq sage-shell:use-prompt-toolkit t)
(setq sage-shell:sage-root "/home/aleph/PROG/SageMath")
;; Run SageMath by M-x run-sage instead of M-x sage-shell:run-sage
(sage-shell:define-alias)

;; Turn on eldoc-mode in Sage terminal and in Sage source files
(add-hook 'sage-shell-mode-hook #'eldoc-mode)
(add-hook 'sage-shell:sage-mode-hook #'eldoc-mode)

;; If you have Sage 7.4 or later, uncomment the following line.
(setq sage-shell:use-prompt-toolkit t)

;; Ob-sagemath supports only evaluating with a session.
(setq org-babel-default-header-args:sage '((:session . t)
                                           (:results . "output")))

;; C-c c for asynchronous evaluating (only for SageMath code blocks).
(with-eval-after-load "org"
  (define-key org-mode-map (kbd "C-c c") 'ob-sagemath-execute-async))

;; Do not confirm before evaluation
(setq org-confirm-babel-evaluate nil)

;; Do not evaluate code blocks when exporting.
(setq org-export-babel-evaluate nil)

;; Show images when opening a file.
(setq org-startup-with-inline-images t)

;; Show images after evaluating code blocks.
(add-hook 'org-babel-after-execute-hook 'org-display-inline-images)
(add-hook 'sage-shell:sage-mode-hook 'ac-sage-setup)
(add-hook 'sage-shell-mode-hook 'ac-sage-setup)

(provide 'init-sage)
