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

;; * Styling
(c-add-style "my-style"
             '("stroustrup"
               (indent-tabs-mode . nil)        ; use spaces rather than tabs
               (c-basic-offset . 4)            ; indent by four spaces
               (c-offsets-alist . ((inline-open . 0)  ; custom indentation rules
                                   (brace-list-open . 0)
                                   (statement-case-open . +)))))

(defun my-c++-mode-hook ()
  (c-set-style "my-style")        ; use my-style defined above
  (auto-fill-mode)
  (c-toggle-auto-hungry-state 1))

(add-hook 'c++-mode-hook 'my-c++-mode-hook)
(font-lock-add-keywords 'c++-mode
                        `((,(concat
                             "\\<[_a-zA-Z][_a-zA-Z0-9]*\\>"       ; Object identifier
                             "\\s *"                              ; Optional white space
                             "\\(?:\\.\\|->\\)"                   ; Member access
                             "\\s *"                              ; Optional white space
                             "\\<\\([_a-zA-Z][_a-zA-Z0-9]*\\)\\>" ; Member identifier
                             "\\s *"                              ; Optional white space
                             "(")                                 ; Paren for method invocation
                           1 'font-lock-function-name-face t)))

;; * Tagging
(use-package rtags
  :bind (:map c++-mode-map
              ("M-\." . rtags-find-symbol-at-point))
  :init
  :config
  (require 'rtags)
  (setq rtags-completions-enabled t)
  (setq rtags-autostart-diagnostics t)
  (rtags-enable-standard-keybindings)
  (add-hook 'c-mode-hook 'rtags-start-process-unless-running)
  (add-hook 'c++-mode-hook 'rtags-start-process-unless-running)
  (add-hook 'objc-mode-hook 'rtags-start-process-unless-running)
  (rtags-enable-standard-keybindings c-mode-base-map "<f2> r")
  )

(use-package company-rtags
  :init
  :config
  (require 'company-rtags)
  (eval-after-load 'company
    '(add-to-list
      'company-backends 'company-rtags))
  )

(use-package helm-rtags
  :init
  :config
  (setq rtags-use-helm t)
  (setq rtags-display-result-backend 'helm)
  )

;; * Misc
(use-package irony
  :init
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'objc-mode-hook 'irony-mode)

  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

(use-package company-irony
  :init
  :config
  (eval-after-load 'company
    '(add-to-list 'company-backends 'company-irony))
  )

(use-package flycheck-irony
  :init
  :config
  (add-hook 'c++-mode-hook 'flycheck-mode)
  (add-hook 'c-mode-hook 'flycheck-mode)
  (eval-after-load 'flycheck
    '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))
  )

(use-package irony-eldoc
  :init
  :config
  (add-hook 'irony-mode-hook 'irony-eldoc))

(provide 'smax-c)
