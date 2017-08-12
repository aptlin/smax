;;; smax-python.el ---

;;; Commentary:
;;
;;; Code:

;; * R
(use-package ess
  :ensure t
  :init
  :config
  (setq ess-eval-visibly nil)       ; ESS will not print the evaluated
                                        ; commands, also speeds up the
                                        ; evaluation

  (setq ess-ask-for-ess-directory nil)  ;if you don't want to be
                                        ;prompted each time you start
                                        ;an interactive R session
  (add-hook 'ess-mode-hook
            (lambda ()
              (setq outline-regexp "## \\*+"
                    orgstruct-heading-prefix-regexp "## ?\\*+\\|\\`")
              (orgstruct-mode)
              (org-global-cycle 3)))
  )

;; * End
(provide 'smax-r)

;;; smax-r.el ends here
