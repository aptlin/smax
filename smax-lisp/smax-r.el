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
  )

;; * End
(provide 'smax-r)

;;; smax-r.el ends here
