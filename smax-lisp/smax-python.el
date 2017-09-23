;;; smax-python.el ---

;;; Commentary:
;;
;;; Code:

;; * Python
;; **    Behaviour

;; This eliminates an annoying message about the interpreter not using
;; readline. That doesn't seem to matter at all.
(setq warning-suppress-types '((python)
                               (emacs)))
;; **    Elpy

(use-package elpy
  :init
  :bind (:map python-mode-map
              ("M-." . elpy-doc)
              ("M-*" . elpy-goto-definition))
  :config
  (add-hook 'python-mode-hook 'elpy-enable)
  (setq python-indent-offset 4)
  )

;; **    Keybindings
(define-key inferior-python-mode-map (kbd "C-c r")   'ipython-reset)
(define-key python-mode-map (kbd "C-c C-c") 'python-shell-send-defun)
(define-key python-mode-map (kbd "C-c C-l") 'python-shell-send-buffer)
(define-key python-mode-map (kbd "C-c h")   'mk-python-docs)

;; **    Highlight Indentation
(use-package highlight-indentation
  :ensure t
  :diminish highlight-indentation-mode
  :config (progn (add-hook 'python-mode-hook #'highlight-indentation-mode)))
;; **    Utils
(use-package pyvenv
  :init
  :config
  (setq shell-file-name "/bin/bash")
  (setenv "WORKON_HOME" "~/dev/edu/")
  (pyvenv-activate "~/dev/edu/env")
  )
;; * End
(provide 'smax-python)

;;; smax-python.el ends here
