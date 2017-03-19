;;; smax-vc.el --- Package

;;; Commentary:
;;
;; Code :
;; * Version control
;; ** Git
;; Disable all version control bug git.
(setq vc-handled-backends '(Git))
;; *** Packages
;; **** Magit
(use-package magit
  :init (setq magit-completing-read-function 'ivy-completing-read)
  :bind
  ("<f5>" . magit-status)
  ("C-c v t" . magit-status)
  :config
  (define-key 'vc-prefix-map "t" 'magit-status)
  )

;; **** Utilities
(use-package git-messenger
  :bind ("C-x v o" . git-messenger:popup-message))
(use-package git-timemachine
  :ensure t
  :init)
;; ** Functions and Bindings

;; *** Functions
(defun smax-update ()
  "Update smax from github."
  (interactive)
  (let ((default-directory smax-dir))
    (when (not (string= "" (shell-command-to-string "git status --porcelain")))
      (shell-command "git add *")
      (shell-command "git commit -am \"commiting smax.\""))
    (shell-command "git pull origin master")
    (shell-command "git submodule update")
    (load-file "init.el")))

(provide 'smax-vc)

;; *** Bindings

(define-key 'vc-prefix-map "p" (lambda () (interactive) (vc-git-push nil)))
(define-key 'vc-prefix-map "P" (lambda () (interactive) (vc-git-pull nil)))
