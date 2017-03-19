;;; smax.el ---

;;; Commentary:
;; 
;; * Basic settings
;; ** Theme and Look 
(load-theme 'leuven)
(setq inhibit-startup-screen t) ;; stop showing startup screen
(tool-bar-mode -1)           ; remove the icons
(menu-bar-mode -1)           ; keep the menus
(global-visual-line-mode 1) ;; how long lines are handled. 
(if (display-graphic-p)
    (progn
      (scroll-bar-mode -1)))

;; ** Locations

(setq custom-file (expand-file-name "user/custom.el" smax-dir))
(setq auto-save-list-file-prefix (expand-file-name "auto-save-list/saves-" smax-dir))

;; ** Behaviour
(global-auto-revert-mode 1)
(global-font-lock-mode t) ;; turn on font-lock mode everywhere
(auto-fill-mode -1)
(fset 'yes-or-no-p 'y-or-n-p) ; answer with y/n instead of yes/no
(setq x-select-enable-clipboard t
      x-select-enable-primary t
      save-interprogram-paste-before-kill t
      apropos-do-all t
      mouse-yank-at-point t
      require-final-newline t
      load-prefer-newer t
      save-place-file (concat user-emacs-directory "places")
      backup-directory-alist `(("." . ,(concat user-emacs-directory
					       "backups"))))
;; abbrevs
(setq abbrev-file-name (expand-file-name "user/abbrev_defs" smax-dir))
(setq save-abbrevs t)
(setq-default abbrev-mode t)
(setq save-abbrevs 'silently)

;; * Version control
;; Disable all version control. makes startup and opening files much faster
;; except git and svn which I actually use
(setq vc-handled-backends '(Git))

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

;; * Diminish modes
(diminish 'orgstruct-mode)
(diminish 'ivy-mode)
(diminish 'lispy-mode)
(diminish 'abbrev-mode)
(diminish 'visual-line-mode)
(diminish 'beacon-mode)
(diminish 'aggressive-indent-mode)
(diminish 'emacs-keybinding-command-tooltip-mode)

;; * Programming
;; ** Debugging
(add-hook 'edebug-mode-hook
	  (lambda ()
	    (define-key edebug-mode-map (kbd "h") 'edebug-goto-here)))

;; * Misc

(require 'image-mode)
(define-key image-mode-map (kbd "q")
  (lambda ()
    (interactive)
    (kill-buffer (current-buffer))))


;; * The end
(provide 'smax)

;;; smax.el ends here
