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
(defun smax-disable-scroll-bar (frame)
  (modify-frame-parameters frame
			   '((vertical-scroll-bars . nil)
			     (horizontal-scroll-bars . nil))))
(add-hook 'after-make-frame-functions 'smax-disable-scroll-bar)

;; ** Locations

(setq custom-file (expand-file-name "user/custom.el" smax-dir))
(setq auto-save-list-file-prefix (expand-file-name "auto-save-list/saves-" smax-dir))

;; ** Behaviour
(setq redisplay-dont-pause nil)
(global-auto-revert-mode 1)
(setq auto-revert-verbose nil)
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
(setq vc-follow-symlinks nil)
;; ** Bindings
(π "C-!" #'save-buffers-kill-emacs)
;; ** Modes
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . sh-mode))
(if (version< emacs-version "25.0")
    (progn
      (require 'saveplace)
      (setq-default save-place t))
  (save-place-mode 1))
;; *** Diminish modes
(diminish 'abbrev-mode)
(diminish 'emacs-keybinding-command-tooltip-mode)
(diminish 'ivy-mode)
(diminish 'orgstruct-mode)
(diminish 'save-place-mode)
(diminish 'visual-line-mode)
;; * Helpers
(use-package which-key
  :ensure t
  :init
  :diminish which-key-mode
  :config
  (add-hook 'after-init-hook
	    (lambda ()
	      (which-key-mode)
	      (which-key-setup-side-window-right-bottom)
	      (diminish 'guide-key-mode)
	      )))
;; * Programming

(use-package flycheck
  :diminish flycheck-mode
  :init
  (global-flycheck-mode t))

;; * Images

(require 'image-mode)
(define-key image-mode-map (kbd "q")
  (lambda ()
    (interactive)
    (kill-buffer (current-buffer))))


;; * The end
(provide 'smax)

;;; smax.el ends here
