;; smax-editing -- Summary: navigation
;;; Commentary:
;;; Code:
;; * Editing
;; ** Packages
;; *** Delimiters

;; **** Paredit
(use-package paredit
  :ensure t
  :init
  :config

  (define-key paredit-mode-map (kbd "C-w") 'paredit-kill-region-or-backward-word)
  (define-key paredit-mode-map (kbd "M-C-<backspace>") 'backward-kill-sexp)

  ;; don't hijack \ please
  (define-key paredit-mode-map (kbd "\\") nil)

  ;; Enable `paredit-mode' in the minibuffer, during `eval-expression'.
  (defun conditionally-enable-paredit-mode ()
    (if (eq this-command 'eval-expression)
	(paredit-mode 1)))

  (add-hook 'minibuffer-setup-hook 'conditionally-enable-paredit-mode)

  ;; making paredit work with delete-selection-mode
  (put 'paredit-forward-delete 'delete-selection 'supersede)
  (put 'paredit-backward-delete 'delete-selection 'supersede)
  (put 'paredit-newline 'delete-selection t))
;; **** Smartparens
(use-package smartparens-config
  :ensure smartparens
  :config
  (show-smartparens-global-mode t)
  (smartparens-global-mode 1)
  (require 'smartparens-latex)
  (add-hook 'prog-mode-hook 'turn-on-smartparens-strict-mode) 
  (τ smartparens smartparens "<C-backspace>" #'sp-backward-kill-sexp)
  (τ smartparens smartparens "M-b"           #'sp-backward-sexp)
  (τ smartparens smartparens "M-d"           #'sp-kill-sexp)
  (τ smartparens smartparens "M-f"           #'sp-forward-sexp)
  (τ smartparens smartparens "M-h"           #'sp-select-next-thing)
  (τ smartparens smartparens "M-k"           #'sp-kill-hybrid-sexp)
  (τ smartparens smartparens "M-t"           #'sp-add-to-previous-sexp))
;; **** Expand-region
(use-package expand-region
  :ensure t
  :init
  :bind (
	 ("C-@" . er/expand-region)))
;; *** Multiple-cursors

(use-package multiple-cursors
  :ensure t
  :init
  :config
  (global-set-key (kbd "C-c m p") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c m n") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-c m t") 'mc/mark-all-like-this)
  (global-set-key (kbd "C-c m r") 'set-rectangular-region-anchor)
  (global-set-key (kbd "C-c m c") 'mc/edit-lines)
  (global-set-key (kbd "C-c m e") 'mc/edit-ends-of-lines)
  (global-set-key (kbd "C-c m a") 'mc/edit-beginnings-of-lines))

;; *** Hungry-Delete
(use-package hungry-delete
  :ensure t
  :init
  :config
  (global-hungry-delete-mode))
;; *** Aggressive Indent
(use-package aggressive-indent
  :init
  :config
  (aggressive-indent-global-mode 1)
  (add-to-list 'aggressive-indent-excluded-modes 'haskell-mode))
;; ** Modes
;; *** Parentheses
(show-paren-mode 1)         ;; highlight parentheses
(setq show-paren-style 'mixed)
(electric-indent-mode 0)
;; ** Functions and Bindings
;; *** Functions

(defun swap-text (str1 str2 beg end)
  "Changes all STR1 to STR2 and all STR2 to STR1 in beg/end region."
  (interactive "sString A: \nsString B: \nr")
  (if mark-active
      (setq deactivate-mark t)
    (setq beg (point-min) end (point-max))) 
  (goto-char beg)
  (while (re-search-forward
          (concat "\\(?:\\b\\(" (regexp-quote str1) "\\)\\|\\("
                  (regexp-quote str2) "\\)\\b\\)") end t)
    (if (match-string 1)
	(replace-match str2 t t)
      (replace-match str1 t t))))
(defun prettify-paragraph ()
  (interactive)
  (align-current)
  (fill-paragraph))

;; *** Bindings
(global-set-key (kbd "S-RET"	) 'prettify-paragraph)
(global-set-key (kbd "RET"	) 'newline-and-indent)
(global-set-key (kbd "C-\."	) 'align-regexp)

(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR." t)
(global-set-key (kbd "M-z") 'zap-up-to-char)
