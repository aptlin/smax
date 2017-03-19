;;* Editing
;; ** Helpers
(defun prettify-paragraph ()
  (interactive)
  (align-current)
  (fill-paragraph)
  )
(global-set-key (kbd "<S-return>") 'prettify-paragraph)

(defun rename-this-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file!" name))
    (progn
      (when (file-exists-p filename)
        (rename-file filename new-name 1))
      (set-visited-file-name new-name)
      (rename-buffer new-name))))

;; ** Packages

(electric-indent-mode 1)
(define-key global-map (kbd "RET")	'newline-and-indent)
(define-key global-map (kbd "C-\.")	'align-regexp)
(subword-mode)
(use-package hungry-delete
  :ensure t
  :init
  :config
  (global-hungry-delete-mode))
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
  (put 'paredit-newline 'delete-selection t)

  ;; functions in smartparens that do not have an equivalent in paredit - take a look at them
  (when nil
    '(sp-beginning-of-sexp
      sp-end-of-sexp
      sp-next-sexp
      sp-previous-sexp
      sp-kill-sexp
      sp-unwrap-sexp
      sp-backward-unwrap-sexp
      sp-select-next-thing-exchange
      sp-select-next-thing
      sp-forward-symbol
      sp-backward-symbol))
  )

(use-package smartparens-config
  :ensure smartparens
  :config
  (progn
    (show-smartparens-global-mode t))
  (smartparens-global-mode)
  (require 'smartparens-latex)
  (add-hook 'prog-mode-hook 'turn-on-smartparens-strict-mode)
  
  (bind-keys
   :map smartparens-mode-map
   ("C-M-a" . sp-beginning-of-sexp)
   ("C-M-e" . sp-end-of-sexp)

   ("C-<down>" . sp-down-sexp)
   ("C-<up>"   . sp-up-sexp)
   ("M-<down>" . sp-backward-down-sexp)
   ("M-<up>"   . sp-backward-up-sexp)
   
   ("C-M-f" . sp-forward-sexp)
   ("C-M-b" . sp-backward-sexp)

   ("C-M-n" . sp-next-sexp)
   ("C-M-p" . sp-previous-sexp)

   ("C-S-f" . sp-forward-symbol)
   ("C-S-b" . sp-backward-symbol)

   ("C-<right>" . sp-forward-slurp-sexp)
   ("M-<right>" . sp-forward-barf-sexp)
   ("C-<left>"  . sp-backward-slurp-sexp)
   ("M-<left>"  . sp-backward-barf-sexp)

   ("C-M-t" . sp-transpose-sexp)
   ("C-M-k" . sp-kill-sexp)
   ("C-k"   . sp-kill-hybrid-sexp)
   ("M-k"   . sp-backward-kill-sexp)
   ("C-M-w" . sp-copy-sexp)
   
   ("M-<backspace>" . backward-kill-word)
   ("C-<backspace>" . sp-backward-kill-word)
   ([remap sp-backward-kill-word] . backward-kill-word)

   ("M-[" . sp-backward-unwrap-sexp)
   ("M-]" . sp-unwrap-sexp)
   ("C-x C-t" . sp-transpose-hybrid-sexp)
   )
  )

(add-hook 'prog-mode-hook 'turn-on-smartparens-strict-mode)
(use-package ivy-historian
  :ensure t
  :init
  :config
  (add-hook 'after-init-hook
	    (lambda ()
	      (ivy-historian-mode)
	      (diminish 'historian-mode)
	      (diminish 'ivy-historian-mode)))
  )
  
(use-package expand-region
  :ensure t
  :init
  :bind (
	 ("C-@" . er/expand-region)))

(use-package multiple-cursors
  :ensure t
  :init
  :config
  (global-set-key (kbd "C-c m p") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c m n") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-c m t") 'mc/mark-all-like-this)
  )
;; multiple-cursors

;; extend string editing
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
;; From active region to multiple cursors:
(global-set-key (kbd "C-c m r") 'set-rectangular-region-anchor)
(global-set-key (kbd "C-c m c") 'mc/edit-lines)
(global-set-key (kbd "C-c m e") 'mc/edit-ends-of-lines)
(global-set-key (kbd "C-c m a") 'mc/edit-beginnings-of-lines)

;;* Git
(use-package git-timemachine
  :ensure t
  :init)
;;* Search
(global-set-key (kbd "C-<") 'counsel-ag)
(global-set-key (kbd "C->") 'helm-locate)
(global-set-key (kbd "C-!") 'save-buffers-kill-emacs)

;;* UI
(use-package which-key
  :ensure t
  :init
  :config
  (add-hook 'after-init-hook
	    (lambda ()
	      (which-key-mode)
	      (which-key-setup-side-window-right-bottom)
	      (diminish 'guide-key-mode)
	      ))
  )
;;* Code
(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode t)
  (diminish 'flycheck))
;;* Fixes
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . sh-mode))
;;** Helpers
(defun add-auto-mode (mode &rest patterns)
  "Add entries to `auto-mode-alist' to use `MODE' for all given file `PATTERNS'."
  (dolist (pattern patterns)
    (add-to-list 'auto-mode-alist (cons pattern mode))))
(provide 'init-utils)




