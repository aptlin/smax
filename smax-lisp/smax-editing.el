;; smax-editing -- Summary: navigation
;;; Commentary:
;;; Code:
;; * Editing
;; ** Behaviour
;; *** Better Indentation
(defun malb/indent-or-complete (&optional arg)
  (interactive "P")
  (cond
   ;; if a region is active, indent
   ((use-region-p)
    (indent-region (region-beginning)
                   (region-end)))
   ;; if the next char is space or eol, but prev char not whitespace
   ((and (not (active-minibuffer-window))
         (or (looking-at " ")
             (looking-at "$"))
         (looking-back "[^[:space:]]")
         (not (looking-back "^")))

    (cond (company-mode (company-complete-common))
          (auto-complete-mode (auto-complete))))

   ;; no whitespace anywhere
   ((and (not (active-minibuffer-window))
         (looking-at "[^[:space:]]")
         (looking-back "[^[:space:]]")
         (not (looking-back "^")))
    (cond
     ((bound-and-true-p origami-mode)
      (origami-toggle-node (current-buffer) (point)))
     ((bound-and-true-p outline-minor-mode)
      (save-excursion (outline-cycle)))))

   ;; by default just call whatever was bound
   (t
    (let ((fn (or (lookup-key (current-local-map) (kbd "TAB"))
                  'indent-for-tab-command)))
      (if (not (called-interactively-p 'any))
          (fn arg)
        (setq this-command fn)
        (call-interactively fn))))))

(bind-key "<tab>" #'malb/indent-or-complete)
;; *** Dealing with sentences
(setq sentence-end-double-space nil)
(bind-key "C-x C-t" #'transpose-sentences)
(defadvice kill-sentence (after delete-horizontal-space activate)
  "Delete trailing spaces and tabs as well."
  (delete-horizontal-space))
;; ** Packages
;; *** Delimiters

;;**** Paredit
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
;;**** Smartparens
(use-package smartparens-config
  :ensure smartparens
  :diminish smartparens-mode
  :config
  (show-smartparens-global-mode t)
  (smartparens-global-mode 1)
  (require 'smartparens-latex)
  (add-hook 'prog-mode-hook 'turn-on-smartparens-strict-mode)
  (define-key smartparens-mode-map (kbd  "<C-backspace>") 'sp-backward-kill-sexp)
  (define-key smartparens-mode-map (kbd  "M-b")           'sp-backward-sexp)
  (define-key smartparens-mode-map (kbd  "M-f")           'sp-forward-sexp)
  (define-key smartparens-mode-map (kbd  "M-h")           'sp-select-next-thing)
  (define-key smartparens-mode-map (kbd  "M-k")           'sp-kill-hybrid-sexp)
  (define-key smartparens-mode-map (kbd  "M-u")           'sp-backward-unwrap-sexp)
  (define-key smartparens-mode-map (kbd  "M-t")           'sp-add-to-previous-sexp))

;;**** Expand-region
(use-package expand-region
  :ensure t
  :init
  :bind (
         ("C-@" . er/expand-region)))
;;**** Embrace
(use-package embrace
  :ensure t
  :config (progn
            (bind-key "M-`" #'embrace-commander)
            (add-hook 'org-mode-hook #'embrace-org-mode-hook)))

;; *** Multiple-cursors

(use-package multiple-cursors
  :ensure t
  :init
  :config
  (define-key global-map (kbd  "C-c m p") 'mc/mark-previous-like-this)
  (define-key global-map (kbd  "C-c m n") 'mc/mark-next-like-this)
  (define-key global-map (kbd  "C-c m t") 'mc/mark-all-like-this)
  (define-key global-map (kbd  "C-c m r") 'set-rectangular-region-anchor)
  (define-key global-map (kbd  "C-c m c") 'mc/edit-lines)
  (define-key global-map (kbd  "C-c m e") 'mc/edit-ends-of-lines)
  (define-key global-map (kbd  "C-c m a") 'mc/edit-beginnings-of-lines))


;; *** ZZZ-to-char
(use-package zzz-to-char
  :init
  :config
  (define-key global-map (kbd  "M-z")       'zzz-up-to-char))
;; *** Hungry-Delete
(use-package hungry-delete
  :ensure t
  :init
  :config
  (global-hungry-delete-mode))
;; *** Aggressive Indent
(use-package aggressive-indent
  :init
  :diminish aggressive-indent
  :config
  (aggressive-indent-global-mode 1)
  (add-to-list 'aggressive-indent-excluded-modes '(python-mode
                                                   nix-mode
                                                   haskell-mode))
  (add-to-list
   'aggressive-indent-dont-indent-if
   '(and (derived-mode-p 'python-mode)
         (null (string-match "\\([#\\b\\*{}]\\)"
                             (thing-at-point 'line)))))
  )

;; *** Operating on a Whole Line or a Region
(use-package whole-line-or-region
  :init
  :config
  (whole-line-or-region-mode 1))
(use-package drag-stuff
  :ensure t
  :diminish drag-stuff-mode
  :config (progn
            (defhydra malb/hydra-drag-stuff (:color red)
              "drag stuff"
              ("p" drag-stuff-up "↑")
              ("n" drag-stuff-down "↓")
              ("SPC" nil)
              ("q" nil))
            (bind-key "C-c d" #'malb/hydra-drag-stuff/body)))
;; *** Regexp


(use-package visual-regexp-steroids
  :ensure t)

(use-package visual-regexp
  :ensure t
  :bind (("C-x m" . vr/mc-mark)
         ("M-%" . vr/query-replace)
         ("C-S-s" . vr/isearch-forward)
         ("C-S-r" . vr/isearch-backward)))
;; *** Completion
(use-package company
  :init
  (require 'company)
  (setq company-backends
        '((company-yasnippet
           company-files                ; files & directory
           company-keywords             ; keywords
           company-capf
           )
          (company-abbrev company-dabbrev)
          ))
  (add-hook 'after-init-hook 'global-company-mode))
;; *** Wrapping
(use-package wrap-region
  :init
  :config
  (wrap-region-global-mode +1)
  (wrap-region-add-wrapper "$" "$")
  )
;; *** Speedbar
(use-package sr-speedbar
  :init
  (setq speedbar-use-images nil)
  (defadvice sr-speedbar-toggle (after speedbar-jump activate)
    (other-window 1))
  :bind (("<f11>" . sr-speedbar-toggle))

  )
;;**** Hippie-Expand

(use-package hippie-expand
  :ensure nil
  :init
  (setq hippie-expand-try-functions-list
        '(yas-hippie-try-expand
          try-complete-file-name-partially
          try-complete-file-name
          try-expand-dabbrev
          try-expand-dabbrev-all-buffers
          try-expand-dabbrev-from-kill))
  :bind
  ("M-SPC" . hippie-expand))

;;**** Ivy-Historian
;; Persistent storage of completions
(use-package ivy-historian
  :init
  :config
  (add-hook 'after-init-hook
            (lambda ()
              (ivy-historian-mode)
              (diminish 'historian-mode)
              (diminish 'ivy-historian-mode)))
  )
;; *** Whitespace Clean-up
(use-package clean-aindent-mode
  :init
  (require 'clean-aindent-mode)
  (clean-aindent-mode t))
(use-package dtrt-indent
  :init
  (require 'dtrt-indent)
  (dtrt-indent-mode 1)
  (setq dtrt-indent-verbosity 0))
(use-package ws-butler
  :init
  (require 'ws-butler)
  (add-hook 'c-mode-common-hook 'ws-butler-mode))
;; *** Smart dictionary switching
(use-package auto-dictionary
  :init
  (require 'auto-dictionary)
  (add-hook 'flyspell-mode-hook (lambda () (auto-dictionary-mode 1)))
  )

;; *** Writing Helpers
;;**** Proselint
(flycheck-define-checker proselint
  "A linter for prose."
  :command ("proselint" source-inplace)
  :error-patterns
  ((warning line-start (file-name) ":" line ":" column ": "
            (id (one-or-more (not (any " "))))
            (message) line-end))
  :modes (text-mode markdown-mode gfm-mode org-mode message-mode))

(add-to-list 'flycheck-checkers 'proselint)
;; ** Modes
;; *** Parentheses
(show-paren-mode 1)         ;; highlight parentheses
(setq show-paren-style 'mixed)
;; *** Paragraph
(electric-indent-mode 1)
;; ** Functions and Bindings
;; *** Functions
;;**** Swooping
(defun malb/helm-swoop-pre-fill ()
  (thing-at-point 'symbol)) ;; I’m going back and forth what I prefer

(setq malb/helm-swoop-ignore-major-mode
      '(dired-mode paradox-menu-mode doc-view-mode pdf-view-mode mu4e-headers-mode org-mode markdown-mode latex-mode))

(use-package helm-swoop
  :ensure t
  :bind (("C-c s" . helm-multi-swoop-org)
         ("<f2> s" . helm-multi-swoop-all))
  :config (progn

            (setq helm-swoop-pre-input-function  #'malb/helm-swoop-pre-fill
                  helm-swoop-split-with-multiple-windows nil
                  helm-swoop-split-direction #'split-window-horizontally
                  helm-swoop-split-window-function 'helm-default-display-buffer
                  helm-swoop-speed-or-color t)

            ;; https://emacs.stackexchange.com/questions/28790/helm-swoop-how-to-make-it-behave-more-like-isearch
            (defun malb/helm-swoop-C-s ()
              (interactive)
              (if (boundp 'helm-swoop-pattern)
                  (if (equal helm-swoop-pattern "")
                      (previous-history-element 1)
                    (helm-next-line))
                (helm-next-line)))

            (bind-key "C-S-s" #'helm-swoop-from-isearch isearch-mode-map)
            (bind-key "C-S-s" #'helm-multi-swoop-all-from-helm-swoop helm-swoop-map)
            (bind-key "C-r"   #'helm-previous-line helm-swoop-map)
            (bind-key "C-s"   #'malb/helm-swoop-C-s helm-swoop-map)
            (bind-key "C-r"   #'helm-previous-line helm-multi-swoop-map)
            (bind-key "C-s"   #'malb/helm-swoop-C-s helm-multi-swoop-map)))

;;**** Utilities
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
(defun prettify-paragraph ()
  (interactive)
  (align-current)
  (fill-paragraph))
(defun insert-tab-char ()
  "Insert 4 spaces."
  (interactive)
  (insert "    ")
  )
;; *** Bindings
(define-key global-map (kbd  "S-RET")	'prettify-paragraph)
(define-key global-map (kbd  "RET")	'newline-and-indent)
(define-key global-map (kbd  "C-\.")	'align-regexp)
(define-key global-map (kbd  "<f2> t")    'replace-string)
;; activate whitespace-mode to view all whitespace characters
(define-key global-map (kbd "C-c w") 'whitespace-mode)
(define-key global-map (kbd "<f2> l") 'whitespace-cleanup)
;; show unncessary whitespace that can mess up your diff
(add-hook 'prog-mode-hook (lambda () (interactive) (setq show-trailing-whitespace 1)))
;; make indentation commands use space only (never tab character)
(setq-default indent-tabs-mode nil) ; emacs 23.1, 24.2, default to t
;; if indent-tabs-mode is t, it means it may use tab, resulting mixed space and tab
;; set default tab char's display width to 4 spaces
(setq-default tab-width 4) ; emacs 23.1, 24.2, default to 8
;; make tab key do indent first then completion.
(setq-default tab-always-indent 'complete)

(provide 'smax-editing)
