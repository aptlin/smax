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
;; *** Completion
;; *** Wrapping
(use-package wrap-region
  :init
  :config
  (wrap-region-global-mode +1)
  (wrap-region-add-wrapper "$" "$")
  )
;; **** Hippie-Expand

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

;; **** Ivy-Historian
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
;; ** Modes
;; *** Parentheses
(show-paren-mode 1)         ;; highlight parentheses
(setq show-paren-style 'mixed)
;; *** Paragraph
(electric-indent-mode 1)
;; ** Functions and Bindings
;; *** Functions

;; #############################################################################
;;; Thank you, Mark. https://github.com/mrkkrp
(defun mk-saturated-occurence (&optional after-space)
  "Return position of first non-white space character after point.
  If AFTER-SPACE is not NIL, require at least one space character
  before target non-white space character."
  (save-excursion
    (let ((this-end (line-end-position)))
      (if (re-search-forward
           (concat (when after-space "[[:blank:]]")
                   "[^[:blank:]]")
           this-end			; don't go after this position
           t)				; don't error
          (1- (point))
        this-end))))  

(defun mk-column-at (point)
  "Return column number at POINT."
  (save-excursion
    (goto-char point)
    (current-column)))

(defun mk-smart-indent (&optional arg)
  "Align first non-white space char after point with content of previous line.

   With prefix argument ARG, align to next line instead."
  
  (interactive "P")
  (let* ((this-edge (mk-column-at (mk-saturated-occurence)))
         (that-edge
          (save-excursion
            (forward-line (if arg 1 -1))
            (move-to-column this-edge)
            (mk-column-at (mk-saturated-occurence t)))))
    (when (> that-edge this-edge)
      (insert-char 32 (- that-edge this-edge))
      (move-to-column that-edge))))
(define-key global-map (kbd  "C-S-r") 'mk-smart-indent)

(defun mk-transpose-line-down (&optional arg)
  "Move current line and cursor down.

Argument ARG, if supplied, specifies how many times the operation
should be performed."
  (interactive "p")
  (dotimes (_ (or arg 1))
    (let ((col (current-column)))
      (forward-line    1)
      (transpose-lines 1)
      (forward-line   -1)
      (move-to-column col))))

(defun mk-transpose-line-up (&optional arg)
  "Move current line and cursor up.

   Argument ARG, if supplied, specifies how many times the operation
   should be performed."
  (interactive "p")
  (dotimes (_ (or arg 1))
    (let ((col (current-column)))
      (transpose-lines 1)
      (forward-line   -2)
      (move-to-column col))))
(defun mk-show-date (&optional stamp)
  "Show current date in the minibuffer.

If STAMP is not NIL, insert date at point."
  (interactive)
  (funcall (if stamp 'insert 'message)
           (format-time-string "%A, %e %B, %Y")))
(defun mk-grab-input (prompt &optional initial-input add-space)
  "Grab input from user.

If there is an active region, use its contents, otherwise read
text from the minibuffer.  PROMPT is a prompt to show,
INITIAL-INPUT is the initial input.  If INITIAL-INPUT and
ADD-SPACE are not NIL, add one space after the initial input."
  (if mark-active
      (buffer-substring (region-beginning)
                        (region-end))
    (read-string prompt
                 (concat initial-input
                         (when (and initial-input add-space) " ")))))

(defun mk-show-default-dir ()
  "Show default directory in the minibuffer."
  (interactive)
  (message (f-full default-directory)))

(defun mk-file-name-to-kill-ring (arg)
  "Put name of file into kill ring.

   If user's visiting a buffer that's associated with a file, use
   name of the file.  If major mode is ‘dired-mode’, use name of
   file at point, but if point is not placed at any file, put name
   of actual directory into kill ring.  Argument ARG, if given,
   makes result string be quoted as for yanking into shell."
  (interactive "P")
  (let ((φ (if (cl-find major-mode
                        '(dired-mode wdired-mode))
               (or (dired-get-filename nil t)
                   default-directory)
	     (buffer-file-name))))
    (when φ
      (message "%s → kill ring"
               (kill-new
                (expand-file-name
                 (if arg
                     (shell-quote-argument φ)
                   φ)))))))

(defvar mk-search-prefix nil
  "This is an alist that contains some prefixes for online search query.

  Prefixes are picked up according to currect major mode.")

(defun mk-search (what)
  "Search Internet for WHAT thing, with DuckDuckGo.

   When called interactively, it uses prefix corresponding to
   current major mode, as specified in ‘mk-search-prefix’."
  (interactive
   (list (mk-grab-input "DuckDuckGo: "
                        (cdr (assoc major-mode
                                    mk-search-prefix))
                        t)))
  (browse-url
   (concat "https://duckduckgo.com/html/?k1=-1&q="
           (url-hexify-string what))))
(define-key global-map (kbd  "C-c s") 'mk-search)
;; #############################################################################

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
(define-key global-map (kbd  "S-SPC")	'insert-tab-char)
(define-key global-map (kbd  "RET")	'newline-and-indent)
(define-key global-map (kbd  "C-\.")	'align-regexp)
(define-key global-map (kbd  "<f2> t")    'replace-string)

;; make indentation commands use space only (never tab character)
(setq-default indent-tabs-mode nil) ; emacs 23.1, 24.2, default to t
;; if indent-tabs-mode is t, it means it may use tab, resulting mixed space and tab
;; set default tab char's display width to 4 spaces
(setq-default tab-width 4) ; emacs 23.1, 24.2, default to 8
;; make tab key do indent first then completion.
(setq-default tab-always-indent 'complete)

(provide 'smax-editing)
