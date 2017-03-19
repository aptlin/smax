;;; smax.el ---

;;; Commentary:
;; 
;; * Basic settings
;; ** Theme and Look 
(load-theme 'leuven)

(setq inhibit-startup-screen t) ;; stop showing startup screen
(tool-bar-mode -1)           ; remove the icons
(menu-bar-mode -1)           ; keep the menus
(global-visual-line-mode 1) ;; how long lines are handled.  This
;; appears to wrap long lines visually,
;; but not add line-returns
(scroll-bar-mode -1)
(global-font-lock-mode t) ;; turn on font-lock mode everywhere

;; I do not like autofill mode.
(auto-fill-mode -1)

(show-paren-mode 1)         ;; highlight parentheses
(setq show-paren-style 'mixed) ;; alternative is 'expression,
;; 'parenthesis or 'mixed


(fset 'yes-or-no-p 'y-or-n-p) ; answer with y/n instead of yes/no

(setq custom-file (expand-file-name "user/custom.el" smax-dir))

(setq auto-save-list-file-prefix (expand-file-name "auto-save-list/saves-" smax-dir))
;; ** Editing and Backups
(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR." t)

(global-set-key (kbd "M-z") 'zap-up-to-char)

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

;; * Navigation

(defvar navy-l 'forward-char
  "The next item in a forward sense.")

(defvar navy-h 'backward-char
  "The previous item in a backward sense.")

(defvar navy-k 'previous-line
  "The previous item in an up sense.")

(defvar navy-j 'next-line
  "The next item in a down sense.")

(defvar navy-semicolon 'avy-goto-char
  "Command bound to ;.")

(defvar navy-quote 'avy-goto-line
  "Command bound to '.")

(defvar navy-comma 'avy-goto-char-2
  "Command bound to ,")

(defvar navy-period 'avy-goto-word-0
  "Command bound to .")

(defvar navy-slash 'end-of-visual-line
  "The end of an item.")

(defvar navy-mode "char"
  "The active mode.")


(defhydra navy (:color red :hint nil) 
  "
%s(format \"%s-mode\" navy-mode)
%s(make-string (length (symbol-name navy-h)) ? )     _j_: %`navy-j
%`navy-h :_h_     _l_: %`navy-l     _;_: %`navy-semicolon  _'_: %`navy-quote
%s(make-string (length (symbol-name navy-h)) ? )     _k_: %`navy-k
  _,_: %`navy-comma _._: %`navy-period _/_: %`navy-slash
  point-min: _<_    _>_: point-max     
   
"
  ("h" (funcall navy-h))
  ("l" (funcall navy-l))
  ("j" (funcall navy-j))
  ("k" (funcall navy-k))

  ("q" nil "quit" :color blue)

  ("j" (call-interactively navy-j))
  
  (";" (call-interactively navy-semicolon))
  ("'" (call-interactively navy-quote))

  ("," (call-interactively navy-comma))
  ("." (call-interactively navy-period))
  ("/" (call-interactively navy-slash))

  ("<" beginning-of-buffer)
  (">" end-of-buffer)
  ;; these are different modes
  ;; char

  ("c" (lambda ()
	 (interactive)
	 (setq navy-mode "char"
	       navy-h 'backward-char
	       navy-k 'previous-line
	       navy-l 'forward-char 
	       navy-j 'next-line
	       navy-semicolon 'avy-goto-char-2
	       navy-quote 'avy-goto-line
	       navy-comma 'avy-goto-char-in-line
	       navy-period 'avy-goto-word-1))
   "char mode")

  ("w" (lambda ()
	 (interactive)
	 (setq navy-mode "word"
	       navy-h 'backward-word
	       navy-k 'previous-line
	       navy-l 'forward-word 
	       navy-j 'next-
	       navy-semicolon 'avy-goto-char-2
	       navy-quote 'avy-goto-line
	       navy-comma 'avy-goto-word-1 
	       navy-period 'avy-goto-word-or-subword-1))
   "word mode")
  
  ("s" (lambda ()
	 (interactive)
	 (setq navy-mode "sentence"
	       navy-h 'backward-sentence
	       navy-k 'previous-line
	       navy-j 'next-line
	       navy-l 'forward-sentence
	       navy-semicolon 'avy-goto-char-2
	       navy-quote 'avy-goto-line
	       navy-comma 'avy-goto-word-1 
	       navy-period 'avy-goto-word-or-subword-1))
   "sentence mode")

  ("p" (lambda ()
	 (interactive)
	 (setq navy-mode "paragraph"
	       navy-h 'backward-paragraph
	       navy-l 'forward-paragraph
	       navy-k 'previous-line
	       navy-j 'next-line 
	       navy-semicolon 'avy-goto-char-2
	       navy-quote 'avy-goto-line
	       navy-comma 'avy-goto-word-1 
	       navy-period 'avy-goto-word-or-subword-1))
   "paragraph mode")

  ("g" (lambda ()
	 (interactive)
	 (setq navy-mode "page"
	       navy-h 'backward-page
	       navy-l 'forward-page
	       navy-k 'backward-page
	       navy-j 'forward-page 
	       navy-semicolon 'avy-goto-char-2
	       navy-quote 'avy-goto-line
	       navy-comma 'avy-goto-word-1 
	       navy-period 'avy-goto-word-or-subword-1))
   "page mode")

  ("n" (lambda ()
	 (interactive)
	 (setq navy-mode "line"
	       navy-k 'avy-goto-line-above
	       navy-j 'avy-goto-line-below
	       navy-l 'next-line
	       navy-h 'previous-line 
	       navy-semicolon 'avy-goto-char-2
	       navy-quote 'avy-goto-line
	       navy-comma 'avy-goto-word-1 
	       navy-period 'avy-goto-word-or-subword-1))
   "line mode")

  ("x" (lambda ()
	 (interactive)
	 (setq navy-mode "sexp"
	       navy-h 'backward-sexp
	       navy-l 'forward-sexp
	       navy-k 'previous-line
	       navy-j 'next-line
	       navy-semicolon 'avy-goto-char-2
	       navy-quote 'avy-goto-line
	       navy-comma 'lispy-ace-symbol
	       navy-period 'lispy-ace-paren)) 
   "sexp mode")

  ("a" swiper-all "swiper-all")
  ("o" helm-org-agenda-files-headings "org headlines")
  ("r" counsel-git-grep "git grep")
  ("t" avy-goto-char-timer "char timer"))


(defun navy ()
  "Run the `navy/body' hydra."
  (interactive)
  (setq navy-mode "char"
	navy-h 'backward-char
	navy-k 'previous-line
	navy-l 'forward-char 
	navy-j 'next-line
	navy-quote 'avy-goto-line
	navy-comma 'avy-goto-char-2
	navy-period 'avy-goto-char-in-line
	navy-semicolon 'avy-goto-char)
  (navy/body))

;;  I mapped Capslock to f12
(global-set-key (kbd "<f12>") 'navy)


;; * The end
(provide 'smax)

;;; smax.el ends here
