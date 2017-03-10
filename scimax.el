;;; scimax.el ---

;;; Commentary:
;; 
;; * Basic settings
(load-theme 'leuven)

(setq inhibit-startup-screen t) ;; stop showing startup screen
(tool-bar-mode -1)           ; remove the icons
(menu-bar-mode -1)           ; keep the menus
(global-visual-line-mode 1) ;; how long lines are handled.  This
;; appears to wrap long lines visually,
;; but not add line-returns

(set-scroll-bar-mode nil)
(setq scroll-error-top-bottom 'true)

(global-font-lock-mode t) ;; turn on font-lock mode everywhere

;; I do not like autofill mode.
(auto-fill-mode -1)

(show-paren-mode 1)         ;; highlight parentheses
(setq show-paren-style 'mixed) ;; alternative is 'expression,
			       ;; 'parenthesis or 'mixed

(setq backup-inhibited t)  ;; disable backup file creation

(fset 'yes-or-no-p 'y-or-n-p) ; answer with y/n instead of yes/no

(setq custom-file (expand-file-name "user/custom.el" scimax-dir))

(setq auto-save-list-file-prefix (expand-file-name "auto-save-list/saves-" scimax-dir))

;; abbrevs
(setq abbrev-file-name (expand-file-name "user/abbrev_defs" scimax-dir))
(setq save-abbrevs t)
(setq-default abbrev-mode t)
(setq save-abbrevs 'silently)

;; * Version control
;; Disable all version control. makes startup and opening files much faster
;; except git and svn which I actually use
(setq vc-handled-backends '(Git SVN))

(defun scimax-update ()
  "Update scimax from github."
  (interactive)
  (let ((default-directory scimax-dir))
    (when (not (string= "" (shell-command-to-string "git status --porcelain")))
      (shell-command "git add *")
      (shell-command "git commit -am \"commiting scimax.\""))
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
;; ** debugging
(add-hook 'edebug-mode-hook
	  (lambda ()
	    (define-key edebug-mode-map (kbd "h") 'edebug-goto-here)))

;; ** Emacs lisp
;; Setup pretty outlines in Emacs-lisp code
"^;; \\(\\*+.*\\)$"
(defconst lel-font-lock-keywords
  '(("^;; ?\\(\\* .*\\)$" 1 'org-level-1 prepend)
    ("^;; ?\\(\\*\\* .*\\)$" 1 'org-level-2 prepend)
    ("^;; ?\\(\\*\\*\\* .*\\)$" 1 'org-level-3 prepend)
    ("^;; ?\\(\\*\\*\\*\\* .*\\)$" 1 'org-level-4 prepend)
    ("^;; ?\\(\\*\\*\\*\\*\\* .*\\)$" 1 'org-level-5 prepend)
    ;; (lel-outline-comment-highlight 1 'default prepend)
    ("`\\([^\n']+\\)'" 1 font-lock-constant-face prepend)))

(font-lock-add-keywords 'emacs-lisp-mode lel-font-lock-keywords)

(defun lisp-outline-setup ()
  "Setup outline and orgstruct mode for emacs-lisp code.
This enables you to use tab to open and close outlines."
  (setq-local outline-regexp ";; ?\\*+\\|\\`")
  (setq-local orgstruct-heading-prefix-regexp ";; ?\\*+\\|\\`")
  (outline-minor-mode)
  (orgstruct-mode)
  (outline-show-branches))

(add-hook 'emacs-lisp-mode-hook
	  #'lisp-outline-setup)

;; ** Python
(setq python-indent-offset 4)

;; This eliminates an annoying message about the interpreter not using
;; readline. That doesn't seem to matter at all.
(setq warning-suppress-types '((python)
			       (emacs)))

;; *** Outlines in python code
(defconst lpy-font-lock-keywords
  '(("^# \\(\\* .*\\)$" 1 'org-level-1 prepend)
    ("^# \\(\\*\\* .*\\)$" 1 'org-level-2 prepend)
    ("^# \\(\\*\\*\\* .*\\)$" 1 'org-level-3 prepend)
    ("^# \\(\\*\\*\\*\\* .*\\)$" 1 'org-level-4 prepend)
    ("^# \\(\\*\\*\\*\\*\\* .*\\)$" 1 'org-level-5 prepend)
    (lpy-outline-comment-highlight 1 'default prepend)
    ("`\\([^\n']+\\)'" 1 font-lock-constant-face prepend)))

(font-lock-add-keywords 'python-mode lpy-font-lock-keywords)

(defun lpy-outline-comment-highlight (limit)
  (while (re-search-forward "^# \\(?:[^*]\\|$\\)" limit t)
    (let* ((pt (point))
	   (success (save-excursion
		      (and (re-search-backward "^# \\*" nil t)
			   (null (re-search-forward "^[^#]" pt t))))))
      (when success
	(set-match-data (list (line-beginning-position) (line-end-position)
			      (point) (line-end-position)))
	(end-of-line)
	t))))

(add-hook 'python-mode-hook
	  (lambda ()
	    (setq outline-regexp "# \\*+"
		  orgstruct-heading-prefix-regexp "# ?\\*+\\|\\`")
	    (orgstruct-mode)
	    (org-global-cycle 3)))

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


;; * dired enhancements
;; http://ergoemacs.org/emacs/elisp_dired_rename_space_to_underscore.html
(require 'dired )

(defun xah-dired-rename-space-to-underscore ()
  "In dired, rename current or marked files by replacing space to underscore _.
If not in `dired', do nothing.
URL `http://ergoemacs.org/emacs/elisp_dired_rename_space_to_underscore.html'
Version 2016-12-22"
  (interactive)
  (require 'dired-aux)
  (if (equal major-mode 'dired-mode)
      (progn
        (mapc (lambda (x)
                (when (string-match " " x )
                  (dired-rename-file x (replace-regexp-in-string " " "_" x) nil)
                  ))
              (dired-get-marked-files ))
        (revert-buffer)
        (forward-line ))
    (user-error "Not in dired")))


(defun xah-dired-rename-space-to-hyphen ()
  "In dired, rename current or marked files by replacing space to hyphen -.
If not in `dired', do nothing.
URL `http://ergoemacs.org/emacs/elisp_dired_rename_space_to_underscore.html'
Version 2016-12-22"
  (interactive)
  (require 'dired-aux)
  (if (equal major-mode 'dired-mode)
      (progn
        (mapc (lambda (x)
                (when (string-match " " x )
                  (dired-rename-file x (replace-regexp-in-string " " "_" x) nil)))
              (dired-get-marked-files ))
        (revert-buffer))
    (user-error "Not in dired")))

(define-key dired-mode-map (kbd "_") 'xah-dired-rename-space-to-underscore)
(define-key dired-mode-map (kbd "-") 'xah-dired-rename-space-to-hyphen)

;;** MIME

(use-package openwith
  :ensure t
  :init
  :config
  (when (require 'openwith nil 'noerror)
    (setq openwith-associations
	  (list
	   (list (openwith-make-extension-regexp
		  '("mpg" "mpeg" "mp3" "mp4"
		    "avi" "wmv" "wav" "mov" "flv"
		    "ogm" "ogg" "mkv"))
		 "vlc"
		 '(file))
	   (list (openwith-make-extension-regexp
		  '("xbm" "pbm" "pgm" "ppm" "pnm"
		    "png" "gif" "bmp" "tif" "jpeg" "jpg"))
		 "eog"
		 '(file))
	   (list (openwith-make-extension-regexp
		  '("doc" "xls" "ppt" "odt" "ods" "odg" "odp"))
		 "libreoffice"
		 '(file))
	   '("\\.lyx" "lyx" (file))
	   '("\\.chm" "kchmviewer" (file))
	   (list (openwith-make-extension-regexp
		  '("pdf" "ps" "ps.gz" "dvi" "djvu"))
		 "evince"
		 '(file))
	   )))

  (openwith-mode 1)
  (add-to-list  'mm-inhibit-file-name-handlers 'openwith-file-handler)
  )
;; * The end
(provide 'scimax)

;;; scimax.el ends here
