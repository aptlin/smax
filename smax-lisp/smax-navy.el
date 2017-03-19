;; smax-navy -- Summary: navigation
;;; Commentary:
;;; Code:
;; * Navigation
;; ** Packages
(use-package ace-link
  :ensure t
  :init
  :config
  (ace-link-setup-default))

(use-package avy
  :bind ("M-'" . avy-goto-word-1))

;; ** Navy Toolbar
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

(global-set-key (kbd "<f12>") 'navy)

(provide 'smax-navy)
;;; smax-navy.el ends here
;; ** Modes
;; *** Words
(global-subword-mode  1)

;; ** Functions and Bindings
;; *** Functions
;; **** Search
(global-set-key (kbd "C-<") 'counsel-ag)
(global-set-key (kbd "C->") 'helm-locate)
;; **** Buffers

(global-set-key (kbd "M-v") '(lambda nil (interactive) (condition-case nil
							   (scroll-down) (beginning-of-buffer (goto-char (point-min))))))
(global-set-key (kbd "C-v") '(lambda nil (interactive) (condition-case nil
							   (scroll-up) (end-of-buffer (goto-char (point-max))))))
;; presentability
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
