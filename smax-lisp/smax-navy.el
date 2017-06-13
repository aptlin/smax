;; smax-navy -- Summary: navigation
;;; Commentary:
;;; Code:
;; * Navigation
;; ** Packages
;;*** Ace
(use-package ace-link
  :ensure t
  :init
  :config
  (ace-link-setup-default))
;; *** Avy
(use-package avy
  :bind ("C-\"" . avy-goto-word-1))
;; ***    Deft
(use-package deft
  :ensure t
  :bind ("<f8> <f8>" . malb/deft)
  :bind ("S-<f8>" . malb/blog)
  :init (progn
          (setq deft-extensions '("org" "tex")
                deft-default-extension "org"
                deft-directory "~/ORG"
                deft-text-mode 'org-mode
                deft-recursive t
                deft-use-filename-as-title t
                deft-auto-save-interval 300.0
                deft-use-filter-string-for-filename t
                deft-current-sort-method 'title
                deft-file-naming-rules '((noslash . "-")
                                         (nospace . "-")
                                         (case-fn . downcase)))

          (defun malb/deft-in-dir (dir)
            "Run deft in directory DIR"
            (setq deft-directory dir)
            (switch-to-buffer "*Deft*")
            (kill-this-buffer)
            (deft))

          (defun malb/blog ()
            (interactive)
            (malb/deft-in-dir "~/WERKE/sdll.github.io"))

          (defun malb/deft ()
            (interactive)
            (malb/deft-in-dir "~/ORG"))

          (add-hook 'deft-mode-hook #'hl-line-mode)))
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

(define-key global-map (kbd "<f12>") 'navy)

(provide 'smax-navy)
;;; smax-navy.el ends here
;; ** Modes
;; *** Words
(global-subword-mode  1)

;; ** Functions and Bindings
;; *** Functions
;; **** Search
(define-key global-map (kbd "C-<") 'counsel-ag)
(define-key global-map (kbd "C->") 'helm-locate)
;; **** Buffers

(define-key global-map (kbd "M-v") '(lambda nil (interactive) (condition-case nil
                                                                  (scroll-down) (beginning-of-buffer (goto-char (point-min))))))
(define-key global-map (kbd "C-v") '(lambda nil (interactive) (condition-case nil
                                                                  (scroll-up) (end-of-buffer (goto-char (point-max))))))

;; presentability
(define-key global-map (kbd "C-=") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)
;; **** Smart Other Window
(defvar snwob-starting-window-or-buffer nil)

(defun snwob-single-buffer-p ()
  "Return non-nil if the current frame has one buffer only."
  (null (cdr (buffer-list (selected-frame)))))

(defun snwob-single-frame-p ()
  "Return non-nil if the selected frame is the only one."
  (null (cdr (frame-list))))

(defun snwob--current-window-or-buffer ()
  "Return the current buffer if there is a single window.
Otherwise, return the selected window."
  (if (one-window-p)
	  (current-buffer)
	(selected-window)))

(defun snwob--other-frame-or-window-or-buffer ()
  "Switch to the other frame if there is more than one.
Otherwise, call `snwob--other-window-or-buffer'."
  (if (snwob-single-frame-p)
	  (snwob--other-window-or-buffer)
	(other-frame 1)
	(setq this-command #'other-frame)))

(defun snwob--other-window-or-buffer ()
  "Switch to another window if there is one.
Otherwise, switch to the other buffer."
  (cond ((one-window-p)
         (switch-to-buffer (other-buffer (current-buffer) t (selected-frame))))
        (t
         (other-window 1))))

(defun smart-next-window-or-buffer ()
  "Switch to the other buffer if there is one window only.
Otherwise, switch to another window.  After a full cycle of two
buffers (or as many windows as there are in the selected frame)
switch to another frame."
  (interactive)
  (cond ((eq last-command #'smart-next-window-or-buffer)
         (if (eq snwob-starting-window-or-buffer (snwob--current-window-or-buffer))
             (snwob--other-frame-or-window-or-buffer)
           (snwob--other-window-or-buffer)))
        (t
         (setq snwob-starting-window-or-buffer
               (snwob--current-window-or-buffer))
         (snwob--other-window-or-buffer))))

(global-set-key (kbd "<f2> <f2>") #'smart-next-window-or-buffer)
;; **** Smart C-a
(defun malb/beginning-of-line-dwim ()
  "Toggles between moving point to the first non-whitespace character, and
  the start of the line."
  (interactive)
  (let ((start-position (point)))
    ;; Move to the first non-whitespace character.
    (back-to-indentation)

    ;; If we haven't moved position, go to start of the line.
    (when (= (point) start-position)
      (move-beginning-of-line nil))))


(bind-key "C-a" #'malb/beginning-of-line-dwim)
(bind-key "<home>"  #'malb/beginning-of-line-dwim lisp-mode-map)
;; *** Bindings
(bind-key "<f2> k" #'kill-this-buffer)
