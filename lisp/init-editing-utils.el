;; add extended utf-8 support
(require-package 'xah-math-input)
(global-xah-math-input-mode 1)
;; add visual bookmarks
(require-package 'bm)

;; add extended utf-8 input support

(global-set-key (kbd "<C-f2>") 'bm-toggle)
(global-set-key (kbd "<f2> n") 'bm-next)
(global-set-key (kbd "<S-f2>") 'bm-previous)

;; LIFO order
(setq bm-in-lifo-order t)

;; include all open buffers in the ring
(setq bm-cycle-all-buffers t)

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
;; extend parenthesis editing

(require-package 'unfill)

(when (fboundp 'electric-pair-mode)
  (electric-pair-mode))
(when (eval-when-compile (version< "24.4" emacs-version))
  (electric-indent-mode 1))
(add-hook 'LaTeX-mode-hook
          (electric-pair-mode 0)) 
;;----------------------------------------------------------------------------
;; Some basic preferences
;;----------------------------------------------------------------------------
(setq-default
 blink-cursor-interval 0.4
 bookmark-default-file (expand-file-name ".bookmarks.el" user-emacs-directory)
 buffers-menu-max-size 30
 case-fold-search t
 column-number-mode t
 delete-selection-mode t
 ediff-split-window-function 'split-window-horizontally
 ediff-window-setup-function 'ediff-setup-windows-plain
 indent-tabs-mode nil
 make-backup-files nil
 mouse-yank-at-point t
 save-interprogram-paste-before-kill t
 scroll-preserve-screen-position 'always
 set-mark-command-repeat-pop t
 tooltip-delay 1.5
 truncate-lines nil
 truncate-partial-width-windows nil)

(global-auto-revert-mode)
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)

(transient-mark-mode t)



;;; Newline behaviour

(global-set-key (kbd "RET") 'newline-and-indent)
(defun sanityinc/newline-at-end-of-line ()
  "Move to end of line, enter a newline, and reindent."
  (interactive)
  (move-end-of-line 1)
  (newline-and-indent))

(global-set-key (kbd "S-<return>") 'sanityinc/newline-at-end-of-line)



(when (eval-when-compile (string< "24.3.1" emacs-version))
  ;; https://github.com/purcell/emacs.d/issues/138
  (after-load 'subword
    (diminish 'subword-mode)))



(when (maybe-require-package 'indent-guide)
  (add-hook 'prog-mode-hook 'indent-guide-mode)
  (after-load 'indent-guide
    (diminish 'indent-guide-mode)))



(require-package 'nlinum)


(when (require-package 'rainbow-delimiters)
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))



(when (fboundp 'global-prettify-symbols-mode)
  (global-prettify-symbols-mode))


(require-package 'undo-tree)
(global-undo-tree-mode)
(diminish 'undo-tree-mode)

(require-package 'highlight-symbol)
(dolist (hook '(prog-mode-hook html-mode-hook css-mode-hook))
  (add-hook hook 'highlight-symbol-mode)
  (add-hook hook 'highlight-symbol-nav-mode))
(add-hook 'org-mode-hook 'highlight-symbol-nav-mode)
(after-load 'highlight-symbol
            (diminish 'highlight-symbol-mode)
            (defadvice highlight-symbol-temp-highlight (around sanityinc/maybe-suppress activate)
              "Suppress symbol highlighting while isearching."
              (unless (or isearch-mode
                          (and (boundp 'multiple-cursors-mode) multiple-cursors-mode))
                ad-do-it)))

;;----------------------------------------------------------------------------
;; Zap *up* to char is a handy pair for zap-to-char
;;----------------------------------------------------------------------------
(autoload 'zap-up-to-char "misc" "Kill up to, but not including ARGth occurrence of CHAR.")
(global-set-key (kbd "M-Z") 'zap-up-to-char)



(require-package 'browse-kill-ring)
(setq browse-kill-ring-separator "\f")
(global-set-key (kbd "M-Y") 'browse-kill-ring)
(after-load 'browse-kill-ring
  (define-key browse-kill-ring-mode-map (kbd "C-g") 'browse-kill-ring-quit)
  (define-key browse-kill-ring-mode-map (kbd "M-n") 'browse-kill-ring-forward)
  (define-key browse-kill-ring-mode-map (kbd "M-p") 'browse-kill-ring-previous))
(after-load 'page-break-lines
  (push 'browse-kill-ring-mode page-break-lines-modes))


;;----------------------------------------------------------------------------
;; Don't disable narrowing commands
;;----------------------------------------------------------------------------
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

;;----------------------------------------------------------------------------
;; Show matching parens
;;----------------------------------------------------------------------------
(show-paren-mode 1)
;;----------------------------------------------------------------------------
;; Expand region
;;----------------------------------------------------------------------------
(require-package 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)


;;----------------------------------------------------------------------------
;; Don't disable case-change functions
;;----------------------------------------------------------------------------
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)


;;----------------------------------------------------------------------------
;; Rectangle selections, and overwrite text when the selection is active
;;----------------------------------------------------------------------------
(cua-selection-mode t)                  ; for rectangles, CUA is nice


;;----------------------------------------------------------------------------
;; Faster shortcuts
;;----------------------------------------------------------------------------

(require-package 'key-chord)
(key-chord-mode 1)
(key-chord-define-global "fg" 'iy-go-to-char)
(key-chord-define-global "df" 'iy-go-to-char-backward)

;;----------------------------------------------------------------------------
;; Handy key bindings
;;----------------------------------------------------------------------------

(global-set-key (kbd "M-j")
                (lambda ()
                  (interactive)
                  (join-line -1)))

(require-package 'ace-jump-mode)
(define-key global-map (kbd "C-#") 'ace-jump-mode)

(require-package 'expand-region)
(global-set-key (kbd "C-@") 'er/expand-region)
(global-set-key (kbd "C-<") 'grep-find)
(global-set-key (kbd "C->") 'helm-locate)
(global-set-key (kbd "C-!") 'save-buffers-kill-emacs)

(global-set-key (kbd "C-.") 'set-mark-command)
(global-set-key (kbd "C-x C-.") 'pop-global-mark)

(when (maybe-require-package 'avy)
  (global-set-key (kbd "C-;") 'avy-goto-word-or-subword-1))

(require-package 'multiple-cursors)
;; multiple-cursors
(global-set-key (kbd "C-c m p") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c m n") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c m t") 'mc/mark-all-like-this)
;; From active region to multiple cursors:
(global-set-key (kbd "C-c m r") 'set-rectangular-region-anchor)
(global-set-key (kbd "C-c m c") 'mc/edit-lines)
(global-set-key (kbd "C-c m e") 'mc/edit-ends-of-lines)
(global-set-key (kbd "C-c m a") 'mc/edit-beginnings-of-lines)

;; quicken navigation

(require-package 'iy-go-to-char)

(global-set-key (kbd "C-c g") 'iy-go-to-char)
(global-set-key (kbd "C-c G") 'iy-go-to-char-backward)
(global-set-key (kbd "C-c ;") 'iy-go-to-or-up-to-continue)
(global-set-key (kbd "C-c ,") 'iy-go-to-or-up-to-continue-backward)

;; Train myself to use M-f and M-b instead
(global-unset-key [M-left])
(global-unset-key [M-right])


;; Move between windows
(require-package 'windmove)
(global-set-key (kbd "<f2> <right>")  'windmove-right)
(global-set-key (kbd "<f2> <left>" )  'windmove-left)
(global-set-key (kbd "<f2> <up>"   )  'windmove-up)
(global-set-key (kbd "<f2> <down>" )  'windmove-down)


(defun kill-back-to-indentation ()
  "Kill from point back to the first non-whitespace character on the line."
  (interactive)
  (let ((prev-pos (point)))
    (back-to-indentation)
    (kill-region (point) prev-pos)))

(global-set-key (kbd "C-M-<backspace>") 'kill-back-to-indentation)


;;----------------------------------------------------------------------------
;; Page break lines
;;----------------------------------------------------------------------------
(require-package 'page-break-lines)
(global-page-break-lines-mode)
(diminish 'page-break-lines-mode)

;;----------------------------------------------------------------------------
;; Shift lines up and down with M-up and M-down. When paredit is enabled,
;; it will use those keybindings. For this reason, you might prefer to
;; use M-S-up and M-S-down, which will work even in lisp modes.
;;----------------------------------------------------------------------------
(require-package 'move-dup)
(global-set-key [M-up] 'md/move-lines-up)
(global-set-key [M-down] 'md/move-lines-down)
(global-set-key [M-S-up] 'md/move-lines-up)
(global-set-key [M-S-down] 'md/move-lines-down)

(global-set-key (kbd "C-c d") 'md/duplicate-down)
(global-set-key (kbd "C-c D") 'md/duplicate-up)

;;----------------------------------------------------------------------------
;; Fix backward-up-list to understand quotes, see http://bit.ly/h7mdIL
;;----------------------------------------------------------------------------
(defun backward-up-sexp (arg)
  "Jump up to the start of the ARG'th enclosing sexp."
  (interactive "p")
  (let ((ppss (syntax-ppss)))
    (cond ((elt ppss 3)
           (goto-char (elt ppss 8))
           (backward-up-sexp (1- arg)))
          ((backward-up-list arg)))))

(global-set-key [remap backward-up-list] 'backward-up-sexp) ; C-M-u, C-M-up


;;----------------------------------------------------------------------------
;; Cut/copy the current line if no region is active
;;----------------------------------------------------------------------------
(require-package 'whole-line-or-region)
(whole-line-or-region-mode t)
(diminish 'whole-line-or-region-mode)
(make-variable-buffer-local 'whole-line-or-region-mode)

(defun suspend-mode-during-cua-rect-selection (mode-name)
  "Add an advice to suspend `MODE-NAME' while selecting a CUA rectangle."
  (let ((flagvar (intern (format "%s-was-active-before-cua-rectangle" mode-name)))
        (advice-name (intern (format "suspend-%s" mode-name))))
    (eval-after-load 'cua-rect
      `(progn
         (defvar ,flagvar nil)
         (make-variable-buffer-local ',flagvar)
         (defadvice cua--activate-rectangle (after ,advice-name activate)
           (setq ,flagvar (and (boundp ',mode-name) ,mode-name))
           (when ,flagvar
             (,mode-name 0)))
         (defadvice cua--deactivate-rectangle (after ,advice-name activate)
           (when ,flagvar
             (,mode-name 1)))))))

(suspend-mode-during-cua-rect-selection 'whole-line-or-region-mode)



(defun sanityinc/open-line-with-reindent (n)
  "A version of `open-line' which reindents the start and end positions.
If there is a fill prefix and/or a `left-margin', insert them
on the new line if the line would have been blank.
With arg N, insert N newlines."
  (interactive "*p")
  (let* ((do-fill-prefix (and fill-prefix (bolp)))
	 (do-left-margin (and (bolp) (> (current-left-margin) 0)))
	 (loc (point-marker))
	 ;; Don't expand an abbrev before point.
	 (abbrev-mode nil))
    (delete-horizontal-space t)
    (newline n)
    (indent-according-to-mode)
    (when (eolp)
      (delete-horizontal-space t))
    (goto-char loc)
    (while (> n 0)
      (cond ((bolp)
	     (if do-left-margin (indent-to (current-left-margin)))
	     (if do-fill-prefix (insert-and-inherit fill-prefix))))
      (forward-line 1)
      (setq n (1- n)))
    (goto-char loc)
    (end-of-line)
    (indent-according-to-mode)))

(global-set-key (kbd "C-o") 'sanityinc/open-line-with-reindent)


;;----------------------------------------------------------------------------
;; Random line sorting
;;----------------------------------------------------------------------------
(defun sort-lines-random (beg end)
  "Sort lines in region randomly."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (let ;; To make `end-of-line' and etc. to ignore fields.
          ((inhibit-field-text-motion t))
        (sort-subr nil 'forward-line 'end-of-line nil nil
                   (lambda (s1 s2) (eq (random 2) 0)))))))




(require-package 'highlight-escape-sequences)
(hes-mode)


(require-package 'guide-key)
(setq guide-key/guide-key-sequence '("C-x" "C-c" "C-x 4" "C-x 5" "C-c ;" "C-c ; f" "C-c ' f" "C-x n" "C-x C-r" "C-x r" "M-s" "C-h"))
(add-hook 'after-init-hook
          (lambda ()
            (guide-key-mode 1)
            (diminish 'guide-key-mode)))

;; AUCTeX
;; (load "auctex.el" nil t t)
;; (load "preview-latex.el" nil t t)
;; (require-package 'auto-complete)
;; (require 'auto-complete-config)
;; (ac-config-default)
;; (require 'ac-math)
;; (add-to-list 'ac-modes 'latex-mode)   ; make auto-complete aware of `latex-mode`

;; (defun ac-LaTeX-mode-setup () ; add ac-sources to default ac-sources
;;   (setq ac-sources
;;         (append '(ac-source-math-unicode ac-source-math-latex ac-source-latex-commands)
;;                 ac-sources)))
;; (add-hook 'LaTeX-mode-hook 'ac-LaTeX-mode-setup)
;; (setq ac-math-unicode-in-math-p nil)
;; (global-auto-complete-mode t)

;; (require-package 'auto-complete-auctex)



;;-------------------------------------------------------------------------------

;;Show the column line

;;-------------------------------------------------------------------------------

(require-package 'fill-column-indicator)

(define-globalized-minor-mode global-fci-mode fci-mode (lambda () (fci-mode 1)))
(global-fci-mode 1)
(setq fci-rule-column 80)

;; ----------------------------------------------------------------------------
;; Yasnippet
;; ----------------------------------------------------------------------------


(add-to-list 'load-path
             "~/.emacs.d/plugins/yasnippet"
             "~/.emacs.d/snippets/")
(require-package 'yasnippet)
(yas-global-mode 1)
(yas-load-directory "~/.emacs.d/snippets/")

;; ----------------------------------------------------------------------------
;; More comfortable new line
;; ----------------------------------------------------------------------------

(defun end-of-line-and-indented-new-line ()
  (interactive)
  (end-of-line)
  (while (< count 3)
    (newline-and-indent)
    (setq count (1+ count))))

(global-set-key (kbd "<S-return>") 'end-of-line-and-indented-new-line)
;;from https://emacs.stackexchange.com/questions/4089/can-i-configure-eww-to-use-pdf-view-mode-from-pdf-tools-for-pdfs-instead-of-do
;; (defvar tv/prefer-pdf-tools (fboundp 'pdf-view-mode))
;; (defun tv/start-pdf-tools-if-pdf ()
;;   (when (and tv/prefer-pdf-tools
;;              (eq doc-view-doc-type 'pdf))
;;     (pdf-view-mode)))

;; (add-hook 'doc-view-mode-hook 'tv/start-pdf-tools-if-pdf)

;; (setq tv/prefer-pdf-tools t)

;; autoindentation

(dolist (command '(yank yank-pop))
  (eval `(defadvice ,command (after indent-region activate)
           (and (not current-prefix-arg)
                (member major-mode '(emacs-lisp-mode lisp-mode
                                                     clojure-mode    scheme-mode
                                                     haskell-mode    ruby-mode
                                                     rspec-mode      python-mode
                                                     c-mode          c++-mode
                                                     objc-mode       latex-mode
                                                     plain-tex-mode))
                (let ((mark-even-if-inactive transient-mark-mode))
                  (indent-region (region-beginning) (region-end) nil))))))

(provide 'init-editing-utils)
