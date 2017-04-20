;; smax-latex.el --- Utilities to check the LaTeX setup


;;; Commentary:
;; 

;;; Code:
;; * LaTeX
;; ** Variables
(setq smax-bibnotes "~/ORG/bibnotes.org")
(setq smax-references "~/ORG/references.bib")
(setq cdlatex-paired-parens "$[{(")
;; ** Default Behaviour
;;(load "auctex.el" nil t t)
;;(load "preview-latex.el" nil t t)
(setq TeX-parse-self t); Enable parse on load.
(setq TeX-auto-save t); Enable parse on save.
;;(setq-default TeX-master nil)
;;(setq-default TeX-master "master") ;
(setq TeX-PDF-mode t); PDF mode (rather than DVI-mode)
(defun reader-hook()
  (add-to-list 'TeX-view-program-list '("Zathura" "zathura %o"))
  (setq TeX-view-program-selection '((output-pdf "Zathura"))) 
  )
(add-hook 'LaTeX-mode-hook 'reader-hook)

;; ** Packages
;; *** cdLaTeX
(use-package cdlatex
  :ensure t
  :diminish org-cdlatex-mode
  :config
  (progn (add-hook 'LaTeX-mode-hook 'turn-on-cdlatex) ; with AUCTeX LaTeX mode
	 (add-hook 'org-mode-hook 'turn-on-org-cdlatex)

	 ;; LaTeX-math-mode http://www.gnu.org/s/auctex/manual/auctex/Mathematics.html
	 (add-hook 'TeX-mode-hook 'LaTeX-math-mode)

	 ;; Auto-completion


	 ;; make math mode easier to enter


	 ;; (add-hook 'plain-TeX-mode-hook
	 ;; 	   (lambda () (set (make-variable-buffer-local 'TeX-electric-math)
	 ;; 			   (cons "$" "$"))))
	 ;; (add-hook 'LaTeX-mode-hook
	 ;; 	   (lambda () (set (make-variable-buffer-local 'TeX-electric-math)
	 ;; 			   (cons "\\(" ""))))


	 (setq TeX-electric-sub-and-superscript t)

	 ;; (setq LaTeX-electric-left-right-brace nil)
	 ))
;; *** RefTeX
(add-hook 'TeX-mode-hook 'turn-on-reftex)
(add-hook 'org-mode-hook 'turn-on-reftex)
(eval-after-load 'reftex-vars; Is this construct really needed?
  '(progn
     (setq reftex-cite-prompt-optional-args t); Prompt for empty optional arguments in cite macros.
     ;; Make RefTeX interact with AUCTeX, http://www.gnu.org/s/auctex/manual/reftex/AUCTeX_002dRefTeX-Interface.html
     (setq reftex-plug-into-AUCTeX t)
     ;; So that RefTeX also recognizes \addbibresource. Note that you
     ;; can't use $HOME in path for \addbibresource but that "~"
     ;; works.
     (setq reftex-bibliography-commands '("bibliography" "nobibliography" "addbibresource"))
     (setq reftex-default-bibliography '(smax-references)); So that RefTeX in Org-mode knows bibliography
     (setq helm-bibtex-bibliography '(smax-references));
     (setcdr (assoc 'caption reftex-default-context-regexps) "\\\\\\(rot\\|sub\\)?caption\\*?[[{]"); Recognize \subcaptions, e.g. reftex-citation
     (setq reftex-cite-format; Get ReTeX with biblatex, see http://tex.stackexchange.com/questions/31966/setting-up-reftex-with-biblatex-citation-commands/31992#31992
           '((?t . "\\textcite[]{%l}")
             (?a . "\\autocite[]{%l}")
             (?c . "\\cite[]{%l}")
             (?s . "\\smartcite[]{%l}")
             (?f . "\\footcite[]{%l}")
             (?n . "\\nocite{%l}")
             (?b . "\\blockcquote[]{%l}{}")))))
(setq reftex-bibliography-commands '("bibliography" "nobibliography" "addbibresource"))
;; Fontification (remove unnecessary entries as you notice them) http://lists.gnu.org/archive/html/emacs-orgmode/2009-05/msg00236.html http://www.gnu.org/software/auctex/manual/auctex/Fontification-of-macros.html
(setq font-latex-match-reference-keywords
      '(
        ;; biblatex
        ("printbibliography" "[{")
        ("addbibresource" "[{")
        ;; Standard commands
        ;; ("cite" "[{")
        ("Cite" "[{")
        ("parencite" "[{")
        ("Parencite" "[{")
        ("footcite" "[{")
        ("footcitetext" "[{")
        ;; ;; Style-specific commands
        ("textcite" "[{")
        ("Textcite" "[{")
        ("smartcite" "[{")
        ("Smartcite" "[{")
        ("cite*" "[{")
        ("parencite*" "[{")
        ("supercite" "[{")
                                        ; Qualified citation lists
        ("cites" "[{")
        ("Cites" "[{")
        ("parencites" "[{")
        ("Parencites" "[{")
        ("footcites" "[{")
        ("footcitetexts" "[{")
        ("smartcites" "[{")
        ("Smartcites" "[{")
        ("textcites" "[{")
        ("Textcites" "[{")
        ("supercites" "[{")
        ;; Style-independent commands
        ("autocite" "[{")
        ("Autocite" "[{")
        ("autocite*" "[{")
        ("Autocite*" "[{")
        ("autocites" "[{")
        ("Autocites" "[{")
        ;; Text commands
        ("citeauthor" "[{")
        ("Citeauthor" "[{")
        ("citetitle" "[{")
        ("citetitle*" "[{")
        ("citeyear" "[{")
        ("citedate" "[{")
        ("citeurl" "[{")
        ;; Special commands
        ("fullcite" "[{")))

(setq font-latex-match-textual-keywords
      '(
        ;; biblatex brackets
        ("parentext" "{")
        ("brackettext" "{")
        ("hybridblockquote" "[{")
        ;; Auxiliary Commands
        ("textelp" "{")
        ("textelp*" "{")
        ("textins" "{")
        ("textins*" "{")
        ;; supcaption
        ("subcaption" "[{")))

(setq font-latex-match-variable-keywords
      '(
        ;; amsmath
        ("numberwithin" "{")
        ;; enumitem
        ("setlist" "[{")
        ("setlist*" "[{")
        ("newlist" "{")
        ("renewlist" "{")
        ("setlistdepth" "{")
        ("restartlist" "{")))
;; *** Helm-BibTeX
(use-package helm-bibtex
  :ensure auctex
  :init
  :config
  (add-hook 'TeX-mode-hook
	    (lambda() (define-key TeX-mode-map "\C-ch" 'helm-bibtex)) )
  (setq helm-bibtex-pdf-field "file")
  (setq helm-bibtex-pdf-open-function
	(lambda (fpath)
	  (start-process smax-reader "*helm-bibtex-reader*" smax-reader-path fpath)))
  (setq helm-bibtex-notes-path smax-bibnotes))
;; *** Completion
;; **** Company
;; (use-package company-auctex
;;   :ensure company
;;   :defer t
;;   :init
;;   (add-hook 'LaTeX-mode-hook 'company-auctex-init))
;; (use-package company-math
;;   :ensure auctex
;;   :defer t
;;   :init
;;   (defun latex-mode-setup ()
;;     (setq-local company-backends
;; 		(append '((company-math-symbols-latex company-latex-commands))
;; 			company-backends)))

;;   (add-hook 'TeX-mode-hook 'latex-mode-setup))
;; ** Functions and Bindings
(defvar tlmgr-installed-packages nil
  "Cached list of installed LaTeX packages.")


(defun tlmgr-installed (&optional refresh)
  "Get a list of installed LaTeX packages.  Uses a cached value if possible unless REFRESH is non-nil."
  (unless (or tlmgr-installed-packages refresh)
    (setq tlmgr-installed-packages
	  (mapcar (lambda (s)
		    (split-string (substring s 2) ":" t))
		  (split-string
		   (shell-command-to-string "tlmgr list --only-installed") "\n" t))))
  tlmgr-installed-packages)


(defun texdoc (package)
  "Run texdoc on the PACKAGE."
  (interactive (list (completing-read "Package: " (tlmgr-installed))))
  (shell-command (format "texdoc %s" package)))


(defun kpsewhich (symbol)
  "Run kpsewhich on SYMBOL."
  (interactive "sSymbol: ")
  (message (shell-command-to-string (format "kpsewhich %s" symbol))))

(provide 'smax-latex)

;;; smax-latex.el ends here
