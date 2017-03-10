;;(load "auctex.el" nil t t)
;;(load "preview-latex.el" nil t t)
(setq-default TeX-engine 'xetex)
(setq TeX-parse-self t); Enable parse on load.
(setq TeX-auto-save t); Enable parse on save.
;;(setq-default TeX-master nil)
;;(setq-default TeX-master "master") ;

(setq TeX-PDF-mode t); PDF mode (rather than DVI-mode)
;; improve abbrev-expansion

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


	 (add-hook 'plain-TeX-mode-hook
		   (lambda () (set (make-variable-buffer-local 'TeX-electric-math)
				   (cons "$" "$"))))
	 (add-hook 'LaTeX-mode-hook
		   (lambda () (set (make-variable-buffer-local 'TeX-electric-math)
				   (cons "\\(" "\\)"))))


	 (setq TeX-electric-sub-and-superscript t)

	 (setq LaTeX-electric-left-right-brace t)
	 ))

(use-package company-auctex
  :defer t
  :init
  (add-hook 'LaTeX-mode-hook 'company-auctex-init))

;; * Reftex

(add-hook 'TeX-mode-hook 'turn-on-reftex)
(add-hook 'org-mode-hook 'turn-on-reftex)
;; add helm-bibtex
;; http://iflysib14.iflysib.unlp.edu.ar/tomas/en/blog/reference-management.html
(add-hook 'TeX-mode-hook
          (lambda() (define-key TeX-mode-map "\C-ch" 'helm-bibtex)) )
(setq  helm-bibtex-pdf-field "file")
(setq helm-bibtex-pdf-open-function
      (lambda (fpath)
        (start-process "evince" "*helm-bibtex-evince*" "/usr/bin/evince" fpath)))

(setq helm-bibtex-notes-path "~/ORG/bibnotes.org")

(eval-after-load 'reftex-vars; Is this construct really needed?
  '(progn
     (setq reftex-cite-prompt-optional-args t); Prompt for empty optional arguments in cite macros.
     ;; Make RefTeX interact with AUCTeX, http://www.gnu.org/s/auctex/manual/reftex/AUCTeX_002dRefTeX-Interface.html
     (setq reftex-plug-into-AUCTeX t)
     ;; So that RefTeX also recognizes \addbibresource. Note that you
     ;; can't use $HOME in path for \addbibresource but that "~"
     ;; works.
     (setq reftex-bibliography-commands '("bibliography" "nobibliography" "addbibresource"))
     (setq reftex-default-bibliography '("~/ORG/references.bib")); So that RefTeX in Org-mode knows bibliography
     (setq helm-bibtex-bibliography '("~/ORG/references.bib"));
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

(provide 'init-latex)
