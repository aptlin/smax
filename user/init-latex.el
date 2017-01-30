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
  (progn (add-hook 'LaTeX-mode-hook 'turn-on-cdlatex)   ; with AUCTeX LaTeX mode

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

(provide 'init-latex)
