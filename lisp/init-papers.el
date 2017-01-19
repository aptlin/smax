(require-package 'zotxt)
(require 'org-zotxt)
;; Activate org-zotxt-mode in org-mode buffers
(add-hook 'org-mode-hook 'org-zotxt-mode)
;; Bind something to replace the awkward C-u C-c " i
(define-key org-mode-map
  (kbd "C-c \" \"") (lambda () (interactive)
                      (org-zotxt-insert-reference-link '(4))))
;;(load-file "~/.emacs.d/site-lisp/org-pdcite.el")

(setq org-zotxt-link-description-style :betterbibtexkey)

(provide 'init-papers)
