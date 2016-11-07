(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)

(require-package 'company-auctex)
(require 'company-auctex)

(company-auctex-init)

(provide 'init-latex)
