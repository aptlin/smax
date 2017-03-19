;;; bootstrap.el --- install use-package


;;; Commentary:
;; 

;;; Code:

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(require 'diminish) ;; if you use :diminish

(require 'bind-key) ;; if you use any :bind variant

(use-package smax-builders
  :ensure nil
  :load-path conf-dir
  :init (require 'smax-builders))

(provide 'bootstrap)

;;; bootstrap.el ends here
