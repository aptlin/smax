;; ** Company and Yas integration
(defvar malb/company-mode/enable-yas t
  "Enable yasnippet for all backends.")

(defun malb/company-mode/backend-with-yas (backend)
  (if (or (not malb/company-mode/enable-yas)
          (and (listp backend)
               (member 'company-yasnippet backend)))
      backend
    (append (if (consp backend) backend (list backend))
            '(:with company-yasnippet))))

(setq company-backends
      (mapcar #'malb/company-mode/backend-with-yas company-backends))

(provide 'smax-finish)
