;;; smax-dired.el --- Summary

;;; Commentary:
;;
;;; Code:
;; * Dired Enhancements
;; http://ergoemacs.org/emacs/elisp_dired_rename_space_to_underscore.html
(require 'dired )

(defun xah-dired-rename-space-to-underscore ()
  "In dired, rename current or marked files by replacing space to underscore _.
If not in `dired', do nothing.
URL `http://ergoemacs.org/emacs/elisp_dired_rename_space_to_underscore.html'
Version 2016-12-22"
  (interactive)
  (require 'dired-aux)
  (if (equal major-mode 'dired-mode)
      (progn
        (mapc (lambda (x)
                (when (string-match " " x )
                  (dired-rename-file x (replace-regexp-in-string " " "_" x) nil)
                  ))
              (dired-get-marked-files ))
        (revert-buffer)
        (forward-line ))
    (user-error "Not in dired")))


(defun xah-dired-rename-space-to-hyphen ()
  "In dired, rename current or marked files by replacing space to hyphen -.
If not in `dired', do nothing.
URL `http://ergoemacs.org/emacs/elisp_dired_rename_space_to_underscore.html'
Version 2016-12-22"
  (interactive)
  (require 'dired-aux)
  (if (equal major-mode 'dired-mode)
      (progn
        (mapc (lambda (x)
                (when (string-match " " x )
                  (dired-rename-file x (replace-regexp-in-string " " "_" x) nil)))
              (dired-get-marked-files ))
        (revert-buffer))
    (user-error "Not in dired")))

(define-key dired-mode-map (kbd "_") 'xah-dired-rename-space-to-underscore)
(define-key dired-mode-map (kbd "-") 'xah-dired-rename-space-to-hyphen)

;;** MIME

(use-package openwith
  :ensure t
  :init
  :config
  (when (require 'openwith nil 'noerror)
    (setq openwith-associations
	  (list
	   (list (openwith-make-extension-regexp
		  '("mpg" "mpeg" "mp3" "mp4"
		    "avi" "wmv" "wav" "mov" "flv"
		    "ogm" "ogg" "mkv"))
		 "vlc"
		 '(file))
	   (list (openwith-make-extension-regexp
		  '("xbm" "pbm" "pgm" "ppm" "pnm"
		    "png" "gif" "bmp" "tif" "jpeg" "jpg"))
		 "eog"
		 '(file))
	   (list (openwith-make-extension-regexp
		  '("doc" "xls" "ppt" "odt" "ods" "odg" "odp"))
		 "libreoffice"
		 '(file))
	   '("\\.lyx" "lyx" (file))
	   '("\\.chm" "kchmviewer" (file))
	   (list (openwith-make-extension-regexp
		  '("pdf" "ps" "ps.gz" "dvi" "djvu"))
		 smax-reader
		 '(file))
	   )))

  (openwith-mode 1)
  (add-to-list  'mm-inhibit-file-name-handlers 'openwith-file-handler))
(provide 'smax-dired)

;;; smax-dired.el ends here
