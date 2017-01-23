(require-package 'dired+)
(require-package 'dired-sort)

(setq-default diredp-hide-details-initially-flag nil
              dired-dwim-target t)

;; Prefer g-prefixed coreutils version of standard utilities when available
(let ((gls (executable-find "gls")))
  (when gls (setq insert-directory-program gls)))

(after-load 'dired
  (require 'dired+)
  (require 'dired-sort)
  (when (fboundp 'global-dired-hide-details-mode)
    (global-dired-hide-details-mode -1))
  (setq dired-recursive-deletes 'top)
  (define-key dired-mode-map [mouse-2] 'dired-find-file)

  (when (maybe-require-package 'diff-hl)
    (after-load 'dired
      (add-hook 'dired-mode-hook 'diff-hl-dired-mode))))

(global-set-key (kbd "<f9> d") 'dired)

(require-package 'openwith)
;; (setq openwith-associations '(("\\.pdf\\'" "zathura" (file))))
;; (setq openwith-associations '(("\\.djvu\\'" "zathura" (file))))

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
               "evince"
               '(file))
         )))

(openwith-mode 1)
(add-to-list  'mm-inhibit-file-name-handlers 'openwith-file-handler)
;; open org dirlinks in emacs
(add-to-list 'org-file-apps '(directory . emacs))

(provide 'init-dired)
