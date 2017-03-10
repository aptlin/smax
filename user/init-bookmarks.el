;; from http://ergoemacs.org/emacs/bookmark.html
(setq bookmark-default-file  "~/ORG/bookmarks")
(use-package bookmark+
  :ensure t
  :init
  :bind
  (("<f6>" . bookmark-bmenu-list))
  )
(setq bookmark-version-control t)


(provide 'init-bookmarks)
