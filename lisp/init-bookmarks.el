;; from http://ergoemacs.org/emacs/bookmark.html
(setq bookmark-default-file  "~/ORG/bookmarks")
(require-package 'bookmark+)
(setq bookmark-version-control t)
(after-load 'guide-key
  (add-to-list 'guide-key/guide-key-sequence "C-x p"))


(provide 'init-bookmarks)
