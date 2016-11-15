;; from http://ergoemacs.org/emacs/bookmark.html
(setq inhibit-splash-screen t)
(require 'bookmark)
(bookmark-bmenu-list)
(switch-to-buffer "*Bookmark List*")
(provide 'init-bookmarks)
