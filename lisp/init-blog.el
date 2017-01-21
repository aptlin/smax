(require-package 'blog-admin)
(add-hook 'blog-admin-backend-after-new-post-hook 'find-file)
(setq blog-admin-backend-type 'nikola)
(setq blog-admin-backend-path "~/WERKE/sdll.github.io/")
(setq blog-admin-backend-new-post-in-drafts t)
(setq blog-admin-backend-nikola-executable "~/PROG/PIT/nikola/bin/nikola") ;; path to nikola executable
(setq blog-admin-backend-nikola-config-file "conf.py") ;; conf.py is default
(provide 'init-blog)
