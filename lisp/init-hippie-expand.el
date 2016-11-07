(global-set-key (kbd "M-SPC") 'hippie-expand)

(setq hippie-expand-try-functions-list
      '(yas-hippie-try-expand
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill))

(provide 'init-hippie-expand)
