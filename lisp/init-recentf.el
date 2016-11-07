(recentf-mode 1)
(setq-default
 recentf-max-saved-items 1000
 recentf-exclude '("/tmp/" "/ssh:"))
(run-at-time nil (* 5 60) 'recentf-save-list)


(provide 'init-recentf)
