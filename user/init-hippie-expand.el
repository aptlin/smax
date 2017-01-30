(use-package hippie-expand
  :ensure nil
  :init
  (setq hippie-expand-try-functions-list
	'(yas-hippie-try-expand
	  try-complete-file-name-partially
	  try-complete-file-name
	  try-expand-dabbrev
	  try-expand-dabbrev-all-buffers
	  try-expand-dabbrev-from-kill))
  :bind
  ("M-SPC" . hippie-expand))


(provide 'init-hippie-expand)
