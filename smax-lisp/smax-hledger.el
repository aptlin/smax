(use-package hledger-mode
  :mode ("\\.journal\\'" "\\.ledger\\'" "\\.hledger\\'")
  :preface
  (defun hledger/next-entry ()
    "Move to next entry and pulse."
    (interactive)
    (hledger-next-or-new-entry)
    (hledger-pulse-momentary-current-entry))

  (defun hledger/prev-entry ()
    "Move to last entry and pulse."
    (interactive)
    (hledger-backward-entry)
    (hledger-pulse-momentary-current-entry))

  :bind (("<f2> h" . hledger-jentry)
	 ("<f2> r" . hledger-run-command)
	 :map hledger-mode-map
	 ("M-p" . hledger/prev-entry)
	 ("M-n" . hledger/next-entry))
  :init
  (setq hledger-jfile
        (expand-file-name "~/ORG/FINANCE/accounting.journal"))
  (when (boundp 'my-hledger-service-fetch-url)
    (setq hledger-service-fetch-url
	  my-hledger-service-fetch-url))
  :config
  (add-hook 'hledger-view-mode-hook 'hl-line-mode))
(provide 'smax-hledger)
