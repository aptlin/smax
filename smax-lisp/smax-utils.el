;;; smax-utils.el --- Utility functions smax cannot live without

;;; Commentary:
;; 

;;; Code:

;; * Hotspots
(defcustom smax-user-hotspot-commands '()
  "A-list of hotspots to jump to in `hotspots'.
These are shortcut to commands.
\(\"label\" . command)")

(defcustom smax-user-hotspot-locations '()
  "A-list of hotspot locations to jump to in  `hotspots'.
\(\"label\" . \"Path to file\").

These are like bookmarks.")


;;;###
(defun hotspots (arg)
  "Helm interface to hotspot locations.
This includes user defined
commands (`smax-user-hotspot-commands'),
locations (`smax-user-hotspot-locations'), org agenda files,
recent files and bookmarks. You can set a bookmark also."
  (interactive "P")
  (helm :sources `(((name . "Commands")
		    (candidates . ,smax-user-hotspot-commands)
		    (action . (("Open" . (lambda (x) (funcall x))))))
		   ((name . "My Locations")
		    (candidates . ,smax-user-hotspot-locations)
		    (action . (("Open" . (lambda (x) (find-file x))))))
		   ((name . "My org files")
		    (candidates . ,org-agenda-files)
		    (action . (("Open" . (lambda (x) (find-file x))))))
		   helm-source-recentf
		   helm-source-bookmarks
		   helm-source-bookmark-set)))


(add-to-list 'safe-local-eval-forms 
	     '(progn (require 'emacs-keybinding-command-tooltip-mode) (emacs-keybinding-command-tooltip-mode +1)))

;;;###
(defun smax-help ()
  "Open the ‘smax’ manual."
  (interactive)
  (find-file (expand-file-name
              "smax.org"
	      smax-dir)))

;; * Utilities
;;;###
(defun kill-all-buffers ()
  "Kill all buffers.  Leave one frame open."
  (interactive)
  (mapc 'kill-buffer (buffer-list))
  (delete-other-windows))


;;;###
(defun kill-other-buffers ()
  "Kill all other buffers but this one.  Leave one frame open."
  (interactive)
  (mapc 'kill-buffer
	(delq (current-buffer) (buffer-list)))
  (delete-other-windows))


;;;###
(defun unfill-paragraph ()
  "Unfill paragraph at or after point."
  (interactive "*")
  (let ((fill-column most-positive-fixnum))
    (fill-paragraph nil (region-active-p))))
;; case on regions
(defun sentence-case-region (r1 r2)
  "Capitalize the word at point, and the first word of each
sentence in the region."
  (Interactive "r")
  (save-excursion
    (goto-char r1)
    (capitalize-word 1)
    (while (< (point) r2)
      (forward-sentence)
      (capitalize-word 1))))

;; * The end
(provide 'smax-utils)

;;; smax-utils.el ends here
