(setq temp-notes-dir "~/TMP/NOTES/")
(defun add-note ()
  "Add a new note to temp-notes-dir."
  (interactive)
  (find-file (read-file-name "Add a new note: " temp-notes-dir
                             nil
                             nil
                             (format-time-string "%Y%m%d" (current-time))
                             nil)))

(provide 'init-notes)
