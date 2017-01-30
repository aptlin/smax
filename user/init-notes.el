(setq temp-notes-dir "~/TMP/NOTES/")
(defun add-note (arg)
  "Add a new note to temp-notes-dir."
  (interactive
   (list (read-string "Enter the title of the note:")))
  (find-file (read-file-name "Add a new note: " temp-notes-dir
                             nil
                             nil
			     (concat (format-time-string "%Y%m%d" (current-time))
				     arg)
                             nil)))
(provide 'init-notes)
