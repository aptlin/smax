(setq org-html-link-org-files-as-html nil)
(setq org-directory "~/ORG/")
(setq org-default-notes-file "~/ORG/refile.org")
(setq org-capture-templates
      (quote (("t" "todo" entry (file "~/ORG/refile.org")
               "* TODO %?\n%U\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))%a\n" :clock-in t :clock-resume t)
              ("r" "respond" entry (file "~/ORG/refile.org")
               "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n" :clock-in t :clock-resume t :immediate-finish t)
              ("n" "note" entry (file "~/ORG/refile.org")
               "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
              ("j" "Journal" entry (file+datetree "~/ORG/diary.org")
               "* %?\n%U\n" :clock-in t :clock-resume t)
              ("w" "org-protocol" entry (file "~/ORG/refile.org")
               "* TODO Review %c\n%U\n" :immediate-finish t)
              ("m" "Meeting" entry (file "~/ORG/refile.org")
               "* MEETING with %? :MEETING:\n%U" :clock-in t :clock-resume t)
              ("p" "Phone call" entry (file "~/ORG/refile.org")
               "* PHONE %? :PHONE:\n%U" :clock-in t :clock-resume t)
              ("h" "Habit" entry (file "~/ORG/refile.org")
               "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n"))))
(setq org-ref-bibliography-notes "~/ORG/notes.org"
      org-ref-default-bibliography '("~/ORG/references.bib")
      org-ref-pdf-directory "~/ORG/PAPERS/")
(setq bibtex-completion-bibliography "~/ORG/references.bib"
      bibtex-completion-library-path "~/ORG/PAPERS"
      bibtex-completion-notes-path "~/ORG/bibtex-notes")

