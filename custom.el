(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(coffee-tab-width 2)
 '(custom-safe-themes
   (quote
    ("d5f17ae86464ef63c46ed4cb322703d91e8ed5e718bf5a7beb69dd63352b26b2" "98cc377af705c0f2133bb6d340bf0becd08944a588804ee655809da5d8140de6" "5dc0ae2d193460de979a463b907b4b2c6d2c9c4657b2e9e66b8898d2592e3de5" default)))
 '(fci-rule-color "#37474f")
 '(markdown-command "/usr/local/bin/pandoc" t)
 '(org-agenda-files
   (quote
    ("~/Dropbox/CV_biosketch/cv.org" "/Users/Nick/Dropbox/orgs/Expanding-orgmode.py-to-get-better-org-python-integration.org" "/Users/Nick/Dropbox/orgs/books.org" "/Users/Nick/Dropbox/orgs/class_2017.org" "/Users/Nick/Dropbox/orgs/daily_archive.org" "/Users/Nick/Dropbox/orgs/electrophysiology.org" "/Users/Nick/Dropbox/orgs/image_analysis.org" "/Users/Nick/Dropbox/orgs/journal.org" "/Users/Nick/Dropbox/orgs/master_agenda.org" "/Users/Nick/Dropbox/orgs/papers.org" "/Users/Nick/Dropbox/orgs/projects.org" "/Users/Nick/Dropbox/orgs/reference_texts.org" "/Users/Nick/Dropbox/orgs/sanfrancisco.org" "/Users/Nick/Dropbox/orgs/seminars_and_notes.org" "/Users/Nick/Dropbox/orgs/snakeSnap.org" "/Users/Nick/Dropbox/orgs/statistics.org" "/Users/Nick/Dropbox/orgs/workout.org" "/Users/Nick/Dropbox/lab_notebook/analysis_statistics.org" "/Users/Nick/Dropbox/lab_notebook/data_analysis.org" "/Users/Nick/Dropbox/lab_notebook/git_instructions.org" "/Users/Nick/Dropbox/lab_notebook/myelin_oscillations.org" "/Users/Nick/Dropbox/lab_notebook/project_ideas.org")))
 '(org-capture-templates
   (quote
    (("p" "paper" entry
      (file+headline "~/Dropbox/orgs/papers.org" "Uncategorized")
      (file "~/.emacs.d/templates/paper.orgtmpl")
      :empty-lines-after 1)
     ("t" "todo" entry
      (file+headline "~/Dropbox/orgs/master_agenda.org" "Agenda")
      (file "~/.emacs.d/templates/todo.orgtmpl")
      :empty-lines-after 1)
     ("b" "book" entry
      (file+headline "~/Dropbox/orgs/books.org" "Captured-uncategorized")
      (file "~/.emacs.d/templates/books.orgtmpl")
      :empty-lines-after 1)
     ("a" "article" entry
      (file+headline "~/Dropbox/orgs/master_agenda.org" "Uncategorized notes")
      (file "~/.emacs.d/templates/articles.orgtmpl")
      :empty-lines-after 1)
     ("j" "journal" entry
      (file+datetree "~/Dropbox/orgs/journal.org")
      (file "~/.emacs.d/templates/journal.orgtmpl")
      :empty-lines-after 1)
     ("l" "link" entry
      (file+headline "~/Dropbox/orgs/master_agenda.org" "Uncategorized links")
      (file "~/.emacs.d/templates/link.orgtmpl")
      :empty-lines-after 1)
     ("n" "lab notebook")
     ("no" "odor training" entry
      (file+datetree "~/Dropbox/lab_notebook/notebook/lab_notebook.org")
      (file "~/.emacs.d/templates/training_notebook.orgtmpl")
      :empty-lines-after 1)
     ("q" "quick note" entry
      (file+headline "~/Dropbox/orgs/master_agenda.org" "Uncategorized notes")
      (file "~/.emacs.d/templates/note.orgtmpl")
      :empty-lines-after 1)
     ("w" "workout" entry
      (file+datetree "~/Dropbox/orgs/workout.org")
      (file "~/.emacs.d/templates/workout.orgtmpl"))
     ("n" "notebook")
     ("ng" "General lab notebook entry" entry
      (file+datetree "~/Dropbox/lab_notebook/notebook/lab_notebook.org")
      (file "~/.emacs.d/templates/general-lab.orgtmpl")
      :jump-to-captured t :empty-lines-after 1))))
 '(package-selected-packages
   (quote
    (leuven-theme material-theme projectile s rainbow-delimiters use-package org-link-minor-mode)))
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#f36c60")
     (40 . "#ff9800")
     (60 . "#fff59d")
     (80 . "#8bc34a")
     (100 . "#81d4fa")
     (120 . "#4dd0e1")
     (140 . "#b39ddb")
     (160 . "#f36c60")
     (180 . "#ff9800")
     (200 . "#fff59d")
     (220 . "#8bc34a")
     (240 . "#81d4fa")
     (260 . "#4dd0e1")
     (280 . "#b39ddb")
     (300 . "#f36c60")
     (320 . "#ff9800")
     (340 . "#fff59d")
     (360 . "#8bc34a"))))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
