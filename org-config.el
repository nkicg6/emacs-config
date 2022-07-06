(defun org-clear-scheduled ()
  "Remove the scheduled times for all tasks from buffer."
    (interactive)
  (save-excursion
    (replace-regexp "SCHEDULED: <[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} \\w+>" "")))

(setq org-agenda-files
      '("~/Dropbox/orgs/planner.org"
        "~/Dropbox/orgs/master_agenda.org"
        "~/Dropbox/orgs/daily_archive.org"))

;; https://orgmode.org/worg/doc.html
(setq org-agenda-custom-commands
      '(("c" "simple agenda view"
         agenda ""  ((org-agenda-span 1))
         ((org-agenda-overriding-header "\nDaily todos")
          (org-agenda-sorting-strategy '(todo-state-up scheduled-up deadline-up priority-down))))))

(setq org-agenda-include-diary t)

(bind-key "C-c a" 'org-agenda)

(advice-add 'org-agenda :after #'delete-other-windows)
