(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(coffee-tab-width 2)
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#839496")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-safe-themes
   (quote
    ("d5f17ae86464ef63c46ed4cb322703d91e8ed5e718bf5a7beb69dd63352b26b2" "98cc377af705c0f2133bb6d340bf0becd08944a588804ee655809da5d8140de6" "5dc0ae2d193460de979a463b907b4b2c6d2c9c4657b2e9e66b8898d2592e3de5" default)))
 '(electric-pair-mode t)
 '(fci-rule-color "#eee8d5")
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#002b36" 0.25)
    (quote
     ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#93a1a1")
 '(highlight-tail-colors
   (quote
    (("#073642" . 0)
     ("#546E00" . 20)
     ("#00736F" . 30)
     ("#00629D" . 50)
     ("#7B6000" . 60)
     ("#8B2C02" . 70)
     ("#93115C" . 85)
     ("#073642" . 100))))
 '(hl-bg-colors
   (quote
    ("#7B6000" "#8B2C02" "#990A1B" "#93115C" "#3F4D91" "#00629D" "#00736F" "#546E00")))
 '(hl-fg-colors
   (quote
    ("#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36")))
 '(hl-paren-colors (quote ("#2aa198" "#b58900" "#268bd2" "#6c71c4" "#859900")))
 '(magit-diff-use-overlays nil)
 '(markdown-command "/usr/local/bin/pandoc" t)
 '(nrepl-message-colors
   (quote
    ("#dc322f" "#cb4b16" "#b58900" "#5b7300" "#b3c34d" "#0061a8" "#2aa198" "#d33682" "#6c71c4")))
 '(org-capture-templates
   (quote
    (("p" "paper" entry
      (file+headline "~/Dropbox/orgs/papers.org" "Uncategorized")
      (file "~/.emacs.d/templates/paper.orgtmpl")
      :empty-lines-after 1)
     ("t" "todo" entry
      (file+headline "~/Dropbox/orgs/master_agenda.org" "Unfiled todos")
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
     ("q" "quick note" entry
      (file+headline "~/Dropbox/orgs/master_agenda.org" "Uncategorized notes")
      (file "~/.emacs.d/templates/note.orgtmpl")
      :empty-lines-after 1)
     ("w" "workout" entry
      (file+datetree "~/Dropbox/orgs/workout.org")
      (file "~/.emacs.d/templates/workout.orgtmpl")))))
 '(package-selected-packages
   (quote
    (company-jedi org ox emacs-htmlize ox-publish pipenv paredit paredit-mode leuven-theme material-theme projectile s rainbow-delimiters use-package org-link-minor-mode)))
 '(pos-tip-background-color "#073642")
 '(pos-tip-foreground-color "#93a1a1")
 '(python-shell-interpreter "python3")
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#073642" 0.2))
 '(term-default-bg-color "#002b36")
 '(term-default-fg-color "#839496")
 '(vc-annotate-background nil)
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#dc322f")
     (40 . "#cb4366eb20b4")
     (60 . "#c1167942154f")
     (80 . "#b58900")
     (100 . "#a6ae8f7c0000")
     (120 . "#9ed892380000")
     (140 . "#96be94cf0000")
     (160 . "#8e5397440000")
     (180 . "#859900")
     (200 . "#77679bfc4635")
     (220 . "#6d449d465bfd")
     (240 . "#5fc09ea47092")
     (260 . "#4c68a01784aa")
     (280 . "#2aa198")
     (300 . "#303498e7affc")
     (320 . "#2fa1947cbb9b")
     (340 . "#2c879008c736")
     (360 . "#268bd2"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (quote
    (unspecified "#002b36" "#073642" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#839496" "#657b83")))
 '(xterm-color-names
   ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#eee8d5"])
 '(xterm-color-names-bright
   ["#002b36" "#cb4b16" "#586e75" "#657b83" "#839496" "#6c71c4" "#93a1a1" "#fdf6e3"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
