;; revised, simplified init file for new computer.
(setq user-full-name "Nick George"
      user-mail-address "nicholas.m.george@cuanschutz.edu")

(defun my/tangle-dotfiles ()
   "If the current file is this file, the code blocks are tangled"
   (when (equal (buffer-file-name) (expand-file-name "~/.emacs.d/revised-init.el"))
     (byte-compile-file "~/.emacs.d/revised-init.el")))

(add-hook 'after-save-hook #'my/tangle-dotfiles)

;; rainbows
(use-package rainbow-delimiters
  :defer t)
;; s package
(use-package s
  :defer t)

(use-package projectile
  :commands (projectile-mode)
  :defer t
  :init
  (projectile-mode))
(setq projectile-indexing-method 'native)

(use-package material-theme
  :ensure t
  :defer t)

(use-package zenburn-theme
  :ensure t
  :defer t)

(use-package solarized-theme
  :defer t
  :init
  (setq solarized-use-variable-pitch nil)
  :ensure t)

(use-package leuven-theme
  :ensure t
  :config
  (load-theme 'leuven t))

(defun switch-theme (theme)
  "Disables any currently active themes and loads THEME."
  ;; This interactive call is taken from `load-theme'
  (interactive
   (list
    (intern (completing-read "Load custom theme: "
                             (mapc 'symbol-name
                                   (custom-available-themes))))))
  (let ((enabled-themes custom-enabled-themes))
    (mapc #'disable-theme custom-enabled-themes)
    (load-theme theme t)))

(defun disable-active-themes ()
  "Disables any currently active themes listed in `custom-enabled-themes'."
  (interactive)
  (mapc #'disable-theme custom-enabled-themes))

(global-set-key (kbd "C-c t") 'switch-theme)

;; (setq x-select-enable-clipboard t)

(use-package paredit)

(use-package openwith
  :defer t)

(setq openwith-associations '(("\\.pptx?\\'" "open" (file))
                              ("\\.docx?\\'" "open" (file))
                              ("\\.xlsx?\\'" "open" (file))
                              ("\\.ods?\\'" "open" (file))))
(openwith-mode t)

(defalias 'yes-or-no-p 'y-or-n-p)


(global-set-key (kbd "C-c y") 'kill-buffer-and-window) ;; kill buffer and window is C-c C-k
(global-set-key (kbd "C-c c")'org-capture) ;; start org capture.
(global-set-key (kbd "C-c m") (lambda () (interactive) (find-file "~/Dropbox/orgs/master_agenda.org"))) ;; master agenda in org.
(global-set-key (kbd "C-c i") (lambda () (interactive) (find-file "~/.emacs.d/revised-init.el"))) ;; config file
(global-set-key (kbd "C-c l") (lambda () (interactive) (find-file "~/Dropbox/lab_notebook/lab_notebook.org"))) ;; lab notebook in org.
(global-set-key (kbd "C-c d") (lambda () (interactive) (find-file "~/Dropbox/lab_notebook/data_analysis.org"))) ;; go to data analysis


(bind-key "C-c l" 'org-store-link)
(bind-key "C-c c" 'org-capture)
(bind-key "C-c a" 'org-agenda)
(advice-add 'org-agenda :after #'delete-other-windows)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; Turn off the menu bar at the top of each frame because it's distracting
(menu-bar-mode -1)
;; Show line numbers
(use-package nlinum
  :defer t)
(global-nlinum-mode)
;; You can uncomment this to remove the graphical toolbar at the top. After
;; awhile, you won't need the toolbar.
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;; Don't show native OS scroll bars for buffers because they're redundant
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(set-face-attribute 'default nil :height 140)

(setq ;; makes killing/yanking interact with the clipboard
      select-enable-clipboard t

      ;; I'm actually not sure what this does but it's recommended?
      select-enable-primary t

      ;; Save clipboard strings into kill ring before replacing them.
      ;; When one selects something in another program to paste it into Emacs,
      ;; but kills something in Emacs before actually pasting it,
      ;; this selection is gone unless this variable is non-nil
      save-interprogram-paste-before-kill t

      ;; Shows all options when running apropos. For more info,
      ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Apropos.html
      apropos-do-all t

      ;; Mouse yank commands yank at point instead of at click.
      mouse-yank-at-point t)

;; No cursor blinking, it's distracting
(blink-cursor-mode 0)

;; full path in title bar
(setq-default frame-title-format "%b (%f)")

;; frame size
(if (window-system) (set-frame-size (selected-frame) 100 50))

;; don't pop up font menu
(global-set-key (kbd "s-t") '(lambda () (interactive)))

;; no bell
(setq ring-bell-function 'ignore)

;;    (require 'uniquify)

;; (use-package uniquify
;;       :ensure t
;;       :config
;;       (setq uniquify-buffer-name-style 'forward))

(setq uniquify-buffer-name-style 'forward)
;; Highlights matching parenthesis
(show-paren-mode 1)

;; Highlight current line
(global-hl-line-mode 1)

;; Interactive search key bindings. By default, C-s runs
;; isearch-forward, so this swaps the bindings.
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;; Don't use hard tabs
(setq-default indent-tabs-mode nil)
;; When you visit a file, point goes to the last place where it
;; was when you previously visited the same file.
;; http://www.emacswiki.org/emacs/SavePlace
;;        (require 'saveplace)
(use-package saveplace
  :defer t
  :config
  (setq-default save-place t)  
  (setq save-place-file (concat user-emacs-directory "places")))
;; Emacs can automatically create backup files. This tells Emacs to
;; put all backups in ~/.emacs.d/backups. More info:
;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Backup-Files.html
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory
                                               "backups"))))
(setq auto-save-default nil)

(require 'cl-lib)
(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (cl-letf (((symbol-function #'process-list) (lambda ())))
    ad-do-it))



(use-package ibuffer
  :commands ibuffer
  :bind ("C-x C-b" . ibuffer))

(define-ibuffer-column size-h
  (:name "Size" :inline t)
  (cond
   ((> (buffer-size) 1000000) (format "%7.3fM" (/ (buffer-size) 1000000.0)))
   ((> (buffer-size) 1000) (format "%7.3fk" (/ (buffer-size) 1000.0)))
   (t (format "%8d" (buffer-size)))))

(setq ibuffer-default-sorting-mode 'recency)

(setq ibuffer-saved-filter-groups
      '(("home"
         ("emacs-config" (or (filename . ".emacs.d")
                             (filename . "emacs-config")))
         ("Org" (or (mode . org-mode)
                    (filename . "OrgMode")))
         ("code" (or (mode . python-mode)
                     (mode . inferior-lisp-mode)
                     (mode . ess-mode)
                     (mode . scheme-mode)
                     (mode . clojure-mode)
                     (mode . clojurescript-mode)
                     (mode . prog-mode)))
         ("shell/REPL" (or (mode . eshell-mode)
                           (mode . cider-repl-mode)
                           (mode . comint-mode))))))
(add-hook 'ibuffer-mode-hook
          '(lambda ()
             (ibuffer-switch-to-saved-filter-groups "home")))
(setq ibuffer-truncate-lines t)


;; Enhances M-x to allow easier execution of commands. Provides
;; a filterable list of possible commands in the minibuffer
;; http://www.emacswiki.org/emacs/Smex
(use-package smex
  :defer t
  :bind 
  ("M-x" . smex)
  :config
  (smex-initialize)
  :init
  (setq smex-save-file (concat user-emacs-directory ".smex-items")))

(use-package helm
  :ensure t
  :defer t
  :bind  (("M-a" . helm-M-x)
          ("C-x C-f" . helm-find-files)
          ("M-y" . helm-show-kill-ring)
          ("C-x b" . helm-buffers-list))
  :config (progn
            (setq helm-buffers-fuzzy-matching t)
            (helm-mode 1)))

(use-package helm-projectile
  :defer t)
(helm-projectile-on)

;;  use recent file stuff
(use-package recentf
  :bind ("C-x C-r" . helm-recentf)
  :defer t  
  :config
  (recentf-mode t)
  (setq recentf-max-saved-items 200))

  ;; recommended from https://www.emacswiki.org/emacs/RecentFiles

;;  (run-at-time nil (* 5 60) 'recentf-save-list)
  (setq create-lockfiles nil) ;; see this https://github.com/syl20bnr/spacemacs/issues/5554

(use-package markdown-mode
  :ensure t
  :defer t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;; regular python stuff
   (use-package python-mode
     :defer t
     :ensure t)


;; python environment
(use-package elpy
  :ensure t
  :defer t
  ;;:config
  ;;(setenv "WORKON_HOME" "~/.ve")
  :init
  (add-hook 'python-mode-hook 'elpy-mode)
  )
(elpy-enable)


;; highlight indentation off, only use current column
(highlight-indentation-mode nil)
(add-hook 'python-mode-hook 'highlight-indentation-current-column-mode)
;; (highlight-indentation-current-column-mode t)

(use-package flycheck
  :ensure t
  :defer t
  :init (global-flycheck-mode))

(use-package jedi
  :defer t)
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)

;; for org babel
(setq org-babel-python-command "python3")

(setq ansi-color-for-comint-mode t)

;; python

(use-package pipenv
  :hook (python-mode . pipenv-mode)
  :init
  (setq
   pipenv-projectile-after-switch-function
   #'pipenv-projectile-after-switch-extended))
(add-hook 'python-mode-hook 'rainbow-delimiters-mode)
(add-hook 'python-mode-hook 'electric-pair-mode)

;; Automatically remove trailing whitespace when file is saved.
(add-hook 'python-mode-hook
      (lambda()
        (add-hook 'local-write-file-hooks
              '(lambda()
                 (save-excursion
                   (delete-trailing-whitespace))))))


(use-package org-pomodoro
  :defer t)

(setq org-src-window-setup (quote current-window))
;; auto open org files in org mode.
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode)) ;; auto activate org mode for org docs.

(setq org-startup-with-inline-images t) ;; for inline code images in python


(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "In-progress(ip)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)" ))))
;; log time on finish
(setq org-log-done 'time)

(setq org-todo-keyword-faces
      '(("TODO" :foreground "red" :weight bold)
        ("NEXT" :foreground "yellow" :weight bold)
        ("In-progress" :foreground "orange" :weight bold)
        ("WAITING" :foreground "black" :background "grey" :weight bold)
        ("DONE" :foreground "#2D6B2D" :weight bold)
        ("CANCELLED" :foreground "#2D6B2D")))

(add-hook 'org-mode-hook
          (lambda ()
            (org-bullets-mode t)))
;; hook for org mode wrap paragraphs
(add-hook 'org-mode-hook  (lambda () (setq truncate-lines nil)))
(add-hook 'org-mode-hook 'visual-line-mode)
(setq org-agenda-files
      '("~/Dropbox/orgs/master_agenda.org"
        "~/Dropbox/orgs/myelin-neuron-communication.org"
        "~/Dropbox/orgs/samplej.org"
        "~/Dropbox/orgs/smaller-projects.org"
        "~/Dropbox/orgs/recurring-reminders-and-tasks.org"))
;; electric pairs rock!
(add-hook 'org-mode-hook 'electric-pair-mode)
(use-package org-bullets
  :defer t)

;; dealing with time here: https://writequit.org/denver-emacs/presentations/2017-04-11-time-clocking-with-org.html
(setq org-clock-idle-time 15)
;; Resume clocking task when emacs is restarted
(org-clock-persistence-insinuate)
;; Save the running clock and all clock history when exiting Emacs, load it on startup
(setq org-clock-persist t)
;; Resume clocking task on clock-in if the clock is open
(setq org-clock-in-resume t)
;; Do not prompt to resume an active clock, just resume it
(setq org-clock-persist-query-resume nil)
;; Change tasks to whatever when clocking in
(setq org-clock-in-switch-to-state "NEXT")
;; Save clock data and state changes and notes in the LOGBOOK drawer
(setq org-clock-into-drawer t)
;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks
;; with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)
;; Clock out when moving task to a done state
(setq org-clock-out-when-done t)
;; Enable auto clock resolution for finding open clocks
(setq org-clock-auto-clock-resolution (quote when-no-clock-is-running))
;; Include current clocking task in clock reports
(setq org-clock-report-include-clocking-task t)
;; use pretty things for the clocktable
(setq org-pretty-entities t)

(setq org-tags-column 45)

;; reftex
(use-package reftex
  :commands turn-on-reftex
  :init
  (progn
    (setq reftex-default-bibliography '("/Users/Nick/Dropbox/bibliography/zotero-library.bib"))
    (setq reftex-plug-intoAUCTex t))
  :defer t  
  )
(use-package helm-bibtex)

(use-package org-ref
  :after org
  :defer t
  :init
  (setq reftex-default-bibliography '("~/Dropbox/bibliography/zotero-library.bib"))
  (setq org-ref-default-bibliography '("~/Dropbox/bibliography/zotero-library.bib"))
  (setq org-ref-pdf-directory '("~/PDFs")))


(setq org-export-cording-system 'utf-8)


(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t) 
   (clojure . t)
   (R . t) 
   (C . t)
   (sqlite . t)
   (latex . t)
   (shell . t)
   (octave . t)
   (matlab . t)
   (org . t)
   (emacs-lisp . t)
   (dot .t)))
;; use python 3 default

(setq org-babel-python-command "python3")


;; dont evaluate on export
;; this causes it to ignore header args and export anyways, so cancel it. 
;; see this https://www.miskatonic.org/2016/10/03/orgexportbabelevaluate/
;;(setq org-export-babel-evaluate nil)
;; dont confirm execute with these languages. 
(defun my-org-confirm-babel-evaluate (lang body)
  (not (member lang '("octave" "sh" "python" "R" "emacs-lisp" "clojure" "shell" "ipython" "bash"))))
(setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)
;; inline images-- nevermind this is annoying
;;(add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)

;; format source blocks natively
;; from http://www.i3s.unice.fr/~malapert/org/tips/emacs_orgmode.html
(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)

(use-package cider)
(setq org-babel-clojure-backend 'cider)
(org-defkey org-mode-map "\C-c\C-x\C-e" 'cider-eval-last-sexp)
(setq cider-repl-display-help-banner nil)


 ;;autocompletion from cider https://github.com/clojure-emacs/cider/blob/master/doc/code_completion.md
(use-package company) ;; autocompletion
(add-hook 'cider-mode-hook #'company-mode)
(add-hook 'cider-repl-mode-hook #'company-mode)
(add-hook 'cider-repl-mode-hook 'paredit-mode)
(define-key cider-mode-map (kbd "C-<tab>") #'company-complete)
(setq company-idle-delay nil) ;;never start completions automatically
(global-set-key (kbd "TAB") #'company-indent-or-complete-common)

;;(use-package inf-clojure)
;;(add-hook 'clojure-mode-hook #'inf-clojure-minor-mode)
(defun lein-tramp ()
  (interactive)
  (inf-clojure "lein trampoline repl"))

(add-hook 'inf-clojure-mode #'paredit-mode)
(add-hook 'inf-clojure-mode-hook #'paredit-mode)
(add-hook 'clojure-mode-hook #'eldoc-mode)
(add-hook 'inf-clojure-mode-hook #'eldoc-mode)
(add-hook 'inf-clojure-mode #'company-mode)
(add-hook 'inf-clojure-mode-hook #'company-mode)
(setq company-idle-delay nil) ; never start completions automatically
;;(global-set-key (kbd "C-<tab>") #'company-complete)

(setq org-publish-project-alist
      '(
        ("programming"
         :base-directory "~/personal_projects/website-clj/resources/org-programming"
         :base-extension "org"
         :publishing-directory "~/personal_projects/website-clj/resources/programming"
         :publishing-function org-html-publish-to-html
         :headline-levels 4
         :html-extension "html"
         :body-only t)
        ("science"
         :base-directory "~/personal_projects/website-clj/resources/org-science"
         :base-extension "org"
         :publishing-directory "~/personal_projects/website-clj/resources/science"
         :publishing-function org-html-publish-to-html
         :headline-levels 4
         :html-extension "html"
         :body-only t)
        ("clj-site" :components ("programming" "science"))))


;; (use-package ess-site
;;   :defer t) 
(use-package ess
  :ensure t
  :defer t)

(add-hook 'ess-mode-hook #'company-mode)
;; https://emacs.stackexchange.com/questions/8041/how-to-implement-the-piping-operator-in-ess-mode
(defun then_R_operator ()
  "R - %>% operator or 'then' pipe operator"
  (interactive)
  (just-one-space 1)
  (insert " %>% ")
  (reindent-then-newline-and-indent))

(defun r_assignment_operator ()
  "R assignment <- operator"
  (interactive)
  (just-one-space 1)
  (insert " <- "))
;;(define-key ess-mode-map (kbd "C-M-m") 'then_R_operator)
;;(define-key inferior-ess-mode-map (kbd "C-M-m") 'then_R_operator)
;;(define-key ess-mode-map (kbd "C-=") 'r_assignment_operator)
;;(define-key inferior-ess-mode-map (kbd "C-=") 'r_assignment_operator)


;; also new YASnippet for assignment <- which is -<TAB>

;; I do not like the underscore replace behavior. 
;;(ess-toggle-underscore nil)

(use-package magit
  :ensure t
  :defer t
  :config
  (define-key magit-status-mode-map (kbd "q") 'magit-quit-session))

(use-package exec-path-from-shell
  :defer t)
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-envs
   '("PATH")))

(setq eshell-prompt-regexp "^[^#$\n]*[#$] "
      eshell-prompt-function
      (lambda nil
        (concat
         (propertize (user-login-name) 'face '(:foreground "black")) " "
         (if (string= (eshell/pwd)
                      (getenv "HOME"))
             (propertize "~" 'face '(:foreground "blue"))
           (propertize (eshell/basename (eshell/pwd)) 'face '(:foreground "blue"))) 
         (if (= (user-uid) 0) "# "
           (concat  " $ " )))))

(setq ispell-program-name "/usr/local/bin/aspell")
(add-hook 'org-mode-hook 'flyspell-mode)
(setq abbrev-file-name             ;; tell emacs where to read abbrev
        "~/.emacs.d/abbrev_defs")    ;; definitions from...

(setq save-abbrevs t)
(setq-default abbrev-mode t)

;; http://ergoemacs.org/emacs/emacs_pretty_lambda.html
(defun pretty-lambda ()
  (setq prettify-symbols-alist
        '(
          ("lambda" . 955))))
(add-hook 'scheme-mode-hook 'prettify-symbols-mode)
(add-hook 'scheme-mode-hook 'pretty-lambda)
(add-hook 'clojure-mode-hook 'prettify-symbols-mode)
(add-hook 'clojure-mode-hook 'pretty-lambda)
(add-hook 'emacs-lisp-mode-hook 'pretty-lambda)
(add-hook 'emacs-lisp-mode-hook 'prettify-symbols-mode)
(add-hook 'lisp-mode-hook 'pretty-lambda)
(add-hook 'lisp-mode-hook 'prettify-symbols-mode)
(add-hook 'prog-mode-hook 'pretty-lambda )
(add-hook 'prog-mode-hook 'prettify-symbols-mode)
(use-package yasnippet
  :ensure t
  :defer t)

(yas-global-mode t)
(setq yas-trigger-key "<tab>")


;; helm customizations
(define-key helm-map (kbd "<left>") 'helm-find-files-up-one-level)
(define-key helm-map (kbd "<right>") 'helm-execute-persistent-action)


(add-hook 'emacs-lisp-mode-hook 'paredit-mode)
