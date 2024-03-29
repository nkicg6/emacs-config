; simplified init file.
(setq user-full-name "Nick George")

(defun my/tangle-dotfiles ()
   "If the current file is this file, the code blocks are tangled."
   (when (equal (buffer-file-name) (expand-file-name "~/.emacs.d/revised-init.el"))
     (byte-compile-file "~/.emacs.d/revised-init.el")))

(defun writing-large ()
  "For large screens."
  (interactive)
  (set-frame-size (selected-frame) 120 (x-display-pixel-height))
  (set-frame-position (selected-frame) 480 0)
  (message "writing-large"))

(defun writing-small ()
  "For small screens."
  (interactive)
  (set-frame-size (selected-frame) 120 (x-display-pixel-height))
  (set-frame-position (selected-frame) 280 0)
    (message "writing-large"))

(defun writing ()
  "Set frame to centered and a nice size for writing."
  (interactive)
  (if (< (x-display-pixel-width) 1500)
      (writing-small) (writing-large)))

(size-indication-mode 1)

(add-hook 'after-save-hook #'my/tangle-dotfiles)

;;;; Load custom packages ;;;;

(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))


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

;; (setq x-select-enable-clipboard t)

(use-package paredit)

(use-package openwith
  :config
  (setq openwith-associations '(("\\.pptx?\\'" "open" (file))
                              ("\\.docx?\\'" "open" (file))
                              ("\\.xlsx?\\'" "open" (file))
                              ("\\.ods?\\'" "open" (file))))
  (openwith-mode t)
  :defer t)

(defalias 'yes-or-no-p 'y-or-n-p)

;; I keep hitting this on accident, so unbind
(global-unset-key (kbd "C-x C-c"))

;; Theme

(use-package material-theme
  :defer t)

(use-package zenburn-theme
  :defer t)

(use-package solarized-theme
  :defer t
  :init
  (setq solarized-use-variable-pitch nil))

(use-package leuven-theme
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
  "Disable any currently active themes listed in `custom-enabled-themes'."
  (interactive)
  (mapc #'disable-theme custom-enabled-themes))

(global-set-key (kbd "C-c t") 'switch-theme)

;; global keyboard shortcuts
(global-set-key (kbd "C-c y") 'kill-buffer-and-window) ;; kill buffer and window is C-c C-k
(global-set-key (kbd "C-c c")'org-capture) ;; start org capture.
(global-set-key (kbd "C-c m") (lambda () (interactive) (find-file "~/Dropbox/orgs/master_agenda.org"))) ;; master agenda in org.
(global-set-key (kbd "C-c o") (lambda () (interactive) (find-file "~/Dropbox/orgs/org.org"))) ;; master agenda in org.
(global-set-key (kbd "C-c p") (lambda () (interactive) (find-file "~/Dropbox/lab_notebook/projects_and_data/mnc/publication/mnc/paper.org")))
(global-set-key (kbd "C-c i") (lambda () (interactive) (find-file "~/.emacs.d/revised-init.el"))) ;; config file
(global-set-key (kbd "C-c l") (lambda () (interactive) (find-file "~/Dropbox/lab_notebook/lab_notebook.org"))) ;; lab notebook in org.

;; quickly jump to mnc project in helm buffer
(defun mnc ()
  "Quickly jump to helm file view for mnc project."
  (interactive)
  (dired "~/Dropbox/lab_notebook/projects_and_data/mnc"))

(bind-key "C-c l" 'org-store-link)
(bind-key "C-c c" 'org-capture)

;; org agenda setup

;; utility fn to remove all scheduled tasks
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

;; end agenda setup

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; Turn off the menu bar at the top of each frame because it's distracting
(menu-bar-mode -1)
;; Show line numbers
;; (global-display-line-numbers-mode t)
;; remove toolbar
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

;; No cursor blinking
(blink-cursor-mode 0)

;; full path in title bar
(setq-default frame-title-format "%b (%f)")

;; frame size
(if (window-system) (set-frame-size (selected-frame) 100 50))

;; no bell
(setq ring-bell-function 'ignore)

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
                     (mode . ess-r-mode)
                     (mode . scheme-mode)
                     (mode . clojure-mode)
                     (mode . clojurescript-mode)
                     (mode . emacs-lisp-mode)
                     (mode . prog-mode)
                     (mode . c-mode)
                     (mode . sh-mode)
                     (filename . "*.rs")
                     (mode . rustic-mode)
                     (filename . "*.sh")
                     (filename . "Makefile")
                      (mode . rust-mode)))
         ("shell/REPL" (or (mode . eshell-mode)
                           (mode . cider-repl-mode)
                           (mode . comint-mode)
                           (filename . "\*Python\*")))
         ("web" (or (mode . web-mode)
                    (mode .js-mode))))))

(add-hook 'ibuffer-mode-hook
          '(lambda ()
             (ibuffer-switch-to-saved-filter-groups "home")))
(setq ibuffer-truncate-lines t)

;; Enhances M-x to allow easier execution of commands. Provides
;; a filterable list of possible commands in the minibuffer
;; http://www.emacswiki.org/emacs/Smex
(use-package smex
  :bind 
  ("M-x" . smex)
  :config
  (smex-initialize)
  :init
  (setq smex-save-file (concat user-emacs-directory ".smex-items")))

(use-package helm
  :defer t
  :bind  (("M-a" . helm-M-x)
          ("C-x C-f" . helm-find-files)
          ("M-y" . helm-show-kill-ring)
          ("C-x b" . helm-buffers-list))
  :config (progn
            (setq helm-buffers-fuzzy-matching t)
            (helm-mode 1)
            (define-key helm-map (kbd "<left>") 'helm-find-files-up-one-level)
            (define-key helm-map (kbd "<right>") 'helm-execute-persistent-action)))

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
  :defer t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;; web-mode http://web-mode.org/

(use-package web-mode)
(setq auto-mode-alist (append
                       '(("\\.html\\'" . web-mode)
                         ("\\.css\\'" . web-mode)
                         ("index\\'" . web-mode))
                       auto-mode-alist))
(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-enable-auto-pairing t)
(setq web-mode-enable-css-colorization t)
(setq web-mode-enable-current-element-highlight t)
(setq web-mode-enable-current-column-highlight t)

(setq web-mode-ac-sources-alist
  '(("css" . (ac-source-css-property))
    ("html" . (ac-source-words-in-buffer ac-source-abbrev))))
;; regular python stuff

(defun hs-mode-and-hide ()
  "Turn on code folding and folds all code blocks."
  (interactive)
  (hs-minor-mode)
  (hs-hide-all))

(use-package python-mode
  :defer t
  :config
  (add-hook 'python-mode-hook 'hs-mode-and-hide)
  (add-hook 'python-mode-hook 'blacken-mode)
  (define-key python-mode-map (kbd "C-c h") 'hs-hide-all)
  (define-key python-mode-map (kbd "C-c s") 'hs-show-all)
  (define-key python-mode-map (kbd "C-<tab>") 'hs-toggle-hiding))

;; python environment
(use-package elpy
  :defer t
  :config
  (setenv "WORKON_HOME" "~/.ve")
  (add-hook 'elpy-mode-hook 'blacken-mode)
  (setq elpy-shell-echo-output nil
      python-shell-interpreter "ipython"
      python-shell-interpreter-args "--simple-prompt")
  
  (elpy-enable)
  :init
  (add-hook 'python-mode-hook 'elpy-mode)
  (setq python-shell-prompt-detect-failure-warning nil)
  (setq python-shell-completion-native-enable nil) ; stop annoying warning. 
  )
(use-package pyenv-mode)

;; (setq elpy-shell-echo-output nil
;;       python-shell-interpreter "python3"   
;;       python-shell-interpreter-args "")

;; (setq elpy-shell-echo-output nil
;;       python-shell-interpreter "python3"
;;       python-shell-interpreter-args "-i")

(use-package blacken)

;; highlight indentation off, only use current column
(highlight-indentation-mode nil)
(add-hook 'python-mode-hook 'highlight-indentation-current-column-mode)

(use-package flycheck
  :defer t
  :init (global-flycheck-mode))

(use-package company-jedi)
(defun my/python-mode-hook ()
  (add-to-list 'company-backends 'company-jedi))

(add-hook 'python-mode-hook 'my/python-mode-hook)

;; for org babel
(setq org-babel-python-command "python3")

(setq ansi-color-for-comint-mode t)

;; python

(add-hook 'python-mode-hook 'rainbow-delimiters-mode)
(add-hook 'python-mode-hook 'electric-pair-mode)

;; Automatically remove trailing whitespace when file is saved.
(add-hook 'python-mode-hook
      (lambda()
        (add-hook 'local-write-file-hooks
              '(lambda()
                 (save-excursion
                   (delete-trailing-whitespace))))))

(setq flycheck-python-pycompile-executable "/usr/local/bin/python3")


(setq org-export-with-sub-superscripts '{})
(setq org-use-sub-superscripts '{})

(setq org-src-window-setup (quote current-window))
;; auto open org files in org mode.
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode)) ;; auto activate org mode for org docs.

(setq org-startup-with-inline-images t) ;; for inline code images in python


(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "WAITING(w)"  "NEXT(n)" "|" "DONE(d)" "CANCELLED(c)" ))))
;; log time on finish
(setq org-log-done 'time)

(setq org-todo-keyword-faces
      '(("TODO" :foreground "red" :weight bold)
        ("NEXT" :foreground "black" :background "yellow" :weight bold)
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

;; electric pairs rock!
(add-hook 'org-mode-hook 'electric-pair-mode)
(use-package org-bullets
  :defer t)
;; latex headlines
(setq org-latex-prefer-use-labels t)
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
(add-hook 'clojure-mode-hook #'paredit-mode)
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

(use-package ess
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

;; also new YASnippet for assignment <- which is -<TAB>

;; I do not like the underscore replace behavior. 
;;(ess-toggle-underscore nil)

(use-package magit
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

(setq display-buffer-alist '(("\\`\\*e?shell" display-buffer-pop-up-window)))

(setq ispell-program-name "/usr/local/bin/aspell")
(add-hook 'org-mode-hook 'flyspell-mode)
(add-hook 'markdown-mode-hook 'flyspell-mode)
(setq abbrev-file-name             ;; tell emacs where to read abbrev
        "~/.emacs.d/abbrev_defs")    ;; definitions from...

(setq save-abbrevs t)

(add-hook 'prog-mode-hook
       (lambda ()
         (abbrev-mode -1)))
(add-hook 'org-mode-hook #'abbrev-mode)
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
  :init (setq yas-trigger-key "<tab>")
  :config (yas-global-mode t))

;; NEURON settings
(add-to-list 'auto-mode-alist '("\\.hoc$" . c-mode))
(add-to-list 'auto-mode-alist '("\\.mod$" . c-mode))
(add-to-list 'auto-mode-alist '("\\.ses$" . c-mode))
(add-hook 'c-mode-hook 'company-mode)
(define-key c-mode-map (kbd "C-<tab>") #'company-complete)

(add-hook 'emacs-lisp-mode-hook 'paredit-mode)

;;; publishing and org-ref

(with-eval-after-load 'ox-latex
   (add-to-list 'org-latex-classes
                '("elifecustom"
                "\\documentclass[9pt,lineno]{elife} [NO-DEFAULT-PACKAGES]"
             ("\\section{%s}" . "\\section{%s}")
             ("\\subsection{%s}" . "\\subsection{%s}")
             ("\\subsubsection{%s}" . "\\subsubsection{%s}")
             ("\\paragraph{%s}" . "\\paragraph{%s}")
             ("\\subparagraph{%s}" . "\\subparagraph{%s}"))))

(use-package helm-bibtex
  :init (setq bibtex-completion-bibliography
      '("/Users/nick/Dropbox/bibliography/zotero-library.bib")))

(use-package org-ref
  :after org
  :init
  (setq reftex-default-bibliography '("/Users/Nick/Dropbox/bibliography/zotero-library.bib"))
  (setq org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "bibtex %b"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))  
  :config
  (setq reftex-default-bibliography '("/Users/nick/Dropbox/bibliography/zotero-lib.bib"))
  (bind-key* "C-c ]" 'org-ref-helm-insert-cite-link)
  (setq org-ref-pdf-directory '("~/PDFs"))
  (require 'org-ref))

(setq org-export-cording-system 'utf-8)

(setq bibtex2html-program "/usr/local/bin/bibtex2html")

;;; end publishing

;; go

(use-package go-mode
  :config (add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))
  (add-hook 'before-save-hook 'gofmt-before-save)
  )

;; rust
;; https://youtu.be/omJhc7zprNs?t=337
(use-package cargo)

(use-package rustic
  :ensure t
  :config 
  (require 'lsp-rust) 
  (setq rustic-format-on-save t)
  (setq lsp-rust-analyzer-completion-add-call-parenthesis nil)
  (setq lsp-rust-analyzer-proc-macro-enable t))

(use-package lsp-mode
  :ensure t
  :bind (:map lsp-mode-map
              ("C-c d" . lsp-describe-thing-at-point)))

(use-package toml-mode :ensure)
