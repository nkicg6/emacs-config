;; simple init file for terminal emacs.

(setq inhibit-startup-message t)
(setq initial-scratch-message "")
(setq package-enable-at-startup nil)
(setq package-archives  '(("melpa" . "http://melpa.org/packages/")))
(eval-when-compile
  (require 'package)
  (package-initialize)
  (unless package-archive-contents
    (package-refresh-contents)
    (package-install 'use-package))
  (require 'use-package)
  (setq use-package-always-ensure t))

(setq user-full-name "Nick George"
      user-mail-address "nicholas.m.george@cuanschutz.edu")

;; rainbows
(use-package rainbow-delimiters
  :defer t)
;; s package
(use-package s
  :defer t)
(defalias 'yes-or-no-p 'y-or-n-p)

(global-set-key (kbd "C-c y") 'kill-buffer-and-window) ;; kill buffer and window is C-c C-k

(global-set-key (kbd "C-c m") (lambda () (interactive) (find-file "~/Dropbox/orgs/master_agenda.org"))) ;; master agenda in org.
(global-set-key (kbd "C-c p") (lambda () (interactive) (find-file "~/Dropbox/orgs/planner.org")))
(global-set-key (kbd "C-c i") (lambda () (interactive) (find-file "~/.emacs.d/revised-init.el"))) ;; config file

(global-set-key (kbd "C-c i") (lambda () (interactive) (find-file "~/.emacs.d/terminal_init.el"))) ;; config file

(use-package nlinum
  :defer t)
(global-nlinum-mode)
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

(blink-cursor-mode 0)
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

(define-key helm-map (kbd "<left>") 'helm-find-files-up-one-level)
(define-key helm-map (kbd "<right>") 'helm-execute-persistent-action)

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
  "Disable any currently active themes listed in `custom-enabled-themes'."
  (interactive)
  (mapc #'disable-theme custom-enabled-themes))

(global-set-key (kbd "C-c t") 'switch-theme)

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
                     (mode . c-mode)))
         ("shell/REPL" (or (mode . eshell-mode)
                           (mode . cider-repl-mode)
                           (mode . comint-mode))))))
(add-hook 'ibuffer-mode-hook
          '(lambda ()
             (ibuffer-switch-to-saved-filter-groups "home")))
(setq ibuffer-truncate-lines t)

(use-package smex
  :defer t
  :bind 
  ("M-x" . smex)
  :config
  (smex-initialize)
  :init
  (setq smex-save-file (concat user-emacs-directory ".smex-items")))
