;;begin initialization

;; Initial Setup ;;
(setq gc-cons-threshold 400000000)
(when window-system
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (tooltip-mode -1))

(menu-bar-mode -1)
(blink-cursor-mode 0)
(setq ring-bell-function 'ignore)

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

(show-paren-mode 1)
(global-hl-line-mode 1)
(setq uniquify-buffer-name-style 'forward)
;; Don't show native OS scroll bars for buffers because they're redundant
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(setq-default frame-title-format "%b (%f)")


(setq select-enable-clipboard t
      select-enable-primary t
      save-interprogram-paste-before-kill t
      apropos-do-all t
      mouse-yank-at-point t)
(setq-default indent-tabs-mode nil)
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory
                                               "backups"))))
(setq auto-save-default nil)

(set-face-attribute 'default nil :height 140)

(setq inhibit-startup-message t)
(setq initial-scratch-message "")
(setq package-enable-at-startup nil)
(setq package-archives  '(("melpa" . "http://melpa.org/packages/")
                          ("gnu" . "http://elpa.gnu.org/packages/")
                          ("org" . "http://orgmode.org/elpa/")))

(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (cl-letf (((symbol-function #'process-list) (lambda ())))
    ad-do-it))

(require 'cl-lib)

;; set up package
(eval-when-compile
  (require 'package)
  (package-initialize)
  (unless package-archive-contents
    (package-refresh-contents)
    (package-install 'use-package))
  (require 'use-package)
  (setq use-package-always-ensure t
        use-package-expand-minimally t))

(eval-when-compile
  (require 'use-package))

(server-start)
;; Use packages and configs ;;
(use-package smex
  :bind ("M-x" . smex)
  :config (smex-initialize)
  :init (setq smex-save-file (concat user-emacs-directory ".smex-items")))



(use-package helm
  :defer t
  :bind  (("M-a" . helm-M-x)
          ("C-x C-f" . helm-find-files)
          ("M-y" . helm-show-kill-ring)
          ("C-x b" . helm-buffers-list))
  :config (helm-mode 1)
  (progn
    (setq helm-buffers-fuzzy-matching t)
    (helm-mode 1)
    (define-key helm-map (kbd "<left>") 'helm-find-files-up-one-level)
    (define-key helm-map (kbd "<right>") 'helm-execute-persistent-action)))

(use-package recentf
  :bind ("C-x C-r" . helm-recentf)
  :config
  (recentf-mode t)
  (setq recentf-max-saved-items 200))

(use-package ibuffer
  :commands ibuffer
  :bind ("C-x C-b" . ibuffer)
  :config (define-ibuffer-column size-h
  (:name "Size" :inline t)
  (cond
   ((> (buffer-size) 1000000) (format "%7.3fM" (/ (buffer-size) 1000000.0)))
   ((> (buffer-size) 1000) (format "%7.3fk" (/ (buffer-size) 1000.0)))
   (t (format "%8d" (buffer-size)))))
  (setq ibuffer-default-sorting-mode 'recency)
  (setq display-buffer-alist '(("\\`\\*e?shell" display-buffer-pop-up-window)))
  (setq ibuffer-truncate-lines t)
  (add-hook 'ibuffer-mode-hook
          '(lambda ()
             (ibuffer-switch-to-saved-filter-groups "home")))
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
                    (mode . js-mode)))))))

(use-package ispell
  :config (setq ispell-program-name "/usr/local/bin/aspell"))

(use-package async)

(use-package flycheck
  :config (global-flycheck-mode))

(use-package rainbow-delimiters)

;; s package
(use-package s)

(use-package company)

(use-package paredit
  :config
  (add-hook 'clojure-mode-hook #'paredit-mode)
  (add-hook 'lisp-mode-hook #'paredit-mode)
  )

(use-package yasnippet
  :init (setq yas-trigger-key "<tab>")
  :config (yas-global-mode t))

(use-package magit
  :defer t
  :config
  (define-key magit-status-mode-map (kbd "q") 'magit-quit-session))

(use-package exec-path-from-shell
  :config (when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-envs
   '("PATH"))))

(use-package org
  :config (add-hook 'org-mode-hook 'flyspell-mode))

(use-package markdown-mode
  :config (add-hook 'markdown-mode-hook 'flyspell-mode)
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package web-mode
  :config (setq auto-mode-alist (append
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
    ("html" . (ac-source-words-in-buffer ac-source-abbrev)))))

(use-package cargo)

(use-package rust-mode)

(use-package lsp-mode
  :bind (:map lsp-mode-map
              ("C-c d" . lsp-describe-thing-at-point)))
(use-package rustic
  :config (require 'lsp-rust) 
  (setq rustic-format-on-save t)
  (setq lsp-rust-analyzer-completion-add-call-parenthesis nil)
  (setq lsp-rust-analyzer-proc-macro-enable t))


(use-package toml-mode)

;; Themes ;;

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

;; Custom Functions ;;

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

;; Misc configs ;;
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)
(global-unset-key (kbd "C-x C-c"))

(setq create-lockfiles nil)

(defalias 'yes-or-no-p 'y-or-n-p)

(setq save-abbrevs t)
(setq abbrev-file-name             ;; tell emacs where to read abbrev
        "~/.emacs.d/abbrev_defs")

(add-hook 'prog-mode-hook
       (lambda ()
         (abbrev-mode -1)))

(global-set-key (kbd "C-c t") 'switch-theme)

(global-set-key (kbd "C-c y") 'kill-buffer-and-window)

(global-set-key (kbd "C-c i") (lambda () (interactive) (find-file "~/.emacs.d/init.el")))
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

(setq gc-cons-threshold 800000)
;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(package-selected-packages
;;    (quote
;;     (leuven-theme material-theme projectile s rainbow-delimiters use-package org-link-minor-mode))))
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  )
