(setq user-full-name "Nick George")

(when window-system
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (tooltip-mode -1))

(setq inhibit-startup-message t)
(setq initial-scratch-message "")
(setq package-enable-at-startup nil)
(setq package-archives  '(("melpa" . "http://melpa.org/packages/")
                          ("gnu" . "http://elpa.gnu.org/packages/")
                          ("org" . "http://orgmode.org/elpa/")))
;; set up package
(eval-when-compile
  (require 'package)
  (package-initialize)
  (unless package-archive-contents
    (package-refresh-contents)
    (package-install 'use-package))
  (require 'use-package)
  (setq use-package-always-ensure t))

(eval-when-compile
  (require 'use-package))

;;(require 'diminish)                ;; if you use :diminish
;;(require 'bind-key)                ;; if you use any :bind variant
(server-start)

;; rainbows
(use-package rainbow-delimiters
  :defer t)

;; s package
(use-package s
  :defer t)

(use-package paredit)
(defalias 'yes-or-no-p 'y-or-n-p)

;; I keep hitting this on accident, so unbind
(global-unset-key (kbd "C-x C-c"))
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

(global-set-key (kbd "C-c t") 'switch-theme)

;; global keyboard shortcuts
(global-set-key (kbd "C-c y") 'kill-buffer-and-window) ;; kill buffer and window is C-c C-k
(global-set-key (kbd "C-c i") (lambda () (interactive) (find-file "~/.emacs.d/revised-init.el"))) ;; config file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

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

(helm-mode 1)
;;  use recent file stuff
(use-package recentf
  :bind ("C-x C-r" . helm-recentf)
  :defer t  
  :config
  (recentf-mode t)
  (setq recentf-max-saved-items 200))

(setq create-lockfiles nil)

(use-package flycheck
  :defer t
  :init (global-flycheck-mode))

(use-package company) ;; autocompletion
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

;; rust
(use-package rust-mode)
(use-package cargo)
(use-package flycheck-rust)
(use-package rustic)
(use-package lsp-mode
  :ensure
  :commands lsp
  :custom
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-eldoc-render-all t)
  (lsp-idle-delay 0.6)
  (lsp-rust-analyzer-server-display-inlay-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
  (lsp-rust-analyzer-display-parameter-hints t)
  :config (add-hook 'lsp-mode-hook 'lsp-ui-mode)  
  :hook
  (rustic-mode . lsp)
  (rust-mode . lsp))

(use-package lsp-ui
  :ensure
  :commands lsp-ui-mode
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable nil))

(add-to-list 'auto-mode-alist '("\\.rs\\'" . rustic-mode))

(add-hook 'rust-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c <tab>") #'rust-format-buffer)
            (add-hook 'before-save-hook 'rust-format-buffer)
            (linum-mode t)))

(with-eval-after-load 'rust-mode
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))


(setq gc-cons-threshold 800000)
