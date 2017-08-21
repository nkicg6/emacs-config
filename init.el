;;begin initialization
;; setup with help from :
;; https://www.youtube.com/watch?v=gRb3bq0NiXY&t=12s
(setq gc-cons-threshold 400000000)
(when window-system
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (tooltip-mode -1))

(setq inhibit-startup-message t)
(setq initial-scratch-message "")

;; set up package
(require 'package)
(add-to-list 'package-archives
  '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/")t)
;; from https://emacs.stackexchange.com/questions/8182/how-to-use-org-plus-contrib
;; below recommended from http://orgmode.org/elpa.html
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(setq package-load-list '(all))
;; verify org installed
(unless (package-installed-p 'org)
  (package-install 'org))
;; initialize packages
(package-initialize)

;;Bootstrap use-package
;; Install use-package if its not already there
;; use package to configure the rest of the packages. 

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;;From use-package README
(eval-when-compile
  (require 'use-package))
(require 'diminish)                ;; if you use :diminish
(require 'bind-key)                ;; if you use any :bind variant
(server-start)
;; load the org config file

(org-babel-load-file (concat user-emacs-directory "config.org"))

(setq gc-cons-threshold 800000)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (leuven-theme material-theme projectile s rainbow-delimiters use-package org-link-minor-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
