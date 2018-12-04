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

(require 'diminish)                ;; if you use :diminish
(require 'bind-key)                ;; if you use any :bind variant
(server-start)

;; load the org config file

(load-file "~/.emacs.d/init-org.elc")

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
