;;
;; Common settings
;;

(setq gc-cons-threshold 10000000)
(setq package-enable-at-startup nil) ; tells emacs not to load any packages before starting up

;;
;; Package initialization
;;

(require 'package)

;; the following lines tell emacs where on the internet to look up
;; for new packages.
(setq package-archives '(("org"       . "http://orgmode.org/elpa/")
                         ("melpa"     . "https://melpa.org/packages/")))

(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package) ; unless it is already installed
  (package-refresh-contents)               ; updage packages archive
  (package-install 'use-package))          ; and install the most recent version of use-package

;; use-package initialization
(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

;; auto install from ELPA sources
(setq use-package-always-ensure t)

;;
;; Evil - Extensible Vi layer for Emacs.
;;

(use-package evil
  :config
  (evil-mode 1))

;;
;; General - Convenience wrappers for keybindings.
;;

(use-package general
  :config
  (general-evil-setup)

  ;; Define main keymaps
  (define-prefix-command 'ffe-apps-map)
  (define-prefix-command 'ffe-buffers-map)
  (define-prefix-command 'ffe-files-map)
  (define-prefix-command 'ffe-ui-map)

  (general-define-key :keymaps 'ffe-buffers-map
		      "n" 'next-buffer
		      "p" 'previous-buffer)

  (general-define-key :keymaps 'ffe-apps-map
		      "d" 'dired
		      "u" 'undo-tree-visualize)

  (general-define-key :keymaps 'ffe-ui-map
		      "F" 'toggle-frame-fullscreen)

  ;; Main SPC-keymap
  (define-prefix-command 'ffe-spc-map)
  (general-define-key :keymaps 'ffe-spc-map
		      "a" '(ffe-apps-map :which-key "apps")
		      "b" '(ffe-buffers-map :which-key "buffers")
		      "f" '(ffe-files-map :which-key "files")
		      "w" '(evil-window-map :which-key "windows")
		      "U" '(ffe-ui-map :which-key "UI"))

  ;; now attach SPC-driven global keymap
  (general-mmap "SPC" 'ffe-spc-map)
  )

;;
;; Color themes (solarized)
;;

(use-package color-theme :ensure f)

(use-package color-theme-solarized
  :no-require t
  :config
  (load-theme 'solarized t)

  (add-hook 'after-make-frame-functions
          (lambda (frame)
            (let ((mode (if (display-graphic-p frame) 'light 'dark)))
              (set-frame-parameter frame 'background-mode mode)
              (set-terminal-parameter frame 'background-mode mode))
            (enable-theme 'solarized))))

;;
;; Treat undo history as a tree
;;

(use-package undo-tree
  :diminish (undo-tree-mode . ""))

;;
;; Emacs package that displays available keybindings in popup
;;

(use-package which-key
  :diminish (which-key-mode . "")
  :config
  (which-key-mode))

;;
;; Abo-abo stuff (ivy, consuel, swiper)
;;

(use-package counsel
  :diminish (ivy-mode . "")
  :general
  ("C-x C-f" 'counsel-find-file)
  ("M-x" 'counsel-M-x)
  (:keymaps 'ffe-spc-map
	    "SPC" 'counsel-M-x
	    "/" 'counsel-ag)
  (:keymaps 'ffe-files-map
	    "f" 'counsel-find-file)
  (:keymaps 'ffe-buffers-map
	    "b" 'ivy-switch-buffer)
  :config
  (ivy-mode 1))

;;
;; Modular in-buffer completion framework for Emacs
;;

(use-package company
  :diminish (company-mode . "")
  :config
  (add-hook 'after-init-hook 'global-company-mode))

;;
;; An extensible emacs dashboard
;;

(use-package dashboard
  :config
  (dashboard-setup-startup-hook))

;;
;; Project Interaction Library for Emacs
;;

(use-package projectile
  :diminish (projectile-mode . "")
  :init
  (setq projectile-completion-system 'ivy)
  :config
  (projectile-mode))

;;
;; NeoTree, Dired & icons
;;

(use-package all-the-icons)
(use-package all-the-icons-dired
  :config
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

(use-package neotree
  :general
  ;; fix neotree behaviour in Evil mode
  (general-evil-define-key 'normal neotree-mode-map
    "RET" 'neotree-enter
    "TAB" 'neotree-enter
    "q" 'neotree-hide)
  (:keymaps 'ffe-files-map
	    "t" 'neotree-toggle)
  :init
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  (setq neo-smart-open t)
  )

;;
;; Latest Org-mode from correct repo
;;

(use-package org
  :ensure org-plus-contrib
  :pin org)

;;
;; Custom
;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (all-the-icons projectile company which-key color-theme-solarized general evil use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
