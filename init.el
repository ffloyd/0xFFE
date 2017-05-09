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
(setq package-archives '(("org"   . "http://orgmode.org/elpa/")
                         ("melpa" . "https://melpa.org/packages/")))

(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package) ;; unless it is already installed
  (package-refresh-contents)               ;; updage packages archive
  (package-install 'use-package))          ;; and install the most recent version of use-package

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
;; Emacs package that displays available keybindings in popup
;;

(use-package which-key
  :diminish (which-key-mode . "")
  :config
  (which-key-mode))

;;
;; General - Convenience wrappers for keybindings.
;;

(use-package general
  :config
  (progn
    (general-evil-setup)

    ;; Define main keymaps
    (define-prefix-command 'ffe-apps-map)
    (define-prefix-command 'ffe-buffers-map)
    (define-prefix-command 'ffe-files-map)
    (define-prefix-command 'ffe-mode-map)
    (define-prefix-command 'ffe-project-map)
    (define-prefix-command 'ffe-search-map)
    (define-prefix-command 'ffe-toggles-map)
    (define-prefix-command 'ffe-ui-map)

    (general-define-key :keymaps 'ffe-buffers-map
			"n" 'next-buffer
			"p" 'previous-buffer
			"d" 'kill-buffer)

    (general-define-key :keymaps 'ffe-apps-map
			"d" 'dired
			"u" 'undo-tree-visualize)

    (general-define-key :keymaps 'ffe-ui-map
			"F" 'toggle-frame-fullscreen
			"f" 'toggle-frame-maximized)

    ;; Main SPC-keymap
    (define-prefix-command 'ffe-spc-map)

    (general-define-key :keymaps 'ffe-spc-map
			"a" 'ffe-apps-map
			"b" 'ffe-buffers-map
			"f" 'ffe-files-map
			"m" 'ffe-mode-map
			"p" 'ffe-project-map
			"s" 'ffe-search-map
			"t" 'ffe-toggles-map
			"w" 'evil-window-map
			"U" 'ffe-ui-map)
    
    (which-key-add-key-based-replacements
      "SPC a" "app"
      "SPC b" "buffer"
      "SPC f" "file"
      "SPC m" "mode"
      "SPC p" "project"
      "SPC s" "search"
      "SPC t" "toggle"
      "SPC w" "window"
      "SPC U" "UI")

    ;; now attach SPC-driven global keymap
    (general-mmap "SPC" 'ffe-spc-map)))

;;
;; Color themes (solarized)
;;

(use-package color-theme :ensure f)

(use-package color-theme-solarized
  :no-require t
  :init
  (progn
    (setq-default frame-background-mode 'dark)
    (add-hook 'window-setup-hook
	      (lambda ()
		(load-theme 'solarized t)))))

(defun ffe/switch-theme-pallete ()
  "Switch between light and dark solarized theme variants"
  (interactive)
  (let ((next-mode (if (eq frame-background-mode 'dark)) 'light 'dark))
    (setq frame-background-mode next-mode)
    (load-theme 'solarized t)))
    
(general-define-key :keymaps 'ffe-ui-map
		    "T" 'ffe/switch-theme-pallete)

;;
;; Treat undo history as a tree
;;

(use-package undo-tree
  :diminish (undo-tree-mode . ""))

;;
;; Abo-abo stuff (ivy, consuel, swiper)
;;

(use-package counsel
  :diminish (ivy-mode . "")
  :bind (("C-s"     . swiper)
	 ("C-c C-r" . ivy-resume)
	 ("<f6>"    . ivy-resume)
	 ("M-x"     . counsel-M-x)
	 ("C-x C-f" . counsel-find-file)
	 ("<f1> f"  . counsel-describe-function)
	 ("<f1> l"  . counsel-find-library)
	 ("<f1> v"  . counsel-describe-variable)
	 ("<f2> i"  . counsel-info-lookup-symbol)
	 ("<f2> u"  . counsel-unicode-char)
	 ("C-c g"   . counsel-git)
	 ("C-c j"   . counsel-git-grep)
	 ("C-c k"   . counsel-ag)
	 ("C-x l"   . counsel-locate))
  :general
  (:keymaps 'ffe-spc-map
	    "SPC" 'counsel-M-x
	    "/"   'counsel-ag)
  (:keymaps 'ffe-buffers-map
	    "b"   'ivy-switch-buffer)
  (:keymaps 'ffe-files-map
	    "f"   'counsel-find-file
	    "r"   'counsel-recentf)
  (:keymaps 'ffe-search-map
	    "s"   'swiper)
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
  :general
  (:keymaps 'ffe-project-map
      "f" 'projectile-find-file
      "F" 'projectile-find-file-in-known-projects)
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
    "q"   'neotree-hide)
  (:keymaps 'ffe-files-map
      "t" 'neotree-toggle)
  :init
  (progn
    (setq neo-theme (if (display-graphic-p) 'icons 'arrow))  ;; this will use all-the-icons as icons source
    (setq neo-vc-integration '(face))
    (setq neo-smart-open t)))

;;
;; Graphviz files editing

(use-package graphviz-dot-mode
  :general
  (general-mmap :keymaps 'graphviz-dot-mode-map
		:prefix "SPC m"
		"c" 'compile
		"p" 'graphviz-dot-preview)
  :init
  (progn
    (setq graphviz-dot-auto-indent-on-braces  t)
    (setq graphviz-dot-auto-indent-on-newline t)
    (setq graphviz-dot-auto-indent-on-semi    t)))

;;
;; Latest Org-mode from correct repo
;;

(use-package org
  :ensure org-plus-contrib
  :pin org
  :init
  (progn
    (setq org-startup-truncated nil)))

;; Graphviz dot rendering inside org
(org-babel-do-load-languages
 'org-babel-load-languages
 '((dot . t)))

;;
;; Line numbers control
;;

(use-package linum
  :ensure f
  :general
  (:keymaps 'ffe-toggles-map
	    "n" 'linum-mode))

(use-package linum-relative
  :general
  (:keymaps 'ffe-toggles-map
	    "N" 'linum-relative-toggle))

;;
;; Lisp editing
;;

(use-package lispy)

(use-package evil-lispy
  :config
  (add-hook 'emacs-lisp-mode-hook #'evil-lispy-mode))

(use-package rainbow-delimiters
  :config
  (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode))

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
    (graphviz-dot-mode dot-mode rainbow-delimeters evil-lispy parinfer linum-relative all-the-icons projectile company which-key color-theme-solarized general evil use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 
