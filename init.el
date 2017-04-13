;;
;; Initialize load paths
;;

;; (package-initialize)

(defvar ffe-dir (file-name-directory load-file-name)
  "Root dir of FFE")

(defvar ffe-core-dir (expand-file-name "ffe" ffe-dir)
  "FFE core files (functions for internal usage, basic EMACS non-UI tuning)")

(defvar ffe-features-dir (expand-file-name "features" ffe-dir)
  "FFE files with UI and packages configuration, EMACS UI-tuning and so on")

(add-to-list 'load-path ffe-core-dir)
(add-to-list 'load-path ffe-features-dir)

;; Always load newer bytecode
(setq load-prefer-newer t)

;;
;; Load FFE core configs
;;

(require 'ffe-package)
(require 'ffe-backups)

;;
;; Load FFE features
;;

(require 'ffe-color-themes)
(require 'ffe-evil)
(require 'ffe-helm)
(require 'ffe-projectile)
(require 'ffe-which-key)
(require 'ffe-neotree)
(require 'ffe-company)
(require 'ffe-dashboard)
(require 'ffe-keybindings)

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
    (all-the-icons evil which-key helm color-theme-solarized))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
