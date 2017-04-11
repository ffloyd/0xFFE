;;
;; Initialize MELPA stuff
;;

(require 'package)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/")))

(package-initialize)

(when (not package-archive-contents)
    (package-refresh-contents))

;;
;; Configure temp files to not pollute workdirs
;;

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))

(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(message "Deleting old backup files...")

(let ((week (* 60 60 24 7))
      (current (float-time (current-time))))
  (dolist (file (directory-files temporary-file-directory t))
    (when (and (backup-file-name-p file)
               (> (- current (float-time (nth 5 (file-attributes file))))
                  week))
      (message "%s" file)
      (delete-file file))))

;;
;; Load or install packages
;;

(defvar ffe-packages
  '(
    color-theme-solarized ;; Solarized themes for Emacs

    helm            ;; Helm is an Emacs incremental and narrowing framework
    which-key       ;; Display available keybindings in popup
    evil            ;; Extensible Vi layer for Emacs.
    projectile      ;; Project Interaction Library for Emacs
    helm-projectile ;; Helm UI for Projectile
    dashboard       ;; An extensible emacs dashboard
    )
  "List of packages used by FFE")

(defun ffe-package-install (package)
  "Install PACKAGE (if not installed)"
  (unless (package-installed-p package)
    (package-install package)))

(mapc 'ffe-package-install ffe-packages)

;;
;; Solarized theme
;;

(load-theme 'solarized t)

;; Light theme for GUI, dark theme for Terminal
(add-hook 'after-make-frame-functions
          (lambda (frame)
            (let ((mode (if (display-graphic-p frame) 'light 'dark)))
              (set-frame-parameter frame 'background-mode mode)
              (set-terminal-parameter frame 'background-mode mode))
            (enable-theme 'solarized)))

;;
;; Evil
;;

(require 'evil)

(evil-mode 1)

;;
;; which-key
;;

(require 'which-key)

(which-key-mode)

;;
;; HELM
;;

(require 'helm-config)

(helm-mode 1)
(setq helm-mode-fuzzy-match t)

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)

;;
;; Projectile
;;

(require 'projectile)

(projectile-mode)

(require 'helm-projectile)

(helm-projectile-on)

;;
;; Dashboard
;;

(require 'dashboard)

(dashboard-setup-startup-hook)

;;
;; Keybindings
;;

;; Essential

(define-prefix-command 'ffe-main-map)

(define-key evil-motion-state-map (kbd "SPC") 'ffe-main-map)

(define-key ffe-main-map (kbd "SPC") 'helm-M-x)

;; Buffer manipulation

(define-prefix-command 'ffe-buffers-map)

(define-key ffe-main-map "b" 'ffe-buffers-map)

(define-key ffe-buffers-map "n" 'evil-next-buffer)
(define-key ffe-buffers-map "p" 'evil-prev-buffer)
(define-key ffe-buffers-map "d" 'evil-delete-buffer)
(define-key ffe-buffers-map "b" 'helm-mini)

;; Windows manipulation

(define-key ffe-main-map "w" 'evil-window-map)

;; File manipulation

(define-prefix-command 'ffe-files-map)

(define-key ffe-main-map "f" 'ffe-files-map)

(define-key ffe-files-map "f" 'helm-find-files)

;;
;; Custom
;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (evil which-key helm color-theme-solarized))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
