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
;; Package control functions
;;

(defun ffe-use-package (package &optional no-require)
  "Install PACKAGE (if not installed) and requires it"
  (unless (package-installed-p package)
    (package-install package))
  (unless no-require
    (require package)))

(defun ffe-use-packages (packages)
  "Install PACKAGES (if not installed) and requires them"
  (mapc 'ffe-use-package packages))


(provide 'ffe-package)
