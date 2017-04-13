(ffe-use-package 'helm t)

(require 'helm-config)

(helm-mode 1)
(setq helm-mode-fuzzy-match t)

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)

(provide 'ffe-helm)
