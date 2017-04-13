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

(provide 'ffe-keybindings)
