;;
;; Solarized theme
;;

(ffe-use-package 'color-theme-solarized t)

(load-theme 'solarized t)

;; Light theme for GUI, dark theme for Terminal
(add-hook 'after-make-frame-functions
          (lambda (frame)
            (let ((mode (if (display-graphic-p frame) 'light 'dark)))
              (set-frame-parameter frame 'background-mode mode)
              (set-terminal-parameter frame 'background-mode mode))
            (enable-theme 'solarized)))

(provide 'ffe-color-themes)
