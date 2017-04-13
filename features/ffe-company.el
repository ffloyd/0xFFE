(ffe-use-packages '(company company-quickhelp))

(add-hook 'after-init-hook 'global-company-mode)

(company-quickhelp-mode)

(provide 'ffe-company)
