(use-package nim-mode
  :ensure t
  :init
  (add-hook 'nim-mode-hook 'nimsuggest-mode)
  (add-hook 'nimsuggest-mode-hook 'company-mode))

(provide 'setup-nim)
