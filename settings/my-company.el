(use-package company               
  :ensure t
  :defer t
  :init (global-company-mode)
  :config
  (setq company-minimum-prefix-length 2)
  :diminish company-mode)


(use-package company-quickhelp
  :ensure t
  :defer t
  :init (add-hook 'global-company-mode-hook #'company-quickhelp-mode))

(provide 'my-company)
