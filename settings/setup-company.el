(use-package company
  :ensure t
  :defer t
  :diminish company-mode
  :bind
  ("M-i" . company-complete)
  :init (global-company-mode)
  :config
  (setq company-minimum-prefix-length 1))

(use-package company-quickhelp
  :ensure t
  :defer t
  :init (add-hook 'global-company-mode-hook #'company-quickhelp-mode))

(provide 'setup-company)
