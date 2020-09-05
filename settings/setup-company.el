(use-package company
  :diminish company-mode
  :bind
  ("M-i" . company-complete)
  :config
  (setq company-tooltip-align-annotations t)
  (setq company-minimum-prefix-length 1)
  :init (global-company-mode))

(use-package company-quickhelp
  :init
  (company-quickhelp-mode))

(provide 'setup-company)
