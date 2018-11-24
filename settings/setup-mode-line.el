(require 'use-package)
(use-package smart-mode-line
  :ensure t
  :config
  (setq sml/no-confirm-load-theme t)
  (setq sml/theme 'dark)
  (sml/setup)
  (use-package nyan-mode
    :ensure t
    :config
    (nyan-mode +1)))
(provide 'setup-mode-line)
