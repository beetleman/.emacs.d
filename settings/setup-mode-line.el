(require 'use-package)
(use-package smart-mode-line
  :ensure t
  :config
  (setq sml/no-confirm-load-theme t)
  (setq sml/theme 'automatic)
  (sml/setup))
(provide 'setup-mode-line)
