(require 'use-package)
(use-package smart-mode-line
  :ensure t
  :config
  (setq sml/no-confirm-load-theme t)
  (sml/setup))
(provide 'setup-mode-line)
