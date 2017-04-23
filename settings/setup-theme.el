(require 'use-package)
(use-package monokai-theme
  :defer t
  :ensure t
  :init
  (load-theme 'monokai t)
  (global-hl-line-mode))
(provide 'setup-theme)
