(require 'use-package)

(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook 'org-bullets-mode))

(provide 'setup-org)
