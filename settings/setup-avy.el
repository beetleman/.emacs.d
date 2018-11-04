(require 'use-package)
(use-package avy
  :ensure t
  :bind
  ("C-:" . avy-goto-char)
  ("M-g f" . avy-goto-line))
(provide 'setup-avy)
