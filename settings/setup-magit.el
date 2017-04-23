(require 'use-package)
(use-package magit
  :ensure t
  :bind
  ("C-x g" . magit-status))
(use-package evil-magit
  :ensure t)
(provide 'setup-magit)
