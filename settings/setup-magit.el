(require 'use-package)
(use-package magit
  :bind
  ("C-x g" . magit-status))
(use-package evil-magit)
(provide 'setup-magit)
