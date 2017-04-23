(require 'use-package)
(use-package projectile
  :init
  (projectile-mode 1))
(use-package counsel-projectile
  :config
  (counsel-projectile-on))
(provide 'setup-projectile)
