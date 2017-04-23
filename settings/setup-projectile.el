(require 'use-package)
(use-package projectile
  :ensure t
  :init
  (projectile-mode 1))
(use-package counsel-projectile
  :ensure t
  :config
  (counsel-projectile-on))
(provide 'setup-projectile)
