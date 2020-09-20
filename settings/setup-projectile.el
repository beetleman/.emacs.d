(require 'use-package)


(use-package projectile
  :bind-keymap
  ("C-c p" . projectile-command-map))


(use-package counsel-projectile
  :config
  (counsel-projectile-mode 1))

(provide 'setup-projectile)
