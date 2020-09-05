(require 'use-package)
(use-package projectile
  :init
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(use-package counsel-projectile
  :config
  (counsel-projectile-mode 1))

(provide 'setup-projectile)
