(require 'use-package)
(use-package magit
  :ensure t
  :bind
  ("C-x g" . magit-status))
(use-package evil-magit
  :ensure t)

(use-package diff-hl
  :ensure t
  :init
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  (global-diff-hl-mode))

(provide 'setup-magit)
