(require 'use-package)
(use-package magit
  :bind
  ("C-x g" . magit-status))

(use-package diff-hl
  :hook (magit-post-refresh . diff-hl-magit-post-refresh)
  :init
  (global-diff-hl-mode)
  (diff-hl-margin-mode))

(provide 'setup-magit)
