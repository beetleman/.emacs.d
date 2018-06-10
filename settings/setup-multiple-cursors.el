(require 'use-package)

(use-package multiple-cursors
  :ensure t
  :config
  (global-set-key (kbd "C-|") 'mc/edit-lines)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C->") 'mc/mark-all-like-this))

(provide 'setup-multiple-cursors)
