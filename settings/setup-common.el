(use-package better-defaults
  :defer t
  :ensure t
  :init
  (require 'better-defaults))

(use-package anzu
  :defer t
  :ensure t
  :init
  (global-anzu-mode 1)
  (global-set-key [remap query-replace] 'anzu-query-replace)
  (global-set-key [remap query-replace-regexp] 'anzu-query-replace-regexp))

(use-package undo-tree
  :defer t
  :ensure t
  :init
  (global-undo-tree-mode 1))

(global-auto-revert-mode 1)
(winner-mode 1)
(setq x-select-enable-clipboard-manager  nil)
(provide 'setup-common)
