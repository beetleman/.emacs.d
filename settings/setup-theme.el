(require 'use-package)
(use-package gruvbox-theme
  :defer t
  :ensure t
  :init
  (load-theme 'gruvbox t)
  (global-hl-line-mode)
  (column-number-mode 1)
  (add-to-list 'default-frame-alist '(font . "Fantasque Sans Mono-13")))

(provide 'setup-theme)
