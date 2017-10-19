(require 'use-package)
(use-package tao-theme
  :defer t
  :ensure t
  :init
  (setq-default line-spacing 1)
  (load-theme 'tao-yang t)
  ;; (global-hl-line-mode)
  (column-number-mode 1)
  (add-to-list 'default-frame-alist '(font . "Fantasque Sans Mono-13")))

(provide 'setup-theme)
