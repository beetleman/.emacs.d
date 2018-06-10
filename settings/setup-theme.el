(require 'use-package)
(use-package tao-theme
  :defer t
  :ensure t
  :init
  (setq-default line-spacing 1)
  (load-theme 'tao-yin t)
  (global-hl-line-mode 1)
  (column-number-mode 1)
  (add-to-list 'default-frame-alist '(font . "Fantasque Sans Mono-13"))
  (add-to-list 'default-frame-alist '(cursor-color . "white")))

(provide 'setup-theme)
