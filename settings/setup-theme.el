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
  (add-to-list 'default-frame-alist '(cursor-color . "white"))
  (when (member "Noto Emoji" (font-family-list))
    (set-fontset-font t 'unicode "Noto Emoji" nil 'prepend)))

(provide 'setup-theme)
