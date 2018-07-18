(require 'use-package)

(defun setup-emoji (frame)
  (set-fontset-font t 'symbol "Noto Emoji" frame 'prepend))

(use-package tao-theme
  :defer t
  :ensure t
  :init
  (setq-default line-spacing 1)
  (load-theme 'tao-yin t)
  (global-hl-line-mode 1)
  (column-number-mode 1)
  (create-fontset-from-fontset-spec standard-fontset-spec)
  (add-to-list 'default-frame-alist '(font . "Fantasque Sans Mono-13"))
  (add-to-list 'default-frame-alist '(cursor-color . "white"))
  (add-hook 'after-make-frame-functions 'setup-emoji)
  (setup-emoji nil))

(provide 'setup-theme)
