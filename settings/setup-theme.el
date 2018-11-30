(require 'use-package)

(defun setup-emoji (frame)
  (set-fontset-font t 'symbol "Noto Emoji" frame 'prepend))

(use-package gruvbox-theme
  :defer t
  :ensure t
  :init

  (load-theme 'gruvbox-dark-hard t)

  (column-number-mode 1)
  (create-fontset-from-fontset-spec standard-fontset-spec)
  (setq-default line-spacing 1)
  (add-to-list 'default-frame-alist '(font . "Fantasque Sans Mono-13"))
  (add-to-list 'default-frame-alist '(cursor-color . "white"))
  (add-hook 'after-make-frame-functions 'setup-emoji)
  (setup-emoji nil))

(provide 'setup-theme)
