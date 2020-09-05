(require 'use-package)

(defun setup-emoji (frame)
  (set-fontset-font t 'symbol "Noto Emoji" frame 'prepend))

(use-package eziam-theme
  :defer t
  :init
  (load-theme 'eziam-dark t)

  (line-number-mode 1)
  (column-number-mode 1)
  (global-hl-line-mode 1)

  (create-fontset-from-fontset-spec standard-fontset-spec)

  (add-to-list 'default-frame-alist '(font . "IBM Plex Mono-11"))
  (add-to-list 'default-frame-alist '(cursor-color . "white"))
  (add-hook 'after-make-frame-functions 'setup-emoji))

(provide 'setup-theme)
