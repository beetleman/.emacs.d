(require 'use-package)

(defun setup-emoji (frame)
  (set-fontset-font t 'symbol "Noto Emoji" frame 'prepend))

(use-package alect-themes
  :defer t
  :ensure t
  :init
  (load-theme 'alect-black t)

  (column-number-mode 1)
  (line-number-mode 1)
  (global-hl-line-mode 1)

  (create-fontset-from-fontset-spec standard-fontset-spec)
  (setq-default line-spacing 1)
  (add-to-list 'default-frame-alist '(font . "Fantasque Sans Mono-13"))
  (add-to-list 'default-frame-alist '(cursor-color . "white"))
  (add-hook 'after-make-frame-functions 'setup-emoji))

(provide 'setup-theme)
