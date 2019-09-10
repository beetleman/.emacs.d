(require 'use-package)

(defun setup-emoji (frame)
  (set-fontset-font t 'symbol "Noto Emoji" frame 'prepend))

(use-package gruvbox-theme
  :defer t
  :ensure t
  :init
  (load-theme 'gruvbox-light-hard t t)
  (load-theme 'gruvbox-dark-hard t t)

  (use-package cycle-themes
    :ensure t
    :init (setq cycle-themes-theme-list
                '(gruvbox-dark-hard
                  gruvbox-light-hard))
    :config (cycle-themes-mode))

  (line-number-mode 1)
  (column-number-mode 1)
  (global-hl-line-mode 1)

  (create-fontset-from-fontset-spec standard-fontset-spec)
  (setq-default line-spacing 1)
  (add-to-list 'default-frame-alist '(font . "Fantasque Sans Mono-13"))
  (add-to-list 'default-frame-alist '(cursor-color . "white"))
  (add-hook 'after-make-frame-functions 'setup-emoji))

(provide 'setup-theme)
