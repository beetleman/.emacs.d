(require 'use-package)

(defun setup-emoji (frame)
  (set-fontset-font t 'symbol "Noto Emoji" frame 'prepend))

(use-package solarized-theme
  :defer t
  :init
  (setq solarized-distinct-fringe-background t)
  (load-theme 'solarized-light t)
  
  (let ((line (face-attribute 'mode-line :underline)))
    (set-face-attribute 'mode-line          nil :overline   line)
    (set-face-attribute 'mode-line-inactive nil :overline   line)
    (set-face-attribute 'mode-line-inactive nil :underline  line)
    (set-face-attribute 'mode-line          nil :box        nil)
    (set-face-attribute 'mode-line-inactive nil :box        nil)
    (set-face-attribute 'mode-line-inactive nil :background "#f9f2d9"))
  
  (line-number-mode 1)
  (column-number-mode 1)
  (global-hl-line-mode 1)

  (create-fontset-from-fontset-spec standard-fontset-spec)

  (add-to-list 'default-frame-alist '(font . "IBM Plex Mono-11"))
  (add-to-list 'default-frame-alist '(cursor-color . "pink"))
  (add-hook 'after-make-frame-functions 'setup-emoji))

(use-package moody
  :config
  (setq x-underline-at-descent-line t)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))

(use-package minions
  :config (minions-mode 1))

(provide 'setup-theme)
