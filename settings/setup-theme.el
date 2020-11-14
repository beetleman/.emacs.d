(require 'use-package)

(line-number-mode 1)
(column-number-mode 1)
(global-hl-line-mode 1)

(use-package all-the-icons)


(use-package doom-themes
  :config
  (load-theme 'doom-one t)
  (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme

  (doom-themes-treemacs-config)
  (doom-themes-org-config)

  (let ((line (face-attribute 'mode-line :underline)))
    (set-face-attribute 'mode-line          nil :overline   line)
    (set-face-attribute 'mode-line-inactive nil :overline   line)
    (set-face-attribute 'mode-line-inactive nil :underline  line)
    (set-face-attribute 'mode-line          nil :box        nil)
    (set-face-attribute 'mode-line-inactive nil :box        nil)
    ;; for ligth "#f9f2d9"
    ;; for dark "#002b36"
    (set-face-attribute 'mode-line-inactive nil
                        :background (face-attribute 'hl-line :background))))

(add-to-list 'default-frame-alist '(font . "IBM Plex Mono-11"))
(add-to-list 'default-frame-alist '(cursor-color . "magenta"))

(defun --set-emoji-font (frame)
  "Adjust the font settings of FRAME so Emacs can display: ✨🍆✨."
  (set-fontset-font t 'symbol "Noto Color Emoji" frame)
  (set-fontset-font t 'symbol "Symbola" frame 'append))


(--set-emoji-font nil)
(add-hook 'after-make-frame-functions '--set-emoji-font)

(use-package moody
  :config
  (setq x-underline-at-descent-line t)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))

(use-package minions
  :config (minions-mode 1))

(provide 'setup-theme)
