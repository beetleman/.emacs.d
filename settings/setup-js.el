(require 'use-package)

(use-package js2-mode
  :ensure t
  :init
  (use-package rjsx-mode
    :ensure t
    :init
    (add-to-list 'auto-mode-alist '("\\.jsx\\'" . rjsx-mode))
    (add-to-list 'auto-mode-alist '("\\.react.js\\'" . rjsx-mode))
    (add-to-list 'auto-mode-alist '("\\index.android.js\\'" . rjsx-mode))
    (add-to-list 'auto-mode-alist '("\\index.ios.js\\'" . rjsx-mode))
    (add-to-list 'magic-mode-alist '("/\\*\\* @jsx React\\.DOM \\*/" . rjsx-mode))
    (add-to-list 'magic-mode-alist '("^import React" . rjsx-mode))
    (set-face-attribute 'js2-warning nil
                        :foreground "black"
                        :background "yellow"
                        :underline nil)
    (set-face-attribute 'js2-error nil
                        :foreground "black"
                        :background "red"
                        :underline nil))

  (use-package tern
    :ensure t
    :diminish "T"
    :commands (tern-mode)
    :init
    (add-hook 'js-mode-hook 'tern-mode))

  (use-package company-tern
    :ensure t
    :config
    (add-to-list 'company-backends 'company-tern))

  (add-hook 'js2-mode-hook #'js2-imenu-extras-mode)
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode)))

(provide 'setup-js)
