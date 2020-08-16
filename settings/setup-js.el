(require 'use-package)

(use-package js2-mode
  :ensure t
  :init
  (defun js2-mode-load-config ()
    (advice-add 'js--multi-line-declaration-indentation :around (lambda (orig-fun &rest args) nil))
    (setq js2-strict-missing-semi-warning nil)
    (define-key js2-mode-map (kbd "M-.") 'xref-find-definitions)
    (set-face-attribute 'js2-warning nil
                        :underline "yellow")
    (set-face-attribute 'js2-error nil
                        :underline "red"))
  (add-hook 'js2-mode-hook 'js2-mode-load-config)

  (use-package rjsx-mode
    :ensure t
    :init
    (add-to-list 'auto-mode-alist '("\\.jsx\\'" . rjsx-mode))
    (add-to-list 'auto-mode-alist '("\\.react.js\\'" . rjsx-mode))
    (add-to-list 'auto-mode-alist '("\\index.android.js\\'" . rjsx-mode))
    (add-to-list 'auto-mode-alist '("\\index.ios.js\\'" . rjsx-mode))
    (add-to-list 'magic-mode-alist '("/\\*\\* @jsx React\\.DOM \\*/" . rjsx-mode))
    (add-to-list 'magic-mode-alist '("^import React" . rjsx-mode))
    (add-hook 'rjsx-mode-hook 'js2-mode-load-config))

  (use-package add-node-modules-path
    :ensure t
    :init
    (add-hook 'js-mode-hook #'add-node-modules-path))

  (use-package prettier-js
    :ensure t
    :init
    (add-hook 'js-mode-hook #'prettier-js-mode))

  (add-hook 'js2-mode-hook #'js2-imenu-extras-mode)
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode)))


(use-package vue-mode
  :ensure t
  :mode "\\.vue\\'"
  :init
  (add-hook 'mmm-mode-hook
            (lambda ()
              (set-face-background 'mmm-default-submode-face nil))))


(provide 'setup-js)
