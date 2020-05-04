(require 'use-package)

(use-package smartparens-config
  :ensure smartparens
  :diminish smartparens-mode
  :config
  (smartparens-global-mode t)
  (sp-use-smartparens-bindings)
  (add-hook 'prog-mode-hook 'turn-on-smartparens-strict-mode)
  (add-hook 'markdown-mode-hook 'turn-on-smartparens-strict-mode))

(use-package eldoc
  :diminish eldoc
  :ensure t
  :init
  (add-hook 'prog-mode-hook #'eldoc-mode))

(use-package subword
  :ensure t
  :init
  (add-hook 'prog-mode-hook #'subword-mode))

(use-package yasnippet
  :ensure t
  :diminish yasnippet
  :init
  (yas-global-mode)
  :config
  (use-package yasnippet-snippets
    :ensure t
    :init
    (yas-reload-all)))

(use-package ws-butler
  :ensure t
  :hook (prog-mode . ws-butler-mode))

(use-package direnv
  :ensure t
  :config
 (direnv-mode))

(provide 'setup-programing)
