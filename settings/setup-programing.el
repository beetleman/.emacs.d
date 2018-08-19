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

(use-package flycheck
  :ensure t
  :config
  (defun flycheck-load-config ()
    (set-face-attribute 'flycheck-warning nil
                        :underline "yellow")
    (set-face-attribute 'flycheck-error nil
                        :underline "red"))
  (add-hook 'flycheck-mode-hook 'flycheck-load-config)
  :init
  (global-flycheck-mode))

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

(use-package whitespace
  :ensure t
  :init
  (add-hook 'before-save-hook 'whitespace-cleanup))

(provide 'setup-programing)
