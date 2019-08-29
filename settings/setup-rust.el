(require 'use-package)

(use-package rust-mode
  :ensure t
  :defer t
  :config
  (use-package cargo
    :ensure t)
  (use-package flycheck-rust
    :ensure t
    :config
    (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))
  (add-hook 'rust-mode-hook #'lsp))

(provide 'setup-rust)
