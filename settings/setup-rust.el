(require 'use-package)

(use-package rust-mode
  :ensure t
  :config)

(use-package cargo
  :ensure t
  :hook (rust-mode-hook . cargo-minor-mode))

(provide 'setup-rust)
