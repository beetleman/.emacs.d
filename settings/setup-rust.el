(require 'use-package)

(use-package rust-mode)

(use-package cargo
  :hook (rust-mode . cargo-minor-mode))

(provide 'setup-rust)
