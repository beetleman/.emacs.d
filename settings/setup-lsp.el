(require 'use-package)

(use-package lsp-mode
  :ensure t
  :config
  (use-package company-lsp
    :ensure t
    :config
    (push 'company-lsp company-backends)))


(provide 'setup-lsp)
