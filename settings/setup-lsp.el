(require 'use-package)

(use-package lsp-mode
  :ensure t
  :config
  (require 'lsp-clients)
  (use-package company-lsp
    :ensure t
    :commands company-lsp))


(provide 'setup-lsp)
