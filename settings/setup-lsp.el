(require 'use-package)

(use-package lsp-mode
  :hook ((python-mode . lsp)
         (rust-mode . lsp)
         (c-mode . lsp)
         (vue-mode . lsp)
         (web-mode . lsp)
         (reason-mode . lsp)
         (python-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

(use-package company-lsp :commands company-lsp)
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)


(provide 'setup-lsp)
