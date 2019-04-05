(require 'use-package)

(use-package lsp-mode
  :ensure t
  :config
  (require 'lsp-clients)
  (setq lsp-prefer-flymake nil
        lsp-ui-sideline-enable nil
        lsp-ui-imenu-enable nil
        lsp-ui-doc-enable nil
        lsp-ui-peek-enable nil)

  (use-package lsp-ui
    :ensure t)

  (use-package company-lsp
    :ensure t
    :commands company-lsp))

(provide 'setup-lsp)
