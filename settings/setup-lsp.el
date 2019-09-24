(require 'use-package)

(use-package eglot
  :ensure t
  :hook
  (rust-mode . eglot-ensure)
  (c-mode . eglot-ensure)
  (js2-mode . eglot-ensure)
  (rjsx-mode . eglot-ensure)
  (python-mode . eglot-ensure))

(provide 'setup-lsp)
