(use-package flycheck-clj-kondo
  :ensure t)

(use-package clojure-mode
  :ensure t
  :config
  (require 'flycheck-clj-kondo))

(use-package cider
  :ensure t
  :defer t
  :init
  (cider-auto-test-mode 1)
  (add-hook 'cider-mode-hook #'clj-refactor-mode)
  (add-hook 'cider-mode-hook #'smartparens-mode)
  (add-hook 'cider-mode-hook #'eldoc-mode)
  :diminish subword-mode)

(use-package cider-eval-sexp-fu
  :ensure t)

(use-package clj-refactor
  :ensure t
  :diminish clj-refactor-mode)

(use-package zprint-mode
  :hook (clojure-mode clojurescript-mode))

(provide 'setup-clojure)
