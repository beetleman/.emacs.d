(use-package flycheck-clj-kondo
  :after (clojure-mode))

(use-package clojure-mode)

(use-package cider
  :hook ((cider-mode . clj-refactor-mode)
         (cider-mode . smartparens-mode)
         (cider-mode . eldoc-mode))
  :init
  (cider-auto-test-mode 1))

(use-package cider-eval-sexp-fu)

(use-package clj-refactor)

(use-package zprint-mode
  :hook (clojure-mode clojurescript-mode))

(provide 'setup-clojure)
