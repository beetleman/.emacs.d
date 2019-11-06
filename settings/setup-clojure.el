(use-package clojure-mode
  :ensure t
  :mode (("\\.clj\\'" . clojure-mode)
         ("\\.cljc\\'" . clojure-mode)
         ("\\.cljs\\'" . clojurescript-mode)
         ("\\.edn\\'" . clojure-mode)))

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
  :ensure t
  :defer t)

(use-package clj-refactor
  :defer t
  :ensure t
  :diminish clj-refactor-mode)

(use-package flymake-quickdef
  :ensure t)

;;TODO: move to seperate module
(use-package flymake-joker
  :load-path "packages/flymake-joker"
  :after flymake-quickdef
  :init
  (add-hook 'clojure-mode-hook #'flymake-joker-clj-enable)
  (add-hook 'clojurescript-mode-hook #'flymake-joker-cljs-enable)
  (add-hook 'clojure-mode-hook #'flymake-mode))

(provide 'setup-clojure)
