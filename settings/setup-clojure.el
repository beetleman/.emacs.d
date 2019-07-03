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

(use-package flycheck-joker
  :ensure t
  :init
  (require 'flycheck-joker)
  (defun clj-joker-hook () (flycheck-mode 1))
  (add-hook 'clojure-mode-hook #'clj-joker-hook))

(provide 'setup-clojure)
