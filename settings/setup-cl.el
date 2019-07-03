(use-package slime-company
  :ensure t)

(use-package slime
  :ensure t
  :init
  (setq slime-contribs '(slime-fancy
                         slime-indentation
                         slime-sbcl-exts
                         slime-company))
  (setq slime-complete-symbol*-fancy t)
  (setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol)
  (setq inferior-lisp-program "ros -Q run")
  (add-hook 'slime-repl-mode-hook #'smartparens-mode)
  :config
  (slime-setup))


(provide 'setup-cl)
