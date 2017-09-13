(load (expand-file-name "~/.roswell/helper.el"))

(use-package slime-company
  :defer t
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
  (slime-setup)
  (load "/home/beetleman/.roswell/lisp/quicklisp/clhs-use-local.el" t))


(provide 'setup-cl)
