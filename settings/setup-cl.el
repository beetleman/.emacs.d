(use-package slime
  :if (file-exists-p "~/.roswell/helper.el")
  :ensure slime-company
  :hook (slime-repl-mode . smartparens-mode)
  :init
  (load "~/.roswell/helper.el")
  (setq inferior-lisp-program "ros -Q run")
  :config (slime-setup '(slime-fancy
                         slime-indentation
                         slime-sbcl-exts
                         slime-company)))

(provide 'setup-cl)
