(require 'use-package)

(use-package smartparens
  :hook ((prog-mode . smartparens-strict-mode)
         (cider-repl-mode . smartparens-strict-mode)
         (markdown-mode . smartparens-strict-mode))
  :config
  (require 'smartparens-config)
  (sp-use-paredit-bindings))

(use-package eldoc
  :hook (prog-mode . eldoc-mode))

(use-package subword
  :hook (prog-mode . subword-mode))

(use-package yasnippet
  :init
  (yas-global-mode))

(use-package yasnippet-snippets
  :after (yasnippet)
  :init
  (yas-reload-all))

(use-package ws-butler
  :hook (prog-mode . ws-butler-mode))

(use-package hl-todo
  :hook (prog-mode . hl-todo-mode)
  :config
  (setq hl-todo-highlight-punctuation ":"
        hl-todo-keyword-faces
        '(("TODO"   . "#FF0000")
          ("FIXME"  . "#FF0000")
          ("DEBUG"  . "#A020F0")
          ("GOTCHA" . "#FF4500")
          ("STUB"   . "#1E90FF"))))

(use-package direnv
  :config
  (direnv-mode))

(provide 'setup-programing)
