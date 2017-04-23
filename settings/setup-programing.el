(require 'use-package)

(use-package smartparens
  :defer t
  :ensure t
  :diminish smartparens-mode
  :init
  (add-hook 'prog-mode-hook #'smartparens-mode)
  (setq sp-override-key-bindings
        '(("C-<right>" . nil)
          ("C-<left>" . nil)
          ("C-)" . sp-forward-slurp-sexp)
          ("M-<backspace>" . nil)
          ("C-(" . sp-forward-barf-sexp)))
  :config
  (use-package smartparens-config)
  (sp-use-smartparens-bindings)
  (sp--update-override-key-bindings)
  :commands (smartparens-mode show-smartparens-mode))


(use-package eldoc
  :diminish eldoc
  :init
  (add-hook 'prog-mode-hook #'eldoc-mode))

(use-package subword
  :init
  (add-hook 'prog-mode-hook #'subword-mode))


(use-package rainbow-delimiters
  :ensure t
  :defer t
  :init
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package yasnippet
  :ensure t
  :diminish yasnippet
  :commands (yas-minor-mode)
  :init
  (add-hook 'prog-mode-hook #'yas-minor-mode)
  :config
  (yas-reload-all))

(use-package whitespace
  :init
  (add-hook 'before-save-hook 'whitespace-cleanup))

(provide 'setup-programing)
