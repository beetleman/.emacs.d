(require 'use-package)

(use-package smartparens
  :defer t
  :ensure t
  :diminish smartparens-mode
  :init
  (use-package smartparens-config
    :init
    (smartparens-global-mode)
    (show-smartparens-global-mode)))


(use-package eldoc
  :diminish eldoc
  :init
  (add-hook 'prog-mode-hook #'eldoc-mode))

(use-package subword
  :init
  (add-hook 'prog-mode-hook #'subword-mode))

(use-package flycheck
  :ensure t
  :config
  (defun flycheck-load-config ()
    (set-face-attribute 'flycheck-warning nil
                        :underline "yellow")
    (set-face-attribute 'flycheck-error nil
                        :underline "red"))
  (add-hook 'flycheck-mode-hook 'flycheck-load-config)
  :init
  (global-flycheck-mode))

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
