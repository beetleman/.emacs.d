(require 'use-package)

(use-package pipenv
  :ensure t
  :hook (python-mode . pipenv-mode)
  :init
  (setq
   pipenv-projectile-after-switch-function
   #'pipenv-projectile-after-switch-extended))

(use-package pyenv-mode
  :ensure t
  :config
  (add-hook 'python-mode-hook 'pyenv-mode))

(use-package py-isort
  :ensure t
  :config
  (add-hook 'before-save-hook 'py-isort-before-save))

(use-package blacken
  :ensure t
  :config
  (setf blacken-skip-string-normalization t)
  (add-hook 'python-mode-hook 'blacken-mode))

(provide 'setup-python)
