(require 'use-package)

(use-package pipenv
  :hook (python-mode . pipenv-mode)
  :init
  (setq
   pipenv-projectile-after-switch-function
   #'pipenv-projectile-after-switch-extended))

(use-package pyenv-mode
  :hook (python-mode . pyenv-mode))

(use-package py-isort
  :hook (before-save py-isort-before-save))

(use-package blacken
  :hook (python-mode . blacken-mode)
  :config
  (setf blacken-skip-string-normalization t))

(provide 'setup-python)
