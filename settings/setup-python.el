(require 'use-package)

(use-package anaconda-mode
  :ensure t
  :commands anaconda-mode
  :diminish anaconda-mode
  :init
  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-hook 'python-mode-hook 'eldoc-mode))

(use-package company-anaconda
  :ensure t
  :init (add-to-list 'company-backends 'company-anaconda))

(use-package pyenv-mode
  :config
  (pyenv-mode))

(provide 'setup-python)
