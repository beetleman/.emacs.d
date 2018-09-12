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


;; not as good as anaconda-mode jet
;; (use-package lsp-python
;;   :ensure t
;;   :after (lsp-mode)
;;   :init
;;   (add-hook 'python-mode-hook #'lsp-python-enable))

(provide 'setup-python)
