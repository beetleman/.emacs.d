(require 'use-package)

(use-package emmet-mode
  :diminish (emmet-mode . "ε")
  :ensure t
  :init
  (setq emmet-move-cursor-between-quotes t)
  :config
  (add-hook 'sgml-mode-hook 'emmet-mode)
  (add-hook 'web-mode-hook 'emmet-mode))

(provide 'setup-web)
