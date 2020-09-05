(require 'use-package)

(use-package emmet-mode
  :diminish (emmet-mode . "ε")
  :hook ((sgml-mode . emmet-mode)
         (rjsx-mode . emmet-mode)
         (web-mode . emmet-mode))
  :init
  (setq emmet-move-cursor-between-quotes t))

(use-package rainbow-mode
  :diminish rainbow-mode
  :hook ((css-mode . rainbow-mode)
         (scss-mode . rainbow-mode)))

(provide 'setup-web)
