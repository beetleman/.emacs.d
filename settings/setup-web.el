(require 'use-package)

(use-package emmet-mode
  :hook ((sgml-mode . emmet-mode)
         (rjsx-mode . emmet-mode)
         (web-mode . emmet-mode))
  :init
  (setq emmet-move-cursor-between-quotes t))

(use-package rainbow-mode
  :hook ((css-mode . rainbow-mode)
         (scss-mode . rainbow-mode)))

(provide 'setup-web)
